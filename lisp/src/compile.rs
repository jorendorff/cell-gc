//! Compile Scheme forms into an internal representation, Code.

use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;
use env::StaticEnvironmentRef;
use errors::Result;
use value::{InternedString, Pair, Value};

pub mod op {
    pub type OpCode = u32;

    pub const RETURN: OpCode = 0; // ()
    pub const POP: OpCode = 1; // ()
    pub const CONSTANT: OpCode = 2; // (constant_index)
    pub const GET_DYNAMIC: OpCode = 3; // (symbol_index)
    pub const GET_STATIC: OpCode = 4; // (up_count, offset)
    pub const LAMBDA: OpCode = 5; // (code_index)
    pub const CALL: OpCode = 6; // (argc)
    pub const TAIL_CALL: OpCode = 7; // (argc)
    pub const JUMP_IF_FALSE: OpCode = 8; // (offset in words)
    pub const JUMP: OpCode = 9; // (offset in words)
    pub const DEFINE: OpCode = 10; // (symbol_index)
    pub const SET_STATIC: OpCode = 11; // (up_count, offset)
    pub const SET_DYNAMIC: OpCode = 12; // (symbol_index)
    pub const PUSH_ENV: OpCode = 13; // (static_env_index)
    pub const POP_ENV: OpCode = 14; // ()
    pub const SAVE: OpCode = 15; // ()
    pub const RESTORE: OpCode = 16; // ()
}

#[derive(IntoHeap)]
pub struct Code<'h> {
    pub insns: VecRef<'h, u32>,
    pub environments: VecRef<'h, StaticEnvironmentRef<'h>>,
    pub constants: VecRef<'h, Value<'h>>,
    pub rest: bool,

    /// Maximum number of values on the operand stack while running this code.
    pub operands_max: usize,
}

struct Emitter<'e, 'h: 'e> {
    hs: &'e mut GcHeapSession<'h>,
    code: Code<'h>,

    /// Number of operands that will be on the stack after executing the
    /// most-recently-emitted instruction.
    operand_depth: usize,
}

/// During compilation, this type tells what sort of continuation the current
/// expression is given.
#[derive(Copy, Clone, PartialEq, Eq)]
enum Ctn {
    /// This expression's value is going to be discarded.
    Ignore,

    /// This expression's continuation is going to do something with its value.
    Single,

    /// This expression is in tail position.
    Tail
}

const PATCH_MARK: u32 = 0x_ffff_ffff;

struct Patch(usize);

impl<'e, 'h> Emitter<'e, 'h> {
    /// When emitting toplevel code, `senv` is the environment in which the
    /// code will run (where new definitions are added). When emitting function
    /// code, it's the environment that contains the arguments.
    ///
    /// Either way, element 0 of every Code object is the static environment of
    /// that code. `vm::eval_compiled()` makes a nice sanity assertion out of
    /// this. `CodeRef::dump()` makes use of it too.
    fn new(hs: &'e mut GcHeapSession<'h>, senv: StaticEnvironmentRef<'h>, rest: bool) -> Emitter<'e, 'h> {
        let insns = hs.alloc(vec![]);
        let environments = hs.alloc(vec![senv]);
        let constants = hs.alloc(vec![]);
        Emitter {
            hs: hs,
            code: Code {
                insns,
                environments,
                constants,
                rest,
                operands_max: 0,
            },
            operand_depth: 0,
        }
    }

    fn finish(self) -> CodeRef<'h> {
        self.hs.alloc(self.code)
    }

    fn emit(&mut self, op_code: op::OpCode) {
        self.code.insns.push(op_code);
    }

    fn write_usize(&mut self, n: usize) -> Result<()> {
        if n >= PATCH_MARK as usize {
            return Err("too much code".into());
        }
        self.code.insns.push(n as u32);
        Ok(())
    }

    /// Bookkeeping, for determing how much stack we need to reserve for
    /// operands before running this code.
    ///
    /// Whenever we emit an instruction that pushes an operand to the stack, we
    /// have to call this method.
    fn push_operands(&mut self, amount: usize) {
        self.operand_depth += amount as usize;
        if self.operand_depth > self.code.operands_max {
            self.code.operands_max = self.operand_depth;
        }
    }

    /// Bookkeeping. Whenever we emit an instruction that pops operands from
    /// the stack, we call this method.
    fn pop_operands(&mut self, amount: usize) {
        assert!(self.operand_depth >= amount);
        self.operand_depth -= amount as usize;
    }

    // One method per opcode ///////////////////////////////////////////////////

    /// Emit an instruction to return the top value of the operand stack.
    fn emit_return(&mut self) {
        self.emit(op::RETURN);
        self.pop_operands(1);
    }

    /// Emit an instruction to discard the top value of the operand stack (used
    /// after each expression in a `(begin)`-expression except the last one).
    fn emit_pop(&mut self) {
        self.emit(op::POP);
        self.pop_operands(1);
    }

    /// A constant (`quote` expressions produce this, but also numbers and
    /// other self-evaluating values).
    fn emit_constant(&mut self, value: Value<'h>) -> Result<()> {
        let index = self.code.constants.len();
        self.code.constants.push(value);
        self.emit(op::CONSTANT);
        self.write_usize(index)?;
        self.push_operands(1);
        Ok(())
    }

    /// Fully general code for a variable-expression (evaluates to the
    /// variable's value).
    fn emit_get_dynamic(&mut self, id: InternedString) -> Result<()> {
        let index = self.code.constants.len();
        self.code.constants.push(Value::Symbol(GcLeaf::new(id)));
        self.emit(op::GET_DYNAMIC);
        self.write_usize(index)?;
        self.push_operands(1);
        Ok(())
    }

    /// A variable-expression, but at a known location in the environment chain.
    fn emit_get_static(&mut self, up_count: usize, index: usize) -> Result<()> {
        self.emit(op::GET_STATIC);
        self.write_usize(up_count)?;
        self.write_usize(index)?;
        self.push_operands(1);
        Ok(())
    }

    /// A lambda expression.
    fn emit_lambda(&mut self, code: CodeRef<'h>) -> Result<()> {
        let index = self.code.constants.len();
        self.code.constants.push(Value::Code(code));
        self.emit(op::LAMBDA);
        self.write_usize(index)?;
        self.push_operands(1);
        Ok(())
    }

    /// Emit the instruction to call a function.  This should come after
    /// emitting the expression for the function and its operands.
    fn emit_call(&mut self, argc: usize, k: Ctn) -> Result<()> {
        self.emit(if k == Ctn::Tail {
            op::TAIL_CALL
        } else {
            op::CALL
        });
        self.write_usize(argc)?;
        self.pop_operands(1 + argc);
        if k != Ctn::Tail {
            self.push_operands(1);
            self.emit_ctn(k);
        }
        Ok(())
    }

    /// Emit the branching instruction for an `if` expression.
    fn emit_jump_if_false(&mut self) -> Patch {
        self.emit(op::JUMP_IF_FALSE);
        let patch = Patch(self.code.insns.len());
        self.code.insns.push(PATCH_MARK);
        self.pop_operands(1);
        patch
    }

    /// This instruction is used at the end of the "then" part of a `if`
    /// expression, before the "else" part.
    fn emit_jump(&mut self) -> Patch {
        self.emit(op::JUMP);
        let patch = Patch(self.code.insns.len());
        self.code.insns.push(PATCH_MARK);
        patch
    }

    /// Patch a previous jump instruction so that it jumps to the present point
    /// in the code.
    fn patch_jump_to_here(&mut self, patch: Patch) -> Result<()> {
        let there = patch.0;
        assert_eq!(self.code.insns.get(there), PATCH_MARK);
        let here = self.code.insns.len();
        let jump = here - there;
        if jump >= PATCH_MARK as usize {
            return Err("too much code".into());
        }
        self.code.insns.set(patch.0, jump as u32);
        Ok(())
    }

    /// A definition at toplevel. (Local definitions are rewritten into
    /// `letrec*` forms.)
    fn emit_define(&mut self, id: InternedString) -> Result<()> {
        let index = self.code.constants.len();
        self.code.constants.push(Value::Symbol(GcLeaf::new(id)));
        self.emit(op::DEFINE);
        self.write_usize(index)?;
        self.pop_operands(1);
        Ok(())
    }

    /// Assign to a static binding. `set!` expressions could emit this but we
    /// currently don't bother; we use it for `letrec*`.
    fn emit_set_static(&mut self, up_count: usize, index: usize) -> Result<()> {
        self.emit(op::SET_STATIC);
        self.write_usize(up_count)?;
        self.write_usize(index)?;
        self.pop_operands(1);
        Ok(())
    }

    /// An assignment expression (`set!`).
    fn emit_set_dynamic(&mut self, id: InternedString) -> Result<()> {
        let index = self.code.constants.len();
        self.code.constants.push(Value::Symbol(GcLeaf::new(id)));
        self.emit(op::SET_DYNAMIC);
        self.write_usize(index)?;
        self.pop_operands(1);
        Ok(())
    }

    /// Emit code to create and push a new environment.
    /// This is used to implement `letrec` and `letrec*`.
    fn emit_push_env(&mut self, senv: StaticEnvironmentRef<'h>) -> Result<()> {
        let index = self.code.environments.len();
        self.code.environments.push(senv);
        self.emit(op::PUSH_ENV);
        self.write_usize(index)
    }

    fn emit_pop_env(&mut self) {
        self.emit(op::POP_ENV);
    }

    /// This is called immediately after emitting some code to compute a value.
    /// It's used to dispose of that value properly.
    ///
    /// Most of the time, the continuation is Single: subsequent instructions
    /// expect a single value, and they'll look for it on the operand stack,
    /// which is exactly where previous instructions put it. So in that case we
    /// don't have to do anything here.
    fn emit_ctn(&mut self, k: Ctn) {
        match k {
            Ctn::Ignore => self.emit_pop(), // discard the value
            Ctn::Single => {} // do nothing, leave the value on the operand stack
            Ctn::Tail => self.emit_return(), // non-call expression in tail position
        }
    }

    /// Convenience method for producing the unspecified value.
    fn emit_unspecified(&mut self, k: Ctn) -> Result<()> {
        if k != Ctn::Ignore {
            self.emit_constant(Value::Unspecified)?;
            self.emit_ctn(k);
        }
        Ok(())
    }

    fn compile_begin<F>(
        &mut self,
        mut f: F,
        senv: &StaticEnvironmentRef<'h>,
        mut expr_list: Value<'h>,
        k: Ctn,
    ) -> Result<()>
        where
        F: FnMut(&mut Self, &StaticEnvironmentRef<'h>, Value<'h>, Ctn) -> Result<()>
    {
        let mut current = match expr_list.next() {
            None => return Err("expression required".into()),
            Some(first) => first?,
        };
        for next in expr_list {
            f(self, senv, current, Ctn::Ignore)?;
            current = next?;
        }
        f(self, senv, current, k)
    }

    /// After parsing a letrec, emit the actual code for it.
    ///
    /// R6RS states that (letrec) and (letrec*) expressions "should" or "must"
    /// not use variables that aren't bound yet, and says "the continuation of
    /// each <init> should not be invoked more than once." We do not detect
    /// either error.
    fn compile_parsed_letrec(
        &mut self,
        senv: &StaticEnvironmentRef<'h>,
        letrec_star: bool,
        exprs: Vec<Value<'h>>,
        body: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        assert!(!exprs.is_empty());
        assert_eq!(senv.names().len(), exprs.len());

        self.emit_push_env(senv.clone())?;
        if letrec_star {
            // (letrec*) initialization expressions are evaluated and bound in order.
            for (i, expr) in exprs.into_iter().enumerate() {
                self.compile_expr(senv, expr, Ctn::Single)?;
                self.emit_set_static(0, i)?;
            }
        } else {
            // (letrec) initialization expressions are all evaluated before binding.
            let n = exprs.len();
            for expr in exprs {
                self.compile_expr(senv, expr, Ctn::Single)?;
            }
            for i in (0..n).rev() {
                self.emit_set_static(0, i)?;
            }
        }
        self.compile_body(senv, body, k)?;
        if k != Ctn::Tail {
            self.emit_pop_env();
        }
        Ok(())
    }

    // Compile the body of a lambda, letrec, or letrec*.
    fn compile_body(
        &mut self,
        senv: &StaticEnvironmentRef<'h>,
        mut body: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        // Parse leading definitions.
        let mut names = vec![];
        let mut init_exprs = vec![];
        loop {
            match body {
                Value::Cons(pair) => {
                    let form = pair.car();
                    if is_definition(&form) {
                        let (name, expr) = parse_define(self.hs, form)?;
                        names.push(name);
                        init_exprs.push(expr);
                        body = pair.cdr();
                    } else {
                        body = Value::Cons(pair);
                        break;
                    }
                }
                Value::Nil => return Err("expression required".into()),
                _ => return Err("improper list".into()),
            }
        }

        // If there are no definitions, this is just like a begin-expression.
        if names.is_empty() {
            return self.compile_begin(Self::compile_expr, senv, body, k);
        }

        // Translate this body into a `letrec*`.
        let body_senv = senv.new_nested_environment(self.hs, names);
        self.compile_parsed_letrec(&body_senv, true, init_exprs, body, k)
    }

    fn compile_name(
        &mut self,
        senv: &StaticEnvironmentRef<'h>,
        s: InternedString,
        k: Ctn,
    ) -> Result<()> {
        match senv.lookup(&s) {
            Some((up_count, index)) => self.emit_get_static(up_count, index)?,
            _ => self.emit_get_dynamic(s)?,
        }
        self.emit_ctn(k);
        Ok(())
    }

    fn compile_lambda(
        &mut self,
        senv: &StaticEnvironmentRef<'h>,
        tail: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        let (mut param_list, body_forms) = tail.as_pair("syntax error in lambda")?;

        let mut names = vec![];
        while let Value::Cons(pair) = param_list {
            if let Value::Symbol(s) = pair.car() {
                names.push(s);
            } else {
                return Err("syntax error in lambda arguments".into());
            }
            param_list = pair.cdr();
        }
        let rest = match param_list {
            Value::Nil => false,
            Value::Symbol(rest_name) => {
                names.push(rest_name);
                true
            }
            _ => return Err("syntax error in lambda arguments".into()),
        };

        let lambda_senv = senv.new_nested_environment(self.hs, names);
        let code = {
            let mut lambda_emitter = Emitter::new(self.hs, lambda_senv.clone(), rest);
            lambda_emitter.compile_body(&lambda_senv, body_forms, Ctn::Tail)?;
            lambda_emitter.finish()
        };
        self.emit_lambda(code)?;
        self.emit_ctn(k);
        Ok(())
    }

    fn compile_quote(
        &mut self,
        tail: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        let (datum, rest) = tail.as_pair("(quote) with no arguments")?;
        if !rest.is_nil() {
            return Err("too many arguments to (quote)".into());
        }
        self.emit_constant(datum)?;
        self.emit_ctn(k);
        Ok(())
    }

    fn compile_if(
        &mut self,
        senv: &StaticEnvironmentRef<'h>,
        tail: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        let (cond, rest) = tail.as_pair("(if) with no arguments")?;
        self.compile_expr(senv, cond, Ctn::Single)?;
        let jif_patch = self.emit_jump_if_false();
        let depth_after_branch = self.operand_depth;

        let (then_expr, rest) = rest.as_pair("missing arguments after (if COND)")?;
        self.compile_expr(senv, then_expr, k)?;
        let depth_on_join = self.operand_depth;
        let j_patch = if k == Ctn::Tail {
            // We already emitted a Return or TailCall. Don't
            // bother emitting an unreachable jump.
            None
        } else if k == Ctn::Ignore && rest.is_nil() {
            // There is no "else" code, so there's no need to
            // jump over it.
            None
        } else {
            Some(self.emit_jump())
        };
        self.patch_jump_to_here(jif_patch)?;

        self.operand_depth = depth_after_branch;
        if rest.is_nil() {
            self.emit_unspecified(k)?;
        } else {
            let (else_expr, rest) = rest.as_pair("improper (if) expression")?;
            if !rest.is_nil() {
                return Err("too many arguments in (if) expression".into());
            }
            self.compile_expr(senv, else_expr, k)?
        };
        if let Some(patch) = j_patch {
            assert_eq!(self.operand_depth, depth_on_join);
            self.patch_jump_to_here(patch)?;
        }
        Ok(())
    }

    fn compile_letrec(
        &mut self,
        senv: &StaticEnvironmentRef<'h>,
        letrec_star: bool,
        tail: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        let (bindings, body_forms) = tail.as_pair("letrec: bindings required")?;
        let mut names = vec![];
        let mut init_exprs = vec![];
        for binding_result in bindings {
            let binding = binding_result?;
            let (name_v, rest) = binding.as_pair("letrec: invalid binding")?;
            let name = name_v.as_symbol("letrec: name required")?;
            let (expr, rest) = rest.as_pair("letrec: value required for binding")?;
            if !rest.is_nil() {
                return Err("letrec: too many elements in binding".into());
            }
            names.push(GcLeaf::new(name));
            init_exprs.push(expr);
        }
        if names.is_empty() {
            return self.compile_body(senv, body_forms, k);
        }

        let body_senv = senv.new_nested_environment(self.hs, names);
        return self.compile_parsed_letrec(&body_senv, letrec_star, init_exprs, body_forms, k);
    }

    fn compile_set(
        &mut self,
        senv: &StaticEnvironmentRef<'h>,
        tail: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        let (first, rest) = tail.as_pair("(set!) with no name")?;
        let name = first.as_symbol("(set!) first argument must be a name")?;
        let (expr, rest) = rest.as_pair("(set!) with no value")?;
        if !rest.is_nil() {
            return Err("(set!): too many arguments".into());
        }

        self.compile_expr(senv, expr, Ctn::Single)?;
        match senv.lookup(&name) {
            Some((up_count, index)) => self.emit_set_static(up_count, index)?,
            _ => self.emit_set_dynamic(name)?,
        };
        return self.emit_unspecified(k);
    }

    fn compile_call(
        &mut self,
        senv: &StaticEnvironmentRef<'h>,
        expr: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        let mut elements = 0;
        for v in expr {
            self.compile_expr(senv, v?, Ctn::Single)?;
            elements += 1; // adds 1 for the callee, not just arguments
        }
        self.emit_call(elements - 1, k) // subtract 1 to compensate for that
    }

    fn compile_expr(
        &mut self,
        senv: &StaticEnvironmentRef<'h>,
        expr: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        match expr {
            Value::Symbol(s) => self.compile_name(senv, s.unwrap(), k),

            Value::Cons(p) => {
                if let Value::Symbol(ref s) = p.car() {
                    match s.as_str() {
                        "lambda" =>
                            self.compile_lambda(senv, p.cdr(), k),
                        "quote" =>
                            self.compile_quote(p.cdr(), k),
                        "if" =>
                            self.compile_if(senv, p.cdr(), k),
                        "begin" =>
                            // In expression context, this is sequencing, not splicing.
                            self.compile_begin(Self::compile_expr, senv, p.cdr(), k),
                        "letrec" =>
                            self.compile_letrec(senv, false, p.cdr(), k),
                        "letrec*" =>
                            self.compile_letrec(senv, true, p.cdr(), k),
                        "set!" =>
                            self.compile_set(senv, p.cdr(), k),
                        "define" =>
                            // In expression context, definitions aren't allowed.
                            Err(
                                "(define) is allowed only at toplevel or in the body \
                                 of a function or let-form"
                                    .into(),
                            ),
                        _ =>
                            self.compile_call(senv, Value::Cons(p), k),
                    }
                } else {
                    self.compile_call(senv, Value::Cons(p), k)
                }
            }

            // Self-evaluating values.
            //
            // Note: Not sure what R6RS says about "three-dimensional" code,
            // eval code containing "constants" (either quoted or
            // self-evaluating) that are passed through. Possibly both this and
            // the (quote) case should do more work, to "flatten" the
            // constants. Here, only StringObj is suspect.
            v @ Value::Bool(_) |
            v @ Value::Int(_) |
            v @ Value::Char(_) |
            v @ Value::ImmString(_) |
            v @ Value::StringObj(_) => {
                self.emit_constant(v)?;
                self.emit_ctn(k);
                Ok(())
            }

            // Everything else is an error.
            _ => Err("not an expression".into()),
        }
    }

    fn compile_toplevel(
        &mut self,
        senv: &StaticEnvironmentRef<'h>,
        expr: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        if let Some(children) = begin_form_children(&expr) {
            self.compile_begin(Self::compile_toplevel, senv, children, k)
        } else if is_definition(&expr) {
            let (name, init_expr) = parse_define(self.hs, expr)?;
            self.compile_expr(senv, init_expr, Ctn::Single)?;
            self.emit_define(name.unwrap())?;
            self.emit_unspecified(k)
        } else {
            self.compile_expr(senv, expr, k)
        }
    }
}

/// If `form` is a begin form, return the list of its children, else None.
fn begin_form_children<'h>(form: &Value<'h>) -> Option<Value<'h>> {
    if let Value::Cons(ref pair) = *form {
        if let Value::Symbol(op) = pair.car() {
            if op.as_str() == "begin" {
                return Some(pair.cdr());
            }
        }
    }
    None
}

fn is_definition<'h>(form: &Value<'h>) -> bool {
    if let Value::Cons(ref pair) = *form {
        if let Value::Symbol(op) = pair.car() {
            if op.as_str() == "define" {
                return true;
            }
        }
    }
    false
}

/// On success, returns the two parts of a `(define)` that we care about: the
/// name to define and the expression to populate it.
fn parse_define<'h>(
    hs: &mut GcHeapSession<'h>,
    mut defn: Value<'h>,
) -> Result<(GcLeaf<InternedString>, Value<'h>)> {
    loop {
        let (define_symbol, tail) = defn.as_pair("internal error")?;
        let (pattern, rest) = tail.as_pair("(define) with no name")?;
        match pattern {
            Value::Symbol(ident) => {
                let (expr, rest) = rest.as_pair("(define) with no value")?;
                if !rest.is_nil() {
                    return Err("too many arguments in (define)".into());
                }
                return Ok((ident, expr));
            }
            Value::Cons(pair) => {
                // Build desugared definition and compile that.
                let name = pair.car();
                let formals = pair.cdr();

                // Transform `(define (,name ,@formals) ,@rest)
                // to        `(define ,name (lambda ,formals ,@rest))
                let lambda_cdr = Value::Cons(hs.alloc(Pair {
                    car: formals,
                    cdr: rest,
                }));
                let lambda = Value::Cons(hs.alloc(Pair {
                    car: Value::Symbol(GcLeaf::new(InternedString::get("lambda"))),
                    cdr: lambda_cdr,
                }));
                let defn_cddr = Value::Cons(hs.alloc(Pair {
                    car: lambda,
                    cdr: Value::Nil,
                }));
                let defn_cdr = Value::Cons(hs.alloc(Pair {
                    car: name,
                    cdr: defn_cddr,
                }));
                defn = Value::Cons(hs.alloc(Pair {
                    car: define_symbol,
                    cdr: defn_cdr,
                }));
            }
            _ => return Err("(define) with a non-symbol name".into()),
        }
    }
}

impl<'h> CodeRef<'h> {
    pub fn dump(&self) {
        let constants = self.constants();
        let environments = self.environments();
        let mut senv = environments.get(0);
        let insns = self.insns();

        let mut pc = 0;
        while pc < insns.len() {
            print!("{:7} ", pc);
            let op = insns.get(pc);
            pc += 1;
            match op {
                op::RETURN => {
                    println!("return")
                }
                op::POP => {
                    println!("pop")
                }
                op::CONSTANT => {
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("constant {}  ;; {}", i, constants.get(i));
                }
                op::GET_DYNAMIC => {
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("get_dynamic {}  ;; {}", i, constants.get(i));
                }
                op::GET_STATIC => {
                    let up_count = insns.get(pc) as usize;
                    pc += 1;
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("get_static {} {}  ;; {}", up_count, i, senv.get_name(up_count, i).as_str());
                }
                op::LAMBDA => {
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("lambda {}", i);
                }
                op::CALL => {
                    let argc = insns.get(pc) as usize;
                    pc += 1;
                    println!("call {}", argc);
                }
                op::TAIL_CALL => {
                    let argc = insns.get(pc) as usize;
                    pc += 1;
                    println!("tail_call {}", argc);
                }
                op::JUMP_IF_FALSE => {
                    let distance = insns.get(pc) as usize;
                    println!("jump_if_false +{}  ;; to {}", distance, pc + distance);
                    pc += 1;
                }
                op::JUMP => {
                    let distance = insns.get(pc) as usize;
                    println!("jump +{}  ;; to {}", distance, pc + distance);
                    pc += 1;
                }
                op::DEFINE => {
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("define {}  ;; {}", i, constants.get(i));
                }
                op::SET_STATIC => {
                    let up_count = insns.get(pc) as usize;
                    pc += 1;
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("set_static {} {}  ;; {}", up_count, i, senv.get_name(up_count, i).as_str());
                }
                op::SET_DYNAMIC => {
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("set_dynamic {}  ;; {}", i, constants.get(i));
                }
                op::PUSH_ENV => {
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    let new_senv = environments.get(i);
                    assert_eq!(new_senv.parent(), Some(senv));
                    senv = new_senv;
                    let names = senv.names().into_iter()
                        .map(|v| v.as_str().to_string())
                        .collect::<Vec<_>>();
                    println!("push_env {}  ;; ({})", i, names.join(" "));
                }
                op::POP_ENV => {
                    senv = senv.parent().unwrap();
                    println!("pop_env");
                }
                _ => {
                    println!("invalid opcode {}", op);
                }
            }
        }
        println!();
    }
}

pub fn compile_toplevel<'h>(
    hs: &mut GcHeapSession<'h>,
    senv: &StaticEnvironmentRef<'h>,
    expr: Value<'h>,
) -> Result<CodeRef<'h>> {
    let mut emitter = Emitter::new(hs, senv.clone(), false);
    emitter.compile_toplevel(senv, expr, Ctn::Tail)?;
    Ok(emitter.finish())
}

pub fn compile_toplevel_forms<'h>(
    hs: &mut GcHeapSession<'h>,
    senv: &StaticEnvironmentRef<'h>,
    mut forms: Vec<Value<'h>>,
) -> Result<CodeRef<'h>> {
    let mut emitter = Emitter::new(hs, senv.clone(), false);
    let last = forms.pop();
    for form in forms {
        emitter.compile_toplevel(senv, form, Ctn::Ignore)?;
    }
    if let Some(form) = last {
        emitter.compile_toplevel(senv, form, Ctn::Tail)?;
    } else {
        emitter.emit_unspecified(Ctn::Tail)?;
    }
    Ok(emitter.finish())
}

fn expr_is_infallible<'h>(
    senv: &StaticEnvironmentRef<'h>,
    expr: &Value<'h>,
) -> Result<bool> {
    match expr {
        &Value::Symbol(ref s) => {
            // This is incorrect for certain (letrec) bindings.
            Ok(senv.lookup(&s).is_some())
        }

        &Value::Cons(ref p) => match p.car() {
            Value::Symbol(ref s) => {
                let s = s.as_str();
                if s == "lambda" || s == "quote" {
                    Ok(true)
                } else if s == "if" || s == "begin" {
                    for subexpr in p.cdr() {
                        if !expr_is_infallible(senv, &subexpr?)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else if s == "set!" {
                    let (first, rest) = p.cdr().as_pair("")?;
                    let name = first.as_symbol("")?;
                    let (subexpr, rest) = rest.as_pair("")?;
                    if !rest.is_nil() {
                        return Err("".into());
                    }

                    // This is incorrect for certain (letrec) bindings.
                    let infallible_binding = senv.lookup(&name).is_some();
                    Ok(infallible_binding &&
                       expr_is_infallible(senv, &subexpr)?)
                } else {
                    Ok(false)
                }
            }
            _ => Ok(false),
        },

        // Self-evaluating values.
        &Value::Bool(_) |
        &Value::Int(_) |
        &Value::Char(_) |
        &Value::ImmString(_) |
        &Value::StringObj(_) => Ok(true),

        _ => Ok(false)
    }
}

pub fn toplevel_form_is_infallible<'h>(
    hs: &mut GcHeapSession<'h>,
    senv: &StaticEnvironmentRef<'h>,
    form: &Value<'h>
) -> bool {
    if is_definition(form) {
        let (_, init_expr) = match parse_define(hs, form.clone()) {
            Err(_) => return false,
            Ok(parts) => parts,
        };
        expr_is_infallible(senv, &init_expr).unwrap_or(false)
    } else {
        expr_is_infallible(senv, form).unwrap_or(false)
    }
}

pub fn toplevel_form_defined_name<'h>(
    hs: &mut GcHeapSession<'h>,
    form: &Value<'h>,
) -> Option<InternedString> {
    if is_definition(form) {
        match parse_define(hs, form.clone()) {
            Err(_) => None,
            Ok((name, _)) => Some(name.unwrap())
        }
    } else {
        None
    }
}
