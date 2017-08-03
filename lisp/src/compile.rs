//! Compile Scheme forms into an internal representation, Expr.

use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;
use env::StaticEnvironmentRef;
use errors::Result;
use std::mem;
use value::{InternedString, Pair, Value};

#[repr(u32)]
pub enum Op {
    Return, // ()
    Pop, // ()
    Constant, // (constant_index)
    GetDynamic, // (symbol_index)
    GetStatic, // (up_count, offset)
    Lambda, // (code_index)
    Call, // (argc)
    TailCall, // (argc)
    JumpIfFalse, // (offset in words)
    Jump, // (offset in words)
    Define, // (symbol_index)
    SetStatic, // (up_count, offset)
    SetDynamic, // (symbol_index)
    PushEnv, // (static_env_index)
    PopEnv, // ()
}

impl Op {
    pub fn from_u32(n: u32) -> Op {
        assert!(n <= Op::PopEnv as u32);
        unsafe { mem::transmute(n) }
    }
}

#[derive(IntoHeap)]
pub struct Code<'h> {
    pub insns: VecRef<'h, u32>,
    pub environments: VecRef<'h, StaticEnvironmentRef<'h>>,
    pub constants: VecRef<'h, Value<'h>>,
    pub rest: bool,
}

struct Emitter<'h> {
    code: Code<'h>
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

impl<'h> Emitter<'h> {
    /// When emitting toplevel code, `senv` is the environment in which the
    /// code will run (where new definitions are added). When emitting function
    /// code, it's the environment that contains the arguments.
    ///
    /// Either way, element 0 of every Code object is the static environment of
    /// that code. `vm::eval_compiled()` makes a nice sanity assertion out of
    /// this. `CodeRef::dump()` makes use of it too.
    fn new(hs: &mut GcHeapSession<'h>, senv: StaticEnvironmentRef<'h>, rest: bool) -> Emitter<'h> {
        let insns = hs.alloc(vec![]);
        let environments = hs.alloc(vec![senv]);
        let constants = hs.alloc(vec![]);
        Emitter {
            code: Code {
                insns,
                environments,
                constants,
                rest,
            }
        }
    }

    fn finish(self, hs: &mut GcHeapSession<'h>) -> CodeRef<'h> {
        hs.alloc(self.code)
    }

    fn emit(&mut self, op: Op) {
        self.code.insns.push(op as u32);
    }

    fn write_usize(&mut self, n: usize) -> Result<()> {
        if n >= PATCH_MARK as usize {
            return Err("too much code".into());
        }
        self.code.insns.push(n as u32);
        Ok(())
    }

    /// A constant (`quote` expressions produce this, but also numbers and
    /// other self-evaluating values).
    fn emit_constant(&mut self, value: Value<'h>) -> Result<()> {
        let index = self.code.constants.len();
        self.code.constants.push(value);
        self.emit(Op::Constant);
        self.write_usize(index)
    }

    /// Fully general code for a variable-expression (evaluates to the
    /// variable's value).
    fn emit_get_dynamic(&mut self, id: InternedString) -> Result<()> {
        let index = self.code.constants.len();
        self.code.constants.push(Value::Symbol(GcLeaf::new(id)));
        self.emit(Op::GetDynamic);
        self.write_usize(index)
    }

    /// A variable-expression, but at a known location in the environment chain.
    fn emit_get_static(&mut self, up_count: usize, index: usize) -> Result<()> {
        self.emit(Op::GetStatic);
        self.write_usize(up_count)?;
        self.write_usize(index)
    }

    /// A lambda expression.
    fn emit_lambda(&mut self, code: CodeRef<'h>) -> Result<()> {
        let index = self.code.constants.len();
        self.code.constants.push(Value::Code(code));
        self.emit(Op::Lambda);
        self.write_usize(index)
    }

    /// Emit the instruction to call a function.  This should come after
    /// emitting the expression for the function and its operands.
    fn emit_call(&mut self, argc: usize, k: Ctn) -> Result<()> {
        self.emit(if k == Ctn::Tail {
            Op::TailCall
        } else {
            Op::Call
        });
        self.write_usize(argc)?;
        if k != Ctn::Tail {
            self.emit_ctn(k);
        }
        Ok(())
    }

    /// Emit the branching instruction for an `if` expression.
    fn emit_jump_if_false(&mut self) -> Patch {
        self.emit(Op::JumpIfFalse);
        let patch = Patch(self.code.insns.len());
        self.code.insns.push(PATCH_MARK);
        patch
    }

    /// This instruction is used at the end of the "then" part of a `if`
    /// expression, before the "else" part.
    fn emit_jump(&mut self) -> Patch {
        self.emit(Op::Jump);
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
        self.emit(Op::Define);
        self.write_usize(index)
    }

    /// Assign to a static binding. `set!` expressions could emit this but we
    /// currently don't bother; we use it for `letrec*`.
    fn emit_set_static(&mut self, up_count: usize, index: usize) -> Result<()> {
        self.emit(Op::SetStatic);
        self.write_usize(up_count)?;
        self.write_usize(index)
    }

    /// An assignment expression (`set!`).
    fn emit_set_dynamic(&mut self, id: InternedString) -> Result<()> {
        let index = self.code.constants.len();
        self.code.constants.push(Value::Symbol(GcLeaf::new(id)));
        self.emit(Op::SetDynamic);
        self.write_usize(index)
    }

    /// Emit code to create and push a new environment.
    /// This is used to implement `letrec` and `letrec*`.
    fn emit_push_env(&mut self, senv: StaticEnvironmentRef<'h>) -> Result<()> {
        let index = self.code.environments.len();
        self.code.environments.push(senv);
        self.emit(Op::PushEnv);
        self.write_usize(index)
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
            Ctn::Ignore => self.emit(Op::Pop), // discard the value
            Ctn::Single => {} // do nothing, leave the value on the operand stack
            Ctn::Tail => self.emit(Op::Return), // non-call expression in tail position
        }
    }

    fn emit_unspecified(&mut self, k: Ctn) -> Result<()> {
        if k != Ctn::Ignore {
            self.emit_constant(Value::Unspecified)?;
            self.emit_ctn(k);
        }
        Ok(())
    }

    fn compile_seq(
        &mut self,
        hs: &mut GcHeapSession<'h>,
        senv: &StaticEnvironmentRef<'h>,
        mut expr_list: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        let mut current = match expr_list.next() {
            None => return Err("expression required".into()),
            Some(first) => first?,
        };
        for next in expr_list {
            self.compile_expr(hs, senv, current, Ctn::Ignore)?;
            current = next?;
        }
        self.compile_expr(hs, senv, current, k)
    }

    fn compile_letrec(
        &mut self,
        hs: &mut GcHeapSession<'h>,
        senv: &StaticEnvironmentRef<'h>,
        exprs: Vec<Value<'h>>,
        body: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        assert!(!exprs.is_empty());
        assert_eq!(senv.names().len(), exprs.len());

        self.emit_push_env(senv.clone())?;
        for (i, expr) in exprs.into_iter().enumerate() {
            self.compile_expr(hs, senv, expr, Ctn::Single)?;
            self.emit_set_static(0, i)?;
        }
        self.compile_body(hs, senv, body, k)?;
        if k != Ctn::Tail {
            self.emit(Op::PopEnv);
        }
        Ok(())
    }

    // Compile the body of a lambda or letrec*.
    fn compile_body(
        &mut self,
        hs: &mut GcHeapSession<'h>,
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
                        let (name, expr) = parse_define(hs, form)?;
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

        // If there are no definitions, this is easy.
        if names.is_empty() {
            return self.compile_seq(hs, senv, body, k);
        }

        // Translate this body into a `letrec*`.
        let body_senv = senv.new_nested_environment(hs, names);
        self.compile_letrec(hs, &body_senv, init_exprs, body, k)
    }

    pub fn compile_expr(
        &mut self,
        hs: &mut GcHeapSession<'h>,
        senv: &StaticEnvironmentRef<'h>,
        expr: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        match expr {
            Value::Symbol(s) => {
                match senv.lookup(&s) {
                    Some((up_count, index)) => self.emit_get_static(up_count, index)?,
                    _ => self.emit_get_dynamic(s.unwrap())?,
                }
                self.emit_ctn(k);
                Ok(())
            }

            Value::Cons(p) => {
                let f = p.car();
                if let Value::Symbol(ref s) = f {
                    if s.as_str() == "lambda" {
                        let (mut param_list, body_forms) = p.cdr().as_pair("syntax error in lambda")?;

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

                        let lambda_senv = senv.new_nested_environment(hs, names);
                        let mut lambda_emitter = Emitter::new(hs, lambda_senv.clone(), rest);
                        lambda_emitter.compile_body(hs, &lambda_senv, body_forms, Ctn::Tail)?;
                        self.emit_lambda(lambda_emitter.finish(hs))?;
                        self.emit_ctn(k);
                        return Ok(());
                    } else if s.as_str() == "quote" {
                        let (datum, rest) = p.cdr().as_pair("(quote) with no arguments")?;
                        if !rest.is_nil() {
                            return Err("too many arguments to (quote)".into());
                        }
                        self.emit_constant(datum)?;
                        self.emit_ctn(k);
                        return Ok(());
                    } else if s.as_str() == "if" {
                        let (cond, rest) = p.cdr().as_pair("(if) with no arguments")?;
                        self.compile_expr(hs, senv, cond, Ctn::Single)?;
                        let jif_patch = self.emit_jump_if_false();

                        let (then_expr, rest) = rest.as_pair("missing arguments after (if COND)")?;
                        self.compile_expr(hs, senv, then_expr, k)?;
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

                        if rest.is_nil() {
                            self.emit_unspecified(k)?;
                        } else {
                            let (else_expr, rest) = rest.as_pair("improper (if) expression")?;
                            if !rest.is_nil() {
                                return Err("too many arguments in (if) expression".into());
                            }
                            self.compile_expr(hs, senv, else_expr, k)?
                        };
                        if let Some(patch) = j_patch {
                            self.patch_jump_to_here(patch)?;
                        }
                        return Ok(());
                    } else if s.as_str() == "begin" {
                        // In expression context, this is sequencing, not splicing.
                        return self.compile_seq(hs, senv, p.cdr(), k);
                    } else if s.as_str() == "letrec" || s.as_str() == "letrec*" {
                        // Treat (letrec) forms just like (letrec*). Nonstandard in
                        // R6RS, which requires implementations to detect invalid
                        // references to letrec bindings before they're bound. But
                        // R5RS does not require this, and anyway well-behaved
                        // programs won't care.
                        let (bindings, body_forms) = p.cdr().as_pair("letrec*: bindings required")?;
                        let mut names = vec![];
                        let mut init_exprs = vec![];
                        for binding_result in bindings {
                            let binding = binding_result?;
                            let (name_v, rest) = binding.as_pair("letrec*: invalid binding")?;
                            let name = name_v.as_symbol("letrec*: name required")?;
                            let (expr, rest) = rest.as_pair("letrec*: value required for binding")?;
                            if !rest.is_nil() {
                                return Err("(letrec*): too many arguments".into());
                            }
                            names.push(GcLeaf::new(name));
                            init_exprs.push(expr);
                        }
                        if names.is_empty() {
                            return self.compile_body(hs, senv, body_forms, k);
                        }

                        let body_senv = senv.new_nested_environment(hs, names);
                        return self.compile_letrec(hs, &body_senv, init_exprs, body_forms, k);
                    } else if s.as_str() == "set!" {
                        let (first, rest) = p.cdr().as_pair("(set!) with no name")?;
                        let name = first.as_symbol("(set!) first argument must be a name")?;
                        let (expr, rest) = rest.as_pair("(set!) with no value")?;
                        if !rest.is_nil() {
                            return Err("(set!): too many arguments".into());
                        }

                        self.compile_expr(hs, senv, expr, Ctn::Single)?;
                        match senv.lookup(&name) {
                            Some((up_count, index)) => self.emit_set_static(up_count, index)?,
                            _ => self.emit_set_dynamic(name)?,
                        };
                        return self.emit_unspecified(k);
                    } else if s.as_str() == "define" {
                        // In expression context, definitions aren't allowed.
                        return Err(
                            "(define) is allowed only at toplevel or in the body \
                             of a function or let-form"
                                .into(),
                        );
                    }
                }

                let mut argc = 0;
                for v in Value::Cons(p) {
                    self.compile_expr(hs, senv, v?, Ctn::Single)?;
                    argc += 1; // erroneously adds 1 for the callee, not just arguments
                }
                self.emit_call(argc - 1, k) // subtract 1 to compensate for that
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
        hs: &mut GcHeapSession<'h>,
        senv: &StaticEnvironmentRef<'h>,
        expr: Value<'h>,
        k: Ctn,
    ) -> Result<()> {
        // TODO: support (begin) here
        if is_definition(&expr) {
            let (name, init_expr) = parse_define(hs, expr)?;
            self.compile_expr(hs, senv, init_expr, Ctn::Single)?;
            self.emit_define(name.unwrap())?;
            self.emit_unspecified(k)
        } else {
            self.compile_expr(hs, senv, expr, k)
        }
    }
}

// Convert the linked list of a `<body>` to a vector; also splice in the
// contents of `(begin)` expressions nested within the `<body>`.
//
// Bug: Both (begin defn ...) and (begin expr ...) forms flatten, but this
// also permits (begin defn ... expr ...) cases that should be errors.
fn flatten_body<'h>(forms: Value<'h>, out: &mut Vec<Value<'h>>) -> Result<()> {
    for form_res in forms {
        let form = form_res?;
        if let Value::Cons(ref pair) = form {
            if let Value::Symbol(op) = pair.car() {
                if op.as_str() == "begin" {
                    flatten_body(pair.cdr(), out)?;
                    continue;
                }
            }
        }
        out.push(form);
    }
    Ok(())
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
            let op = Op::from_u32(insns.get(pc));
            pc += 1;
            match op {
                Op::Return => {
                    println!("return")
                }
                Op::Pop => {
                    println!("pop")
                }
                Op::Constant => {
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("constant {}  ;; {}", i, constants.get(i));
                }
                Op::GetDynamic => {
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("get_dynamic {}  ;; {}", i, constants.get(i));
                }
                Op::GetStatic => {
                    let up_count = insns.get(pc) as usize;
                    pc += 1;
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("get_static {} {}  ;; {}", up_count, i, senv.get_name(up_count, i).as_str());
                }
                Op::Lambda => {
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("lambda {}", i);
                }
                Op::Call => {
                    let argc = insns.get(pc) as usize;
                    pc += 1;
                    println!("call {}", argc);
                }
                Op::TailCall => {
                    let argc = insns.get(pc) as usize;
                    pc += 1;
                    println!("tail_call {}", argc);
                }
                Op::JumpIfFalse => {
                    let distance = insns.get(pc) as usize;
                    println!("jump_if_false +{}  ;; to {}", distance, pc + distance);
                    pc += 1;
                }
                Op::Jump => {
                    let distance = insns.get(pc) as usize;
                    println!("jump +{}  ;; to {}", distance, pc + distance);
                    pc += 1;
                }
                Op::Define => {
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("define {}  ;; {}", i, constants.get(i));
                }
                Op::SetStatic => {
                    let up_count = insns.get(pc) as usize;
                    pc += 1;
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("set_static {} {}  ;; {}", up_count, i, senv.get_name(up_count, i).as_str());
                }
                Op::SetDynamic => {
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    println!("set_dynamic {}  ;; {}", i, constants.get(i));
                }
                Op::PushEnv => {
                    let i = insns.get(pc) as usize;
                    pc += 1;
                    let new_senv = environments.get(i);
                    assert_eq!(new_senv.parent(), Some(senv));
                    senv = new_senv;
                    let names = senv.names().into_iter().collect::<Vec<_>>();
                    println!("push_env {}  ;; {:?}", i, names);
                }
                Op::PopEnv => {
                    senv = senv.parent().unwrap();
                    println!("pop_env");
                }
            }
        }
        println!();
    }
}

pub fn compile_toplevel<'h>(
    hs: &mut GcHeapSession<'h>,
    senv: &StaticEnvironmentRef<'h>,
    expr: Value<'h>
) -> Result<CodeRef<'h>> {
    let mut emitter = Emitter::new(hs, senv.clone(), false);
    emitter.compile_toplevel(hs, senv, expr, Ctn::Tail)?;
    Ok(emitter.finish(hs))
}
