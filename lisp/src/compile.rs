//! Compile Scheme forms into an internal representation, Expr.

use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;
use env::StaticEnvironmentRef;
use errors::Result;
use std::fmt;
use value::{InternedString, Pair, Value};
use value::Value::*;

#[derive(IntoHeap)]
pub enum Expr<'h> {
    /// A constant (`quote` expressions produce this, but also numbers and
    /// other self-evaluating values).
    Con(Value<'h>),

    /// A variable-expression (evaluates to the variable's value).
    Var(GcLeaf<InternedString>),

    /// A variable-expression, but at a known location in the environment chain.
    FastVar { up_count: u32, index: u32 },

    /// A lambda expression.
    Fun(CodeRef<'h>),

    /// A function call expression (an application).
    App(VecRef<'h, Expr<'h>>),

    /// A sequence-expression (`begin` used as an expression).
    Seq(VecRef<'h, Expr<'h>>),

    /// A conditional expression (`if`).
    If(IfRef<'h>),

    /// A definition.
    Def(DefRef<'h>),

    /// An assignment expression (`set!`).
    Set(DefRef<'h>),

    /// A `letrec*` expression.
    Letrec(LetrecRef<'h>),
}

#[derive(IntoHeap)]
pub struct Code<'h> {
    pub senv: StaticEnvironmentRef<'h>,
    pub rest: bool,
    pub body: Expr<'h>,
}

#[derive(IntoHeap)]
pub struct Def<'h> {
    pub name: GcLeaf<InternedString>,
    pub value: Expr<'h>,
}

#[derive(IntoHeap)]
pub struct If<'h> {
    pub cond: Expr<'h>,
    pub t_expr: Expr<'h>,
    pub f_expr: Expr<'h>,
}

#[derive(IntoHeap)]
pub struct Letrec<'h> {
    pub senv: StaticEnvironmentRef<'h>,
    pub exprs: VecRef<'h, Expr<'h>>,
    pub body: Expr<'h>,
}

impl<'h> fmt::Debug for Expr<'h> {
    fn fmt(&self, f: &mut fmt::Formatter) -> ::std::result::Result<(), fmt::Error> {
        match *self {
            Expr::Con(ref v) => write!(f, "'{}", v),
            Expr::Var(ref s) => write!(f, "{}", Value::Symbol(s.clone())),
            Expr::FastVar { up_count, index } =>
                write!(f, "#get<{}, {}>", up_count, index),
            Expr::Fun(ref c) => write!(f, "(lambda ... {:?})", c.body()),
            Expr::App(ref args) => {
                write!(
                    f,
                    "({})",
                    (0..args.len())
                        .map(|i| format!("{:?}", args.get(i)))
                        .collect::<Vec<String>>()
                        .join(" ")
                )
            }
            Expr::Seq(ref exprs) => {
                write!(
                    f,
                    "(begin {})",
                    (0..exprs.len())
                        .map(|i| format!("{:?}", exprs.get(i)))
                        .collect::<Vec<String>>()
                        .join(" ")
                )
            }
            Expr::If(ref r) => write!(f, "(if {:?} {:?} {:?})", r.cond(), r.t_expr(), r.f_expr()),
            Expr::Def(ref r) => write!(f, "(define {} {:?})", Value::Symbol(r.name()), r.value()),
            Expr::Set(ref r) => write!(f, "(set! {} {:?})", Value::Symbol(r.name()), r.value()),
            Expr::Letrec(ref r) => {
                write!(
                    f,
                    "(letrec ({}) {:?})",
                    r.senv().names().into_iter()
                        .zip(r.exprs())
                        .map(|(name, expr)| format!("({} {:?})", name.as_str(), expr))
                        .collect::<Vec<String>>()
                        .join(" "),
                    r.body()
                )
            }
        }
    }
}

fn seq<'h>(hs: &mut GcHeapSession<'h>, mut exprs: Vec<Expr<'h>>) -> Expr<'h> {
    if exprs.len() == 0 {
        Expr::Con(Value::Unspecified)
    } else if exprs.len() == 1 {
        exprs.pop().unwrap()
    } else {
        Expr::Seq(hs.alloc(exprs))
    }
}

fn letrec<'h>(
    hs: &mut GcHeapSession<'h>,
    senv: StaticEnvironmentRef<'h>,
    exprs: Vec<Expr<'h>>,
    body: Expr<'h>,
) -> Expr<'h> {
    assert!(!exprs.is_empty());
    assert_eq!(senv.names().len(), exprs.len());
    let exprs = hs.alloc(exprs);
    Expr::Letrec(hs.alloc(Letrec { senv, exprs, body }))
}

// Convert the linked list of a `<body>` to a vector; also splice in the
// contents of `(begin)` expressions nested within the `<body>`.
//
// Bug: Both (begin defn ...) and (begin expr ...) forms flatten, but this
// also permits (begin defn ... expr ...) cases that should be errors.
fn flatten_body<'h>(forms: Value<'h>, out: &mut Vec<Value<'h>>) -> Result<()> {
    for form_res in forms {
        let form = form_res?;
        if let Cons(ref pair) = form {
            if let Symbol(op) = pair.car() {
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
    if let Cons(ref pair) = *form {
        if let Symbol(op) = pair.car() {
            if op.as_str() == "define" {
                return true;
            }
        }
    }
    false
}

// Compile the body of a lambda or letrec*.
fn compile_body<'h>(
    hs: &mut GcHeapSession<'h>,
    senv: &StaticEnvironmentRef<'h>,
    body_list: Value<'h>
) -> Result<Expr<'h>> {
    let mut forms = vec![];
    flatten_body(body_list, &mut forms)?;

    let mut names = vec![];
    let mut init_exprs = vec![];

    let mut i = 0;
    while i < forms.len() && is_definition(&forms[i]) {
        let (name, expr) = parse_define(hs, forms[i].clone())?;
        names.push(name);
        init_exprs.push(expr);
        i += 1;
    }
    if i == forms.len() {
        return Err("expression required".into());
    }

    let no_defines = names.is_empty();
    let body_senv =
        if no_defines {
            senv.clone()
        } else {
            senv.new_nested_environment(hs, names)
        };

    let init_exprs: Vec<Expr> = init_exprs
        .into_iter()
        .map(|expr| compile_expr(hs, &body_senv, expr))
        .collect::<Result<Vec<Expr>>>()?;
    let body_exprs: Vec<Expr> = forms
        .drain(i..)
        .map(|form| compile_expr(hs, &body_senv, form))
        .collect::<Result<Vec<Expr>>>()?;
    let body = seq(hs, body_exprs);
    Ok(
        if no_defines {
            body
        } else {
            letrec(hs, body_senv, init_exprs, body)
        }
    )
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
            Symbol(ident) => {
                let (expr, rest) = rest.as_pair("(define) with no value")?;
                match rest {
                    Nil => {}
                    _ => {
                        return Err("too many arguments in (define)".into());
                    }
                };
                return Ok((ident, expr));
            }
            Cons(pair) => {
                // Build desugared definition and compile that.
                let name = pair.car();
                let formals = pair.cdr();

                // Transform `(define (,name ,@formals) ,@rest)
                // to        `(define ,name (lambda ,formals ,@rest))
                let lambda_cdr = Cons(hs.alloc(Pair {
                    car: formals,
                    cdr: rest,
                }));
                let lambda = Cons(hs.alloc(Pair {
                    car: Symbol(GcLeaf::new(InternedString::get("lambda"))),
                    cdr: lambda_cdr,
                }));
                let defn_cddr = Cons(hs.alloc(Pair {
                    car: lambda,
                    cdr: Nil,
                }));
                let defn_cdr = Cons(hs.alloc(Pair {
                    car: name,
                    cdr: defn_cddr,
                }));
                defn = Cons(hs.alloc(Pair {
                    car: define_symbol,
                    cdr: defn_cdr,
                }));
            }
            _ => return Err("(define) with a non-symbol name".into()),
        }
    }
}

pub fn compile_toplevel<'h>(
    hs: &mut GcHeapSession<'h>,
    senv: &StaticEnvironmentRef<'h>,
    expr: Value<'h>
) -> Result<Expr<'h>> {
    // TODO: support (begin) here
    if is_definition(&expr) {
        let (name, init_expr) = parse_define(hs, expr)?;
        let init_expr = compile_expr(hs, senv, init_expr)?;
        Ok(Expr::Def(hs.alloc(Def { name, value: init_expr })))
    } else {
        compile_expr(hs, senv, expr)
    }
}

pub fn compile_expr<'h>(
    hs: &mut GcHeapSession<'h>,
    senv: &StaticEnvironmentRef<'h>,
    expr: Value<'h>
) -> Result<Expr<'h>> {
    match expr {
        Symbol(s) =>
            match senv.lookup(&s) {
                Some((up_count, index))
                    if up_count <= u32::max_value() as usize &&
                    index <= u32::max_value() as usize =>
                    Ok(Expr::FastVar {
                        up_count: up_count as u32,
                        index: index as u32
                    }),
                _ =>
                    Ok(Expr::Var(s)),
            },

        Cons(p) => {
            let f = p.car();
            if let Symbol(ref s) = f {
                if s.as_str() == "lambda" {
                    let (mut param_list, body_forms) = p.cdr().as_pair("syntax error in lambda")?;

                    let mut names = vec![];
                    while let Cons(pair) = param_list {
                        if let Symbol(s) = pair.car() {
                            names.push(s);
                        } else {
                            return Err("syntax error in lambda arguments".into());
                        }
                        param_list = pair.cdr();
                    }
                    let rest = match param_list {
                        Nil => false,
                        Symbol(rest_name) => {
                            names.push(rest_name);
                            true
                        }
                        _ => return Err("syntax error in lambda arguments".into()),
                    };

                    let lambda_senv = senv.new_nested_environment(hs, names);
                    let body = compile_body(hs, &lambda_senv, body_forms)?;
                    return Ok(Expr::Fun(hs.alloc(Code { senv: lambda_senv, rest, body })));
                } else if s.as_str() == "quote" {
                    let (datum, rest) = p.cdr().as_pair("(quote) with no arguments")?;
                    if !rest.is_nil() {
                        return Err("too many arguments to (quote)".into());
                    }
                    return Ok(Expr::Con(datum));
                } else if s.as_str() == "if" {
                    let (cond, rest) = p.cdr().as_pair("(if) with no arguments")?;
                    let cond = compile_expr(hs, senv, cond)?;
                    let (tc, rest) = rest.as_pair("missing arguments after (if COND)")?;
                    let t_expr = compile_expr(hs, senv, tc)?;
                    let f_expr = if rest == Nil {
                        Expr::Con(Unspecified)
                    } else {
                        let (fc, rest) = rest.as_pair("missing 'else' argument after (if COND X)")?;
                        if !rest.is_nil() {
                            return Err("too many arguments in (if) expression".into());
                        }
                        compile_expr(hs, senv, fc)?
                    };
                    return Ok(Expr::If(hs.alloc(If {
                        cond,
                        t_expr,
                        f_expr,
                    })));
                } else if s.as_str() == "begin" {
                    // In expression context, this is sequencing, not splicing.
                    let mut exprs = vec![];
                    for expr_result in p.cdr() {
                        let expr = expr_result?;
                        exprs.push(compile_expr(hs, senv, expr)?);
                    }
                    return Ok(seq(hs, exprs));
                } else if s.as_str() == "define" {
                    // In expression context, definitions aren't allowed.
                    return Err(
                        "(define) is allowed only at toplevel or in the body \
                         of a function or let-form"
                            .into(),
                    );
                } else if s.as_str() == "letrec" || s.as_str() == "letrec*" {
                    // Treat (letrec) forms just like (letrec*). Nonstandard in
                    // R6RS, which requires implementations to detect invalid
                    // references to letrec bindings before they're bound. But
                    // R5RS does not require this, and anyway well-behaved
                    // programs won't care.
                    let (bindings, body_forms) = p.cdr().as_pair("letrec*: bindings required")?;
                    let mut names = vec![];
                    let mut exprs = vec![];
                    for binding_result in bindings {
                        let binding = binding_result?;
                        let (name_v, rest) = binding.as_pair("letrec*: invalid binding")?;
                        let name = name_v.as_symbol("letrec*: name required")?;
                        let (expr, rest) = rest.as_pair("letrec*: value required for binding")?;
                        if !rest.is_nil() {
                            return Err("(letrec*): too many arguments".into());
                        }
                        names.push(GcLeaf::new(name));
                        exprs.push(expr);
                    }
                    if names.is_empty() {
                        return compile_body(hs, senv, body_forms);
                    }

                    let body_senv = senv.new_nested_environment(hs, names);
                    let exprs = exprs.into_iter()
                        .map(|expr| compile_expr(hs, &body_senv, expr))
                        .collect::<Result<Vec<Expr<'h>>>>()?;
                    let body = compile_body(hs, &body_senv, body_forms)?;
                    return Ok(letrec(hs, body_senv, exprs, body));
                } else if s.as_str() == "set!" {
                    let (first, rest) = p.cdr().as_pair("(set!) with no name")?;
                    let name = first.as_symbol("(set!) first argument must be a name")?;
                    let (expr, rest) = rest.as_pair("(set!) with no value")?;
                    if !rest.is_nil() {
                        return Err("(set!): too many arguments".into());
                    }
                    let value = compile_expr(hs, senv, expr)?;
                    return Ok(Expr::Set(hs.alloc(Def {
                        name: GcLeaf::new(name),
                        value: value,
                    })));
                }
            }

            let subexprs: Vec<Expr<'h>> = Cons(p)
                .into_iter()
                .map(|v| compile_expr(hs, senv, v?))
                .collect::<Result<_>>()?;
            Ok(Expr::App(hs.alloc(subexprs)))
        }

        // Self-evaluating values.
        Bool(v) => Ok(Expr::Con(Bool(v))),
        Int(v) => Ok(Expr::Con(Int(v))),
        Char(v) => Ok(Expr::Con(Char(v))),
        ImmString(v) => Ok(Expr::Con(ImmString(v))),

        // Note: Not sure what R6RS says about "three-dimensional" code,
        // eval code containing "constants" (either quoted or self-evaluating)
        // that are passed through. Possibly both this and the (quote) case
        // should do more work, to "flatten" the constants.
        StringObj(v) => Ok(Expr::Con(StringObj(v))),

        // Everything else is an error.
        _ => Err("not an expression".into()),
    }
}
