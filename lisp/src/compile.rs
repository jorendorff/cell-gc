use value::{InternedString, Pair, Value};
use value::Value::*;
use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;

#[derive(IntoHeap)]
pub enum Expr<'h> {
    /// A constant (`quote` expressions produce this, but also numbers and
    /// other self-evaluating values).
    Con(Value<'h>),

    /// A variable-expression (evaluates to the variable's value).
    Var(GcLeaf<InternedString>),

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
    pub params: Vec<GcLeaf<InternedString>>,
    pub rest: Option<GcLeaf<InternedString>>,
    pub body: Expr<'h>,
}

#[derive(IntoHeap)]
pub struct Def<'h> {
    pub name: GcLeaf<InternedString>,
    pub value: Expr<'h>
}

#[derive(IntoHeap)]
pub struct If<'h> {
    pub cond: Expr<'h>,
    pub t_expr: Expr<'h>,
    pub f_expr: Expr<'h>,
}

#[derive(IntoHeap)]
pub struct Letrec<'h> {
    pub names: VecRef<'h, GcLeaf<InternedString>>,
    pub exprs: VecRef<'h, Expr<'h>>,
    pub body: Expr<'h>
}

// Convert the linked list of a `<body>` to a vector; also splice in the
// contents of `(begin)` expressions nested within the `<body>`.
fn flatten_body<'h>(forms: Value<'h>, out: &mut Vec<Value<'h>>) -> Result<(), String> {
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
    body_list: Value<'h>,
) -> Result<Expr<'h>, String> {
    let mut forms = vec![];
    flatten_body(body_list, &mut forms)?;

    let mut names = vec![];
    let mut exprs = vec![];

    let mut i = 0;
    while i < forms.len() && is_definition(&forms[i]) {
        let (name, expr) = parse_define(hs, forms[i].clone())?;
        names.push(name);
        exprs.push(expr);
        i += 1;
    }

    if i == forms.len() {
        return Err("expression required".into());
    }

    let names = hs.alloc(names);
    let exprs = hs.alloc(exprs);
    let body_exprs: Result<Vec<Expr>, String> = forms
        .drain(i..)
        .map(|form| compile_expr(hs, form))
        .collect();
    let body = seq(hs, body_exprs?);
    Ok(Expr::Letrec(hs.alloc(Letrec {
        names,
        exprs,
        body,
    })))
}

fn seq<'h>(hs: &mut GcHeapSession<'h>, mut exprs: Vec<Expr<'h>>) -> Expr<'h> {
    if exprs.len() == 0 {
        Expr::Con(Value::Nil)
    } else if exprs.len() == 1 {
        exprs.pop().unwrap()
    } else {
        Expr::Seq(hs.alloc(exprs))
    }
}

/// On success, returns the two parts of a `(define)` that we care about: the
/// name to define and the compiled expression to populate it.
fn parse_define<'h>(hs: &mut GcHeapSession<'h>, mut defn: Value<'h>)
    -> Result<(GcLeaf<InternedString>, Expr<'h>), String>
{
    loop {
        let (define_symbol, tail) = defn.as_pair("internal error")?;
        let (pattern, rest) = tail.as_pair("(define) with no name")?;
        match pattern {
            Symbol(ident) => {
                let (expr, rest) = rest.as_pair("(define) with no value")?;
                match rest {
                    Nil => {}
                    _ => {
                        return Err(
                            "too many arguments in (define)".to_string(),
                        )
                    }
                };

                let value = compile_expr(hs, expr)?;
                return Ok((ident, value));
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
                    cdr: defn_cdr
                }));
            }
            _ => return Err("(define) with a non-symbol name".to_string())
        }
    }
}

pub fn compile_toplevel<'h>(
    hs: &mut GcHeapSession<'h>,
    expr: Value<'h>,
) -> Result<Expr<'h>, String> {
    // TODO: support (begin) here
    if is_definition(&expr) {
        let (name, value) = parse_define(hs, expr)?;
        Ok(Expr::Def(hs.alloc(Def { name, value })))
    } else {
        compile_expr(hs, expr)
    }
}

pub fn compile_expr<'h>(
    hs: &mut GcHeapSession<'h>,
    expr: Value<'h>,
) -> Result<Expr<'h>, String> {
    match expr {
        Symbol(s) => Ok(Expr::Var(s)),

        Cons(p) => {
            let f = p.car();
            if let Symbol(ref s) = f {
                if s.as_str() == "lambda" {
                    let (mut param_list, body_forms) = p.cdr().as_pair("syntax error in lambda")?;

                    let mut params = vec![];
                    while let Cons(pair) = param_list {
                        if let Symbol(s) = pair.car() {
                            params.push(s);
                        } else {
                            return Err("syntax error in lambda arguments".to_string());
                        }
                        param_list = pair.cdr();
                    }
                    let rest =
                        match param_list {
                            Nil => None,
                            Symbol(rest_name) => Some(rest_name),
                            _ => return Err("syntax error in lambda arguments".to_string())
                        };

                    let body = compile_body(hs, body_forms)?;
                    return Ok(Expr::Fun(hs.alloc(Code { params, rest, body })));
                } else if s.as_str() == "quote" {
                    let (datum, rest) = p.cdr().as_pair("(quote) with no arguments")?;
                    if !rest.is_nil() {
                        return Err("too many arguments to (quote)".to_string());
                    }
                    return Ok(Expr::Con(datum));
                } else if s.as_str() == "if" {
                    let (cond, rest) = p.cdr().as_pair("(if) with no arguments")?;
                    let (tc, rest) = rest.as_pair("missing arguments after (if COND)")?;
                    let (fc, rest) =
                        rest.as_pair("missing 'else' argument after (if COND X)")?;
                    if !rest.is_nil() {
                        return Err("too many arguments in (if) expression".to_string());
                    }
                    let cond = compile_expr(hs, cond)?;
                    let t_expr = compile_expr(hs, tc)?;
                    let f_expr = compile_expr(hs, fc)?;
                    return Ok(Expr::If(hs.alloc(If { cond, t_expr, f_expr })));
                } else if s.as_str() == "begin" {
                    // In expression context, this is sequencing, not splicing.
                    let mut exprs = vec![];
                    for expr_result in p.cdr() {
                        let expr = expr_result?;
                        exprs.push(compile_expr(hs, expr)?);
                    }
                    return Ok(Expr::Seq(hs.alloc(exprs)));
                } else if s.as_str() == "define" {
                    // In expression context, definitions aren't allowed.
                    return Err("(define) is allowed only at toplevel or in the body \
                                of a function or let-form".into());
                } else if s.as_str() == "letrec*" {
                    let (bindings, body_forms) = p.cdr().as_pair("letrec*: bindings required")?;
                    let mut names = vec![];
                    let mut exprs = vec![];
                    for binding_result in bindings {
                        let binding = binding_result?;
                        let (name_v, rest) = binding.as_pair("letrec*: invalid binding")?;
                        let name = name_v.as_symbol("letrec*: name required")?;
                        let (expr, rest) = rest.as_pair("letrec*: value required for binding")?;
                        if !rest.is_nil() {
                            return Err("(letrec*): too many arguments".to_string());
                        }
                        names.push(GcLeaf::new(name));
                        exprs.push(compile_expr(hs, expr)?);
                    }
                    let names = hs.alloc(names);
                    let exprs = hs.alloc(exprs);
                    let body = compile_body(hs, body_forms)?;
                    return Ok(Expr::Letrec(hs.alloc(Letrec {
                        names,
                        exprs,
                        body,
                    })));
                } else if s.as_str() == "set!" {
                    let (first, rest) = p.cdr().as_pair("(set!) with no name")?;
                    let name = first.as_symbol("(set!) first argument must be a name")?;
                    let (expr, rest) = rest.as_pair("(set!) with no value")?;
                    if !rest.is_nil() {
                        return Err("(set!): too many arguments".to_string());
                    }
                    let value = compile_expr(hs, expr)?;
                    return Ok(Expr::Set(hs.alloc(Def {
                        name: GcLeaf::new(name),
                        value: value
                    })));
                }
            }

            let subexprs: Vec<Expr<'h>> =
                Cons(p)
                    .into_iter()
                    .map(|v| compile_expr(hs, v?))
                    .collect::<Result<_, String>>()?;
            Ok(Expr::App(hs.alloc(subexprs)))
        }

        // Self-evaluating values.
        Bool(v) => Ok(Expr::Con(Bool(v))),
        Int(v) => Ok(Expr::Con(Int(v))),
        ImmString(v) => Ok(Expr::Con(ImmString(v))),

        // Everything else is an error.
        _ => Err(format!("not an expression")),
    }
}
