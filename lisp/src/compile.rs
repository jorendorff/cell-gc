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

pub fn compile_body<'h>(
    hs: &mut GcHeapSession<'h>,
    body: Value<'h>,
) -> Result<Expr<'h>, String> {
    let compile_result: Result<Vec<Expr<'h>>, String> =
        body.into_iter()
        .map(|expr| compile(hs, expr?))
        .collect();
    let mut exprs = compile_result?;
    if exprs.len() == 0 {
        Ok(Expr::Con(Value::Nil))
    } else if exprs.len() == 1 {
        Ok(exprs.pop().unwrap())
    } else {
        Ok(Expr::Seq(hs.alloc(exprs)))
    }
}

pub fn compile<'h>(
    hs: &mut GcHeapSession<'h>,
    expr: Value<'h>,
) -> Result<Expr<'h>, String> {
    match expr {
        Symbol(s) => Ok(Expr::Var(s)),

        Cons(p) => {
            let f = p.car();
            if let Symbol(ref s) = f {
                if s.as_str() == "lambda" {
                    let (mut param_list, body_lisp) = p.cdr().as_pair("syntax error in lambda")?;

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

                    let body = compile_body(hs, body_lisp)?;
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
                    let cond = compile(hs, cond)?;
                    let t_expr = compile(hs, tc)?;
                    let f_expr = compile(hs, fc)?;
                    return Ok(Expr::If(hs.alloc(If { cond, t_expr, f_expr })));
                } else if s.as_str() == "begin" {
                    return compile_body(hs, p.cdr());
                } else if s.as_str() == "define" {
                    let (name, rest) = p.cdr().as_pair("(define) with no name")?;
                    match name {
                        Symbol(s) => {
                            let (expr, rest) = rest.as_pair("(define) with no value")?;
                            match rest {
                                Nil => {}
                                _ => {
                                    return Err(
                                        "too many arguments in (define)".to_string(),
                                    )
                                }
                            };

                            let value = compile(hs, expr)?;
                            return Ok(Expr::Def(hs.alloc(Def { name: s, value })));
                        }
                        Cons(pair) => {
                            // Build desugared definition and compile that.
                            let define_sym = p.car();
                            let name = pair.car();  // shadows earlier name variable
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
                            let defn = Cons(hs.alloc(Pair {
                                car: define_sym,
                                cdr: defn_cdr,
                            }));
                            return compile(hs, defn);
                        }
                        _ => {
                            return Err("(define) with a non-symbol name".to_string());
                        }
                    }
                } else if s.as_str() == "set!" {
                    let (first, rest) = p.cdr().as_pair("(set!) with no name")?;
                    let name = first.as_symbol("(set!) first argument must be a name")?;
                    let (expr, rest) = rest.as_pair("(set!) with no value")?;
                    if !rest.is_nil() {
                        return Err("(set!): too many arguments".to_string());
                    }
                    let value = compile(hs, expr)?;
                    return Ok(Expr::Set(hs.alloc(Def {
                        name: GcLeaf::new(name),
                        value: value
                    })));
                }
            }

            let subexprs: Vec<Expr<'h>> =
                Cons(p)
                    .into_iter()
                    .map(|v| compile(hs, v?))
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
