use std::fmt;

use crate::syntax::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
    Atom,
    App,
    Fun,
    Let,
}

impl Prec {
    pub const MAX: Self = Self::Let;

    pub fn of_expr(expr: &Expr) -> Self {
        match expr {
            Expr::Int(_) | Expr::Bool(_) | Expr::Var(..) => Self::Atom,
            Expr::Fun(..) => Self::Fun,
            Expr::App(..) => Self::App,
            Expr::Let(..) => Self::Let,
            Expr::If(..) => Self::Let,
        }
    }

    pub fn of_value(value: &Value) -> Self {
        match value {
            Value::Int(_) | Value::Bool(_) => Self::Atom,
            Value::Fun(..) => Self::Fun,
        }
    }
}

fn expr_prec(expr: &Expr, f: &mut fmt::Formatter, prec: Prec) -> fmt::Result {
    let paren = Prec::of_expr(expr) > prec;
    if paren {
        write!(f, "(")?;
    }
    match expr {
        Expr::Int(n) => write!(f, "{n}")?,
        Expr::Bool(b) => write!(f, "{b}")?,
        Expr::Var(_var, name) => write!(f, "{name}")?,
        Expr::Fun(name, body) => write!(f, "fun {name} => {body:?}")?,
        Expr::App(fun, arg) => {
            expr_prec(fun, f, Prec::Atom)?;
            write!(f, " ")?;
            expr_prec(arg, f, Prec::Atom)?;
        }
        Expr::Let(name, init, body) => {
            write!(f, "let {name} = ")?;
            expr_prec(init, f, Prec::Fun)?;
            write!(f, " in ")?;
            expr_prec(body, f, Prec::Let)?;
        }
        Expr::If(cond, then, r#else) => {
            write!(f, "if ")?;
            expr_prec(cond, f, Prec::App)?;
            write!(f, " then ")?;
            expr_prec(then, f, Prec::Fun)?;
            write!(f, " else ")?;
            expr_prec(r#else, f, Prec::Fun)?;
        }
    }
    if paren {
        write!(f, ")")?;
    }
    Ok(())
}

fn value_prec(value: &Value, f: &mut fmt::Formatter, prec: Prec) -> fmt::Result {
    let paren = Prec::of_value(value) > prec;
    if paren {
        write!(f, "(")?;
    }
    match value {
        Value::Int(n) => write!(f, "{n}")?,
        Value::Bool(b) => write!(f, "{b}")?,
        Value::Fun(name, _, body) => write!(f, "fun {name} => {body:?}")?,
    }
    if paren {
        write!(f, ")")?;
    }
    Ok(())
}

impl fmt::Debug for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { expr_prec(self, f, Prec::MAX) }
}

impl fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { value_prec(self, f, Prec::MAX) }
}

#[cfg(test)]
mod tests {
    use expect_test::*;

    use super::*;

    fn assert_debug(it: impl fmt::Debug, expect: Expect) { expect.assert_eq(&format!("{:?}", it)); }

    #[test]
    fn test_debug_int() {
        let expr = Expr::Int(42);
        assert_debug(expr, expect!["42"]);
    }

    #[test]
    fn test_debug_var() {
        let expr = Expr::Var(0, "x");
        assert_debug(expr, expect!["x"]);
    }

    #[test]
    fn test_debug_fun() {
        let expr = Expr::Fun("x", &(Expr::Var(0, "x")));
        assert_debug(expr, expect!["fun x => x"]);
    }

    #[test]
    fn test_debug_app() {
        let expr = Expr::App(&Expr::Var(0, "f"), &Expr::Int(42));
        assert_debug(expr, expect!["f 42"]);

        let expr = Expr::App(
            &Expr::Var(0, "f"),
            &Expr::App(&Expr::Var(1, "g"), &Expr::Int(99)),
        );
        assert_debug(expr, expect!["f (g 99)"]);
    }

    #[test]
    fn test_debug_let() {
        let expr = Expr::Let("x", &Expr::Int(42), &Expr::Var(0, "x"));
        assert_debug(expr, expect!["let x = 42 in x"]);

        let expr = Expr::Let(
            "x",
            &Expr::Let("y", &Expr::Int(42), &Expr::Int(0)),
            &Expr::Var(0, "x"),
        );
        assert_debug(expr, expect!["let x = (let y = 42 in 0) in x"]);
    }

    #[test]
    fn test_debug_ifz() {
        let expr = Expr::If(&Expr::Var(0, "b"), &Expr::Int(1), &Expr::Int(0));
        assert_debug(expr, expect!["if b then 1 else 0"]);
    }

    #[test]
    fn test_debug_value_int() {
        let value = Value::Int(42);
        assert_debug(value, expect!["42"]);
    }

    #[test]
    fn test_debug_value_fun() {
        let value = Value::Fun("x", Env::default(), &Expr::Var(0, "x"));
        assert_debug(value, expect!["fun x => x"]);
    }
}
