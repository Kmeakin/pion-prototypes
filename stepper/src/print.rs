use crate::syntax::*;
use std::fmt;

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
            Expr::Int(_) | Expr::Var(_) => Self::Atom,
            Expr::Fun(_, _) => Self::Fun,
            Expr::App(_, _) => Self::App,
            Expr::Let(_, _, _) => Self::Let,
            Expr::IfZ(_, _, _) => Self::Let,
        }
    }

    pub fn of_value(value: &Value) -> Self {
        match value {
            Value::Int(_) => Self::Atom,
            Value::Fun(_, _, _) => Self::Fun,
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
        Expr::Var(var) => write!(f, "${var}")?,
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
        Expr::IfZ(cond, then, r#else) => {
            write!(f, "ifz ")?;
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
        Value::Fun(name, _, body) => write!(f, "fun {name} => {body:?}")?,
    }
    if paren {
        write!(f, ")")?;
    }
    Ok(())
}

impl fmt::Debug for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        expr_prec(self, f, Prec::MAX)
    }
}

impl fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        value_prec(self, f, Prec::MAX)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::*;

    fn assert_debug(it: impl fmt::Debug, expect: Expect) {
        expect.assert_eq(&format!("{:?}", it));
    }

    #[test]
    fn test_debug_int() {
        let expr = Expr::Int(42);
        assert_debug(expr, expect!["42"]);
    }

    #[test]
    fn test_debug_var() {
        let expr = Expr::Var(0);
        assert_debug(expr, expect!["$0"]);
    }

    #[test]
    fn test_debug_fun() {
        let expr = Expr::Fun("x", &(Expr::Var(0)));
        assert_debug(expr, expect!["fun x => $0"]);
    }

    #[test]
    fn test_debug_app() {
        let expr = Expr::App(&(Expr::Var(0)), &(Expr::Int(42)));
        assert_debug(expr, expect!["$0 42"]);

        let expr = Expr::App(&Expr::Var(0), &Expr::App(&Expr::Var(0), &Expr::Int(99)));
        assert_debug(expr, expect!["$0 ($0 99)"]);
    }

    #[test]
    fn test_debug_let() {
        let expr = Expr::Let("x", &Expr::Int(42), &Expr::Var(0));
        assert_debug(expr, expect!["let x = 42 in $0"]);

        let expr = Expr::Let(
            "x",
            &Expr::Let("y", &Expr::Int(42), &Expr::Int(0)),
            &Expr::Var(0),
        );
        assert_debug(expr, expect!["let x = (let y = 42 in 0) in $0"]);
    }

    #[test]
    fn test_debug_ifz() {
        let expr = Expr::IfZ(&(Expr::Var(0)), &(Expr::Int(1)), &(Expr::Int(0)));
        assert_debug(expr, expect!["ifz $0 then 1 else 0"]);
    }

    #[test]
    fn test_debug_value_int() {
        let value = Value::Int(42);
        assert_debug(value, expect!["42"]);
    }

    #[test]
    fn test_debug_value_fun() {
        let value = Value::Fun("x", Env::default(), &(Expr::Var(0)));
        assert_debug(value, expect!["fun x => $0"]);
    }
}
