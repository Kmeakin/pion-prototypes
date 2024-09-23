use core::fmt;
use std::fmt::Write;

use crate::syntax::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Prec {
    Atom,
    App,

    Add,
    Mul,
    Cmp,

    Fun,
    Let,
    If,
}

impl Prec {
    pub const MAX: Self = Self::If;

    pub fn of_expr(expr: &Expr) -> Self {
        match expr {
            Expr::Int(_) | Expr::Bool(_) | Expr::Var(..) => Self::Atom,
            Expr::Fun(..) => Self::Fun,
            Expr::App(..) => Self::App,
            Expr::Binop(op, ..) => Self::of_binop(*op),
            Expr::Let(..) => Self::Let,
            Expr::If(..) => Self::If,
        }
    }

    pub fn of_value(value: &Value) -> Self {
        match value {
            Value::Int(_) | Value::Bool(_) => Self::Atom,
            Value::Fun(..) => Self::Fun,
        }
    }

    pub fn of_binop(binop: Binop) -> Self {
        match binop {
            Binop::Add | Binop::Sub => Self::Add,
            Binop::Mul => Self::Mul,
            Binop::Eq | Binop::Ne | Binop::Le | Binop::Ge | Binop::Lt | Binop::Gt => Self::Cmp,
        }
    }
}

fn parens<W: Write>(
    out: &mut W,
    cond: bool,
    mut f: impl FnMut(&mut W) -> fmt::Result,
) -> fmt::Result {
    if cond {
        out.write_char('(')?;
    }
    f(out)?;
    if cond {
        out.write_char(')')?;
    }
    Ok(())
}

pub fn expr_prec(out: &mut impl Write, expr: &Expr, prec: Prec) -> fmt::Result {
    parens(out, Prec::of_expr(expr) > prec, |out| match expr {
        Expr::Int(n) => write!(out, "{n}"),
        Expr::Bool(b) => write!(out, "{b}"),
        Expr::Var(_, name) => write!(out, "{name}"),
        Expr::Fun(name, body) => {
            write!(out, "fun {name} => ")?;
            expr_prec(out, body, Prec::Fun)
        }
        Expr::App(fun, arg) => {
            expr_prec(out, fun, Prec::App)?;
            write!(out, " ")?;
            expr_prec(out, arg, Prec::Atom)
        }
        Expr::Binop(op, lhs, rhs) => {
            expr_prec(out, lhs, Prec::of_binop(*op))?;
            write!(out, " {op} ")?;
            expr_prec(out, rhs, Prec::of_binop(*op))
        }
        Expr::Let(name, init, body) => {
            write!(out, "let {name} = ")?;
            expr_prec(out, init, Prec::Fun)?;
            write!(out, " in ")?;
            expr_prec(out, body, Prec::Let)
        }
        Expr::If(cond, then, r#else) => {
            write!(out, "if ")?;
            expr_prec(out, cond, Prec::Cmp)?;
            write!(out, " then ")?;
            expr_prec(out, then, Prec::Fun)?;
            write!(out, " else ")?;
            expr_prec(out, r#else, Prec::Fun)
        }
    })
}

pub fn value_prec(out: &mut impl Write, value: &Value, prec: Prec) -> fmt::Result {
    parens(out, Prec::of_value(value) > prec, |out| match value {
        Value::Int(n) => write!(out, "{n}"),
        Value::Bool(b) => write!(out, "{b}"),
        Value::Fun(name, _, body) => {
            write!(out, "fun {name} => ")?;
            expr_prec(out, body, Prec::Fun)?;
            Ok(())
        }
    })
}

impl<'core> fmt::Display for Expr<'core> {
    fn fmt(&self, out: &mut fmt::Formatter) -> fmt::Result { expr_prec(out, self, Prec::MAX) }
}

impl<'core> fmt::Display for Value<'core> {
    fn fmt(&self, out: &mut fmt::Formatter) -> fmt::Result { value_prec(out, self, Prec::MAX) }
}

impl fmt::Display for Binop {
    fn fmt(&self, out: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(out, "+"),
            Self::Sub => write!(out, "-"),
            Self::Mul => write!(out, "*"),
            Self::Eq => write!(out, "=="),
            Self::Ne => write!(out, "!="),
            Self::Le => write!(out, "<="),
            Self::Ge => write!(out, ">="),
            Self::Lt => write!(out, "<"),
            Self::Gt => write!(out, ">"),
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::*;

    use super::*;

    #[track_caller]
    fn assert_print(expr: Expr, expect: Expect) { expect.assert_eq(&format!("{expr}")); }

    #[test]
    fn test_print_int() { assert_print(Expr::Int(42), expect!["42"]); }

    #[test]
    fn test_print_bool() {
        assert_print(Expr::Bool(true), expect!["true"]);
        assert_print(Expr::Bool(false), expect!["false"]);
    }

    #[test]
    fn test_print_var() { assert_print(Expr::Var(0, "x"), expect!["x"]); }

    #[test]
    fn test_print_fun() { assert_print(Expr::Fun("x", &Expr::Var(0, "x")), expect!["fun x => x"]); }

    #[test]
    fn test_print_app() {
        assert_print(
            Expr::App(&Expr::Var(0, "f"), &Expr::Int(42)),
            expect!["f 42"],
        );
    }

    #[test]
    fn test_print_binop() {
        assert_print(
            Expr::Binop(Binop::Add, &Expr::Int(1), &Expr::Int(2)),
            expect!["1 + 2"],
        );
        assert_print(
            Expr::Binop(Binop::Mul, &Expr::Int(3), &Expr::Int(4)),
            expect!["3 * 4"],
        );
    }

    #[test]
    fn test_print_let() {
        assert_print(
            Expr::Let("x", &Expr::Int(42), &Expr::Var(0, "x")),
            expect!["let x = 42 in x"],
        );
    }

    #[test]
    fn test_print_if() {
        assert_print(
            Expr::If(&Expr::Bool(true), &Expr::Int(1), &Expr::Int(0)),
            expect!["if true then 1 else 0"],
        );
    }

    #[test]
    fn test_print_nested_binop() {
        assert_print(
            Expr::Binop(
                Binop::Add,
                &Expr::Binop(Binop::Mul, &Expr::Int(2), &Expr::Int(3)),
                &Expr::Int(4),
            ),
            expect!["(2 * 3) + 4"],
        );
    }

    #[test]
    fn test_print_nested_let() {
        assert_print(
            Expr::Let(
                "x",
                &Expr::Int(42),
                &Expr::Let("y", &Expr::Int(43), &Expr::Var(0, "x")),
            ),
            expect!["let x = 42 in let y = 43 in x"],
        );
    }

    #[test]
    fn test_print_nested_if() {
        assert_print(
            Expr::If(
                &Expr::Bool(true),
                &Expr::If(&Expr::Bool(false), &Expr::Int(1), &Expr::Int(0)),
                &Expr::Int(2),
            ),
            expect!["if true then (if false then 1 else 0) else 2"],
        );
    }

    #[test]
    fn test_print_mixed_nested() {
        assert_print(
            Expr::If(
                &Expr::Binop(Binop::Eq, &Expr::Int(1), &Expr::Int(1)),
                &Expr::Let(
                    "x",
                    &Expr::Int(42),
                    &Expr::Binop(Binop::Add, &Expr::Var(0, "x"), &Expr::Int(1)),
                ),
                &Expr::Int(0),
            ),
            expect!["if 1 == 1 then (let x = 42 in x + 1) else 0"],
        );
    }
}
