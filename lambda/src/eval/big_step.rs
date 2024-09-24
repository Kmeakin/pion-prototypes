use super::*;

pub fn eval<'core>(expr: &Expr<'core>, env: &mut Env<'core>) -> Value<'core> {
    match expr {
        Expr::Int(n) => Value::Int(*n),
        Expr::Bool(b) => Value::Bool(*b),
        Expr::Var(var, name) => get_local(env, *var, name),
        Expr::Fun(name, body) => Value::Fun(name, env.clone(), body),
        Expr::App(fun, arg) => {
            let fun = eval(fun, env);
            let arg = eval(arg, env);
            match fun {
                Value::Fun(_name, mut env, body) => {
                    env.push(arg);
                    eval(body, &mut env)
                }
                _ => panic!("Expected function, got {fun}"),
            }
        }
        Expr::Binop(op, lhs, rhs) => {
            let lhs = eval(lhs, env);
            let rhs = eval(rhs, env);
            let (Value::Int(lhs), Value::Int(rhs)) = (&lhs, &rhs) else {
                panic!("Expected integers, got {lhs} and {rhs}");
            };
            let (lhs, rhs) = (*lhs, *rhs);
            match op {
                Binop::Add => Value::Int(u32::wrapping_add(lhs, rhs)),
                Binop::Sub => Value::Int(u32::wrapping_sub(lhs, rhs)),
                Binop::Mul => Value::Int(u32::wrapping_mul(lhs, rhs)),
                Binop::Eq => Value::Bool(u32::eq(&lhs, &rhs)),
                Binop::Ne => Value::Bool(u32::ne(&lhs, &rhs)),
                Binop::Le => Value::Bool(u32::le(&lhs, &rhs)),
                Binop::Ge => Value::Bool(u32::ge(&lhs, &rhs)),
                Binop::Lt => Value::Bool(u32::lt(&lhs, &rhs)),
                Binop::Gt => Value::Bool(u32::gt(&lhs, &rhs)),
            }
        }
        Expr::Let(_name, init, body) => {
            let init = eval(init, env);
            env.push(init);
            let body = eval(body, env);
            env.pop();
            body
        }
        Expr::If(cond, then, r#else) => {
            let cond = eval(cond, env);
            match cond {
                Value::Bool(true) => eval(then, env),
                Value::Bool(false) => eval(r#else, env),
                _ => panic!("Expected boolean, got {cond}"),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::*;

    use super::*;

    #[track_caller]
    fn assert_eval(expr: &Expr, expect: Expect) {
        let mut env = Env::new();
        let value = eval(expr, &mut env);
        expect.assert_eq(&format!("{value}"));
    }

    #[test]
    fn test_eval_int() { assert_eval(&Expr::Int(42), expect!["42"]); }

    #[test]
    fn test_eval_bool() {
        assert_eval(&Expr::Bool(true), expect!["true"]);
        assert_eval(&Expr::Bool(false), expect!["false"]);
    }

    #[test]
    fn test_eval_fun() { assert_eval(&Expr::Fun("x", &Expr::Var(1, "x")), expect!["fun x => x"]); }

    #[test]
    fn test_eval_app() {
        assert_eval(
            &Expr::App(&Expr::Fun("x", &Expr::Var(0, "x")), &(Expr::Int(42))),
            expect!["42"],
        );
    }
}
