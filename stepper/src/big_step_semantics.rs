use crate::syntax::*;

/// Big step semantics
pub fn eval<'core>(expr: &Expr<'core>, env: &mut Env<'core>) -> Value<'core> {
    match expr {
        Expr::Int(n) => Value::Int(*n),
        Expr::Bool(b) => Value::Bool(*b),
        Expr::Var(var, name) => get_local(env, name, *var),
        Expr::Fun(name, body) => Value::Fun(name, env.clone(), body),

        Expr::App(fun, arg) => {
            let fun = eval(fun, env);
            let arg = eval(arg, env);
            match fun {
                Value::Fun(_, mut new_env, body) => {
                    new_env.push(arg);
                    eval(body, &mut new_env)
                }
                _ => Value::Error(Error::CalleeNotFun {}),
            }
        }

        Expr::Let(_name, init, body) => {
            let init = eval(init, env);
            env.push(init);
            let body = eval(body, env);
            env.pop().unwrap();
            body
        }

        Expr::If(cond, then, r#else) => {
            let cond = eval(cond, env);
            match cond {
                Value::Bool(true) => eval(then, env),
                Value::Bool(false) => eval(r#else, env),
                _ => Value::Error(Error::CondNotBool),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::*;

    use super::*;

    fn assert_eval<'core>(expr: Expr<'core>, mut env: Env<'core>, expect: Expect) {
        let value = eval(&expr, &mut env);
        expect.assert_eq(&format!("{value}"));
    }

    #[test]
    fn test_eval_int() {
        let expr = Expr::Int(42);
        assert_eval(expr, Env::default(), expect![[r"42"]]);
    }

    #[test]
    fn test_eval_var() {
        let expr = Expr::Var(0, "x");
        assert_eval(expr, vec![Value::Int(42)], expect![["42"]]);
    }

    #[test]
    fn test_eval_fun() {
        let expr = Expr::Fun("x", &Expr::Var(0, "x"));
        assert_eval(expr, Env::default(), expect!["fun x => x"]);
    }

    #[test]
    fn test_eval_app() {
        // (fun x => x) 42
        let fun = Expr::Fun("x", &Expr::Var(0, "x"));
        let app = Expr::App(&fun, &Expr::Int(42));
        assert_eval(app, Env::default(), expect![["42"]]);

        // (fun x y => x) 42
        let fun = Expr::Fun("x", &Expr::Fun("y", &Expr::Var(1, "y")));
        let app = Expr::App(&fun, &Expr::Int(42));
        assert_eval(app, Env::default(), expect!["fun y => y"]);

        // (fun x y => x) 42 99
        let fun = Expr::Fun("x", &Expr::Fun("y", &Expr::Var(1, "x")));
        let fun = Expr::App(&fun, &Expr::Int(42));
        let app = Expr::App(&fun, &Expr::Int(99));
        assert_eval(app, Env::default(), expect!["42"]);
    }

    #[test]
    fn test_eval_app7() {
        // let x = 5 in
        // let y = 10 in
        // let f = fun a => x in
        // let z = f 42 in
        // y
        let expr = Expr::Let(
            "x",
            &Expr::Int(5),
            &Expr::Let(
                "y",
                &Expr::Int(10),
                &Expr::Let(
                    "f",
                    &Expr::Fun("a", &Expr::Var(2, "x")),
                    &Expr::Let(
                        "z",
                        &Expr::App(&Expr::Var(0, "f"), &Expr::Int(42)),
                        &Expr::Var(2, "y"),
                    ),
                ),
            ),
        );
        assert_eval(expr, Env::default(), expect!["10"])
    }

    #[test]
    fn test_eval_if_true() {
        let expr = Expr::If(&(Expr::Bool(true)), &Expr::Int(1), &Expr::Int(2));
        assert_eval(expr, Env::default(), expect![["1"]]);
    }

    #[test]
    fn test_eval_if_false() {
        let expr = Expr::If(&Expr::Bool(false), &Expr::Int(1), &Expr::Int(2));
        assert_eval(expr, Env::default(), expect![["2"]]);
    }

    #[test]
    fn test_eval_let() {
        let init = Expr::Int(42);
        let body = Expr::Var(0, "x");
        let expr = Expr::Let("x", &(init), &(body));
        assert_eval(expr, Env::default(), expect![["42"]]);
    }
}
