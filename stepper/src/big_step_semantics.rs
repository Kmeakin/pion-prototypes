use crate::syntax::*;

fn get_var<'core>(env: &Env<'core>, var: usize) -> Value<'core> {
    let index = env.len() - var - 1;
    match env.get(index) {
        None => panic!("Unbound local variable: {var:?} in len {}", env.len()),
        Some(value) => value.clone(),
    }
}

/// Big step semantics
pub fn eval<'core>(expr: &Expr<'core>, env: &mut Env<'core>) -> Value<'core> {
    match expr {
        Expr::Int(n) => Value::Int(*n),
        Expr::Bool(b) => Value::Bool(*b),
        Expr::Var(var) => get_var(env, *var),
        Expr::Fun(name, body) => Value::Fun(name, env.clone(), body),
        Expr::App(fun, arg) => {
            let fun = eval(fun, env);
            let arg = eval(arg, env);
            match fun {
                Value::Fun(_, mut env, body) => {
                    env.push(arg);
                    eval(body, &mut env)
                }
                _ => panic!("Invalid function application: {fun:?}"),
            }
        }
        Expr::If(cond, then, r#else) => {
            let cond = eval(cond, env);
            match cond {
                Value::Bool(true) => eval(then, env),
                Value::Bool(false) => eval(r#else, env),
                _ => panic!("Invalid if condition: {cond:?}"),
            }
        }
        Expr::Let(_name, init, body) => {
            let init = eval(init, env);
            env.push(init);
            let body = eval(body, env);
            env.pop();
            body
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::*;

    use super::*;

    fn assert_eval<'core>(expr: Expr<'core>, mut env: Env<'core>, expect: Expect) {
        let value = eval(&expr, &mut env);
        expect.assert_eq(&format!("{:?}", value));
    }

    #[test]
    fn test_eval_int() {
        let expr = Expr::Int(42);
        assert_eval(expr, Env::default(), expect![[r"42"]]);
    }

    #[test]
    fn test_eval_var() {
        let expr = Expr::Var(0);
        assert_eval(expr, vec![Value::Int(42)], expect![["42"]]);
    }

    #[test]
    fn test_eval_fun() {
        let expr = Expr::Fun("x", &Expr::Var(0));
        assert_eval(expr, Env::default(), expect![["fun x => $0"]]);
    }

    #[test]
    fn test_eval_app() {
        let expr = Expr::App(&Expr::Fun("x", &Expr::Var(0)), &Expr::Int(42));
        assert_eval(expr, Env::default(), expect![["42"]]);

        let expr = Expr::App(
            &Expr::Fun("x", &Expr::Fun("y", &Expr::Var(1))),
            &Expr::Int(42),
        );
        assert_eval(expr, Env::default(), expect!["fun y => $1"]);
    }

    #[test]
    fn test_eval_if_true() {
        let expr = Expr::If(&(Expr::Bool(true)), &Expr::Int(1), &Expr::Int(2));
        assert_eval(expr, Env::default(), expect![["1"]]);
    }

    #[test]
    fn test_eval_ifz_false() {
        let expr = Expr::If(&Expr::Bool(false), &Expr::Int(1), &Expr::Int(2));
        assert_eval(expr, Env::default(), expect![["2"]]);
    }

    #[test]
    fn test_eval_let() {
        let init = Expr::Int(42);
        let body = Expr::Var(0);
        let expr = Expr::Let("x", &(init), &(body));
        assert_eval(expr, Env::default(), expect![["42"]]);
    }
}
