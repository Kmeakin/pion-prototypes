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
        Expr::IfZ(cond, then, r#else) => {
            let cond = eval(cond, env);
            match cond {
                Value::Int(0) => eval(then, env),
                Value::Int(_) => eval(r#else, env),
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
#[cfg(FALSE)]
mod tests {
    use super::*;

    fn setup_env() -> Env<'static> {
        vec![]
    }

    #[test]
    fn test_eval_int() {
        let env = &mut setup_env();
        let expr = Expr::Int(42);
        let result = eval(&expr, env);
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_eval_var() {
        let env = &mut vec![Value::Int(42)];
        let expr = Expr::Var(0);
        let result = eval(&expr, env);
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_eval_fun() {
        let env = &mut setup_env();
        let expr = Expr::Fun("x", Box::new(Expr::Var(0)));
        let result = eval(&expr, env);
        match result {
            Value::Fun(name, _, _) => assert_eq!(name, "x"),
            _ => panic!("Expected function value"),
        }
    }

    #[test]
    fn test_eval_app() {
        let env = &mut setup_env();
        let fun = Expr::Fun("x", Box::new(Expr::Var(0)));
        let arg = Expr::Int(42);
        let expr = Expr::App(Box::new(fun), Box::new(arg));
        let result = eval(&expr, env);
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_eval_ifz_true() {
        let env = &mut setup_env();
        let cond = Expr::Int(0);
        let then = Expr::Int(1);
        let r#else = Expr::Int(2);
        let expr = Expr::IfZ(Box::new(cond), Box::new(then), Box::new(r#else));
        let result = eval(&expr, env);
        assert_eq!(result, Value::Int(1));
    }

    #[test]
    fn test_eval_ifz_false() {
        let env = &mut setup_env();
        let cond = Expr::Int(1);
        let then = Expr::Int(1);
        let r#else = Expr::Int(2);
        let expr = Expr::IfZ(Box::new(cond), Box::new(then), Box::new(r#else));
        let result = eval(&expr, env);
        assert_eq!(result, Value::Int(2));
    }

    #[test]
    fn test_eval_let() {
        let env = &mut setup_env();
        let init = Expr::Int(42);
        let body = Expr::Var(0);
        let expr = Expr::Let("x", Box::new(init), Box::new(body));
        let result = eval(&expr, env);
        assert_eq!(result, Value::Int(42));
    }
}
