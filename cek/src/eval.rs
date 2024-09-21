use ecow::EcoVec;

pub fn eval<'env, 'core>(expr: &Expr<'core>, local_env: &'env mut LocalEnv<'core>) -> Value<'core> {
    match expr {
        Expr::LocalVar(var) => match local_env.get(*var) {
            None => panic!("Unbound local var: {var:?}"),
            Some(value) => value.clone(),
        },
        Expr::Let { init, body } => {
            let init = eval(init, local_env);
            local_env.push(init);
            let body = eval(body, local_env);
            local_env.pop();
            body
        }
        Expr::Fun { body } => Value::Fun(Closure::new(local_env.clone(), **body)),
        Expr::App { callee, arg } => {
            let callee = eval(callee, local_env);
            let arg = eval(arg, local_env);
            match callee {
                Value::Fun(closure) => {
                    let mut new_local_env = closure.env.clone();
                    new_local_env.push(arg);
                    eval(&closure.body, &mut new_local_env)
                }
            }
        }
    }
}

pub async fn eval_coroutine<'env, 'core>(
    expr: &Expr<'core>,
    local_env: &'env mut LocalEnv<'core>,
) -> Value<'core> {
    match expr {
        Expr::LocalVar(var) => match local_env.get(*var) {
            None => panic!("Unbound local var: {var:?}"),
            Some(value) => value.clone(),
        },
        Expr::Let { init, body } => {
            let init = eval_coroutine(init, local_env).await;
            local_env.push(init);
            let body = eval(body, local_env);
            local_env.pop();
            body
        }
        Expr::Fun { body } => Value::Fun(Closure::new(local_env.clone(), **body)),
        Expr::App { callee, arg } => {
            let callee = eval_coroutine(callee, local_env).await;
            let arg = eval_coroutine(arg, local_env).await;
            match callee {
                Value::Fun(closure) => {
                    let mut new_local_env = closure.env.clone();
                    new_local_env.push(arg);
                    eval(&closure.body, &mut new_local_env)
                }
            }
        }
    }
}
