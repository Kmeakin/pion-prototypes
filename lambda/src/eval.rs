use crate::syntax::*;

#[track_caller]
fn get_local<'core>(env: &Env<'core>, var: usize, name: &str) -> Value<'core> {
    let len = env.len();
    let index = len.checked_sub(var).and_then(|index| index.checked_sub(1));

    match index.and_then(|index| env.get(index)) {
        Some(value) => value.clone(),
        None => panic!("Unbound variable `{name}` (index {var} in environment of length {len})"),
    }
}

pub mod big_step {
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
        fn test_eval_fun() {
            assert_eval(&Expr::Fun("x", &Expr::Var(1, "x")), expect!["fun x => x"]);
        }

        #[test]
        fn test_eval_app() {
            assert_eval(
                &Expr::App(&Expr::Fun("x", &Expr::Var(0, "x")), &(Expr::Int(42))),
                expect!["42"],
            );
        }
    }
}

pub mod small_step {
    use super::*;

    pub enum Control<'core> {
        Expr(Expr<'core>),
        Value(Value<'core>),
    }

    pub enum Frame<'core> {
        App1(&'core Expr<'core>),
        App2(Value<'core>),
        App3(Env<'core>),

        Binop1(Binop, &'core Expr<'core>),
        Binop2(Binop, Value<'core>),

        Let1(&'core str, &'core Expr<'core>),
        Let2,

        If1(&'core Expr<'core>, &'core Expr<'core>),
    }

    pub fn step<'core>(
        control: Control<'core>,
        env: &mut Env<'core>,
        kont: &mut Vec<Frame<'core>>,
    ) -> Control<'core> {
        match control {
            Control::Expr(Expr::Int(n)) => Control::Value(Value::Int(n)),
            Control::Expr(Expr::Bool(b)) => Control::Value(Value::Bool(b)),
            Control::Expr(Expr::Var(var, name)) => Control::Value(get_local(env, var, name)),
            Control::Expr(Expr::Fun(name, body)) => {
                Control::Value(Value::Fun(name, env.clone(), body))
            }
            Control::Expr(Expr::App(fun, arg)) => {
                kont.push(Frame::App1(arg));
                Control::Expr(*fun)
            }
            Control::Expr(Expr::Binop(op, lhs, rhs)) => {
                kont.push(Frame::Binop1(op, rhs));
                Control::Expr(*lhs)
            }
            Control::Expr(Expr::Let(name, init, body)) => {
                kont.push(Frame::Let1(name, body));
                Control::Expr(*init)
            }
            Control::Expr(Expr::If(cond, then, r#else)) => {
                kont.push(Frame::If1(then, r#else));
                Control::Expr(*cond)
            }

            Control::Value(value) => match kont.pop() {
                None => Control::Value(value),
                Some(Frame::App1(arg)) => {
                    kont.push(Frame::App2(value));
                    Control::Expr(*arg)
                }
                Some(Frame::App2(fun)) => match fun {
                    Value::Fun(_name, new_env, body) => {
                        let old_env = std::mem::take(env);
                        kont.push(Frame::App3(old_env));

                        *env = new_env;
                        env.push(value);
                        Control::Expr(*body)
                    }
                    _ => panic!("Expected function, got {fun}"),
                },
                Some(Frame::App3(old_env)) => {
                    *env = old_env;
                    Control::Value(value)
                }
                Some(Frame::Binop1(op, rhs)) => {
                    kont.push(Frame::Binop2(op, value));
                    Control::Expr(*rhs)
                }
                Some(Frame::Binop2(op, lhs)) => {
                    let rhs = value;
                    let (Value::Int(lhs), Value::Int(rhs)) = (&lhs, &rhs) else {
                        panic!("Expected integers, got {lhs} and {rhs}");
                    };
                    let (lhs, rhs) = (*lhs, *rhs);
                    Control::Value(match op {
                        Binop::Add => Value::Int(u32::wrapping_add(lhs, rhs)),
                        Binop::Sub => Value::Int(u32::wrapping_sub(lhs, rhs)),
                        Binop::Mul => Value::Int(u32::wrapping_mul(lhs, rhs)),
                        Binop::Eq => Value::Bool(u32::eq(&lhs, &rhs)),
                        Binop::Ne => Value::Bool(u32::ne(&lhs, &rhs)),
                        Binop::Le => Value::Bool(u32::le(&lhs, &rhs)),
                        Binop::Ge => Value::Bool(u32::ge(&lhs, &rhs)),
                        Binop::Lt => Value::Bool(u32::lt(&lhs, &rhs)),
                        Binop::Gt => Value::Bool(u32::gt(&lhs, &rhs)),
                    })
                }

                Some(Frame::Let1(_name, body)) => {
                    env.push(value);
                    kont.push(Frame::Let2);
                    Control::Expr(*body)
                }
                Some(Frame::Let2) => {
                    env.pop();
                    Control::Value(value)
                }

                Some(Frame::If1(then, r#else)) => match value {
                    Value::Bool(true) => Control::Expr(*then),
                    Value::Bool(false) => Control::Expr(*r#else),
                    _ => panic!("Expected boolean, got {value}"),
                },
            },
        }
    }

    pub fn eval<'core>(expr: &Expr<'core>, env: &mut Env<'core>) -> Value<'core> {
        let mut control = Control::Expr(*expr);
        let mut kont = Vec::new();

        loop {
            match step(control, env, &mut kont) {
                Control::Value(value) if kont.is_empty() => return value,
                c => control = c,
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
        fn test_eval_fun() {
            assert_eval(&Expr::Fun("x", &Expr::Var(1, "x")), expect!["fun x => x"]);
        }

        #[test]
        fn test_eval_app() {
            assert_eval(
                &Expr::App(&Expr::Fun("x", &Expr::Var(0, "x")), &(Expr::Int(42))),
                expect!["42"],
            );
        }
    }
}
