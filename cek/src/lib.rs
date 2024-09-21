use ecow::EcoVec;

pub type LocalEnv<'core> = EcoVec<Value<'core>>;

#[track_caller]
fn get_local<'core>(env: &LocalEnv<'core>, var: usize) -> Value<'core> {
    let absolute = env.len() - var - 1;
    dbg!(absolute);
    match env.get(absolute) {
        None => panic!("Unbound local var: {var:?}"),
        Some(value) => value.clone(),
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'core> {
    Int(u32),
    LocalVar(usize),
    Let {
        init: &'core Self,
        body: &'core Self,
    },
    Fun {
        body: &'core Self,
    },
    App {
        callee: &'core Self,
        arg: &'core Self,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'core> {
    Int(u32),
    Fun(Closure<'core>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure<'core> {
    env: LocalEnv<'core>,
    body: Expr<'core>,
}

impl<'core> Closure<'core> {
    pub fn new(env: LocalEnv<'core>, body: Expr<'core>) -> Self { Self { env, body } }
}

macro_rules! tests {
    ($test:expr) => {
        #[cfg(test)]
        mod tests {
            use super::*;

            #[track_caller]
            fn assert_eval<'core>(expr: Expr<'core>, expected: Value<'core>) {
                let mut local_env = LocalEnv::new();
                let result = $test(&expr, &mut local_env);
                assert_eq!(result, expected);
            }

            #[test]
            fn eval_int() {
                let expr = Expr::Int(42);
                assert_eval(expr, Value::Int(42));
            }

            #[test]
            fn eval_let() {
                let expr = Expr::Let {
                    init: &Expr::Int(42),
                    body: &Expr::LocalVar(0),
                };
                assert_eval(expr, Value::Int(42));
            }

            #[test]
            fn eval_fun_lit() {
                let expr = Expr::Fun {
                    body: &Expr::Int(10),
                };
                assert_eval(
                    expr,
                    Value::Fun(Closure::new(LocalEnv::new(), Expr::Int(10))),
                );
            }

            #[test]
            fn eval_fun_app() {
                let expr = Expr::Fun {
                    body: &Expr::LocalVar(0),
                };
                let expr = Expr::App {
                    callee: &expr,
                    arg: &Expr::Int(42),
                };
                assert_eval(expr, Value::Int(42));
            }

            #[test]
            fn eval_fun_app2() {
                // fun x => fun y => x
                let fun = Expr::Fun {
                    body: &Expr::Fun {
                        body: &Expr::LocalVar(1),
                    },
                };
                let expr = Expr::App {
                    callee: &Expr::App {
                        callee: &fun,
                        arg: &Expr::Int(0),
                    },
                    arg: &Expr::Int(99),
                };
                assert_eval(expr, Value::Int(0));
            }
        }
    };
}

pub mod normal {
    use super::*;

    pub fn eval<'core>(expr: &Expr<'core>, local_env: &mut LocalEnv<'core>) -> Value<'core> {
        match expr {
            Expr::Int(n) => Value::Int(*n),
            Expr::LocalVar(var) => get_local(dbg!(local_env), *var),
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
                    _ => panic!("Cannot apply non-function: {callee:?}"),
                }
            }
        }
    }

    tests!(eval);
}

pub mod coro {
    use std::pin::Pin;

    use futures::executor::block_on;
    use futures::Future;

    use super::*;

    pub fn eval<'core>(expr: &Expr<'core>, local_env: &mut LocalEnv<'core>) -> Value<'core> {
        block_on(step(*expr, local_env.clone()))
    }

    fn step<'core>(
        expr: Expr<'core>,
        mut env: LocalEnv<'core>,
    ) -> Pin<Box<dyn Future<Output = Value<'core>> + 'core>> {
        Box::pin(async move {
            match expr {
                Expr::Int(n) => Value::Int(n),
                Expr::LocalVar(var) => get_local(&env, var),
                Expr::Let { init, body } => {
                    let init = step(*init, env.clone()).await;
                    env.push(init);
                    let body = step(*body, env.clone()).await;
                    env.pop();
                    body
                }
                Expr::Fun { body } => Value::Fun(Closure::new(env.clone(), *body)),
                Expr::App { callee, arg } => {
                    let callee = step(*callee, env.clone()).await;
                    let arg = step(*arg, env).await;
                    match callee {
                        Value::Fun(mut closure) => {
                            let mut env = std::mem::take(&mut closure.env);
                            env.push(arg);
                            step(closure.body, env).await
                        }
                        _ => panic!("Cannot apply non-function: {callee:?}"),
                    }
                }
            }
        })
    }

    tests!(eval);
}

pub mod cek {
    use super::*;

    #[derive(Debug, Clone)]
    enum State<'core> {
        /// A compoind node in the syntax tree was reached (ie evaluation takes
        /// multiple steps).
        /// Evaluate `expr` in `env` and then continue with `cont`.
        Node {
            expr: Expr<'core>,
            env: LocalEnv<'core>,
            cont: Cont<'core>,
        },

        /// A "leaf" node in the syntax was reached (ie a form that can be
        /// evaluated in a single step, like a variable or function literal).
        /// Pass `value` on to `cont`.
        Leaf {
            value: Value<'core>,
            cont: Cont<'core>,
        },
    }

    #[derive(Debug, Clone)]
    enum Cont<'core> {
        Done,
        LetBody((), Expr<'core>, LocalEnv<'core>, Box<Self>),
        FunArg((), Expr<'core>, LocalEnv<'core>, Box<Self>),
        FunApp(Value<'core>, (), Box<Self>),
    }

    fn step<'core>(state: State<'core>) -> State<'core> {
        match state {
            State::Node { expr, env, cont } => match expr {
                Expr::Int(n) => State::Leaf {
                    cont,
                    value: Value::Int(n),
                },

                // Look up local var:
                Expr::LocalVar(var) => State::Leaf {
                    cont,
                    value: get_local(&env, var),
                },

                // Evaluate let initializer and continue with the body later:
                Expr::Let { init, body } => State::Node {
                    expr: *init,
                    env: env.clone(),
                    cont: Cont::LetBody((), *body, env, Box::new(cont)),
                },

                // Evaluate a function literal:
                Expr::Fun { body } => {
                    let value = Value::Fun(Closure::new(env.clone(), *body));
                    State::Leaf { cont, value }
                }

                // Evaluate the callee of a function application, and continue with the argument
                // later:
                Expr::App { callee, arg } => State::Node {
                    expr: *callee,
                    env: env.clone(),
                    cont: Cont::FunArg((), *arg, env, Box::new(cont)),
                },
            },
            State::Leaf { cont, value } => match cont {
                // Continue with the body of a let expression:
                Cont::LetBody((), body, env, cont) => {
                    let mut env = env.clone();
                    env.push(value);
                    State::Node {
                        expr: body,
                        env,
                        cont: *cont,
                    }
                }

                // Continue evaluating a function argument, now that the head of the application has
                // been evaluated
                Cont::FunArg((), arg, env, cont) => State::Node {
                    expr: arg,
                    env,
                    cont: Cont::FunApp(value, (), cont),
                },

                // Apply a function, now that the callee and the argument have been evaluated:
                Cont::FunApp(callee, (), cont) => match callee {
                    Value::Fun(callee) => {
                        let mut env = callee.env.clone();
                        env.push(value);
                        State::Node {
                            expr: callee.body,
                            env,
                            cont: *cont,
                        }
                    }
                    _ => panic!("Cannot apply non-function: {callee:?}"),
                },
                Cont::Done => panic!("Cannot continue after Done"),
            },
        }
    }

    fn eval<'core>(expr: &Expr<'core>, env: &mut LocalEnv<'core>) -> Value<'core> {
        let mut state = State::Node {
            expr: *expr,
            env: env.clone(),
            cont: Cont::Done,
        };

        loop {
            match step(state) {
                State::Leaf {
                    value,
                    cont: Cont::Done,
                } => return value,
                s => state = s,
            }
        }
    }

    tests!(eval);
}

pub mod cek2 {
    use super::*;

    #[derive(Debug, Clone)]
    enum State<'core> {
        /// A compoind node in the syntax tree was reached (ie evaluation takes
        /// multiple steps).
        /// Evaluate `expr` in `env` and then continue with `cont`.
        Node {
            expr: Expr<'core>,
            env: LocalEnv<'core>,
            stack: Vec<Frame<'core>>,
        },

        /// A "leaf" node in the syntax was reached (ie a form that can be
        /// evaluated in a single step, like a variable or function literal).
        /// Pass `value` on to `cont`.
        Leaf {
            value: Value<'core>,
            stack: Vec<Frame<'core>>,
        },
    }

    #[derive(Debug, Clone)]
    enum Frame<'core> {
        LetBody((), Expr<'core>, LocalEnv<'core>),
        FunArg((), Expr<'core>, LocalEnv<'core>),
        FunApp(Value<'core>, ()),
    }

    fn cons<T>(mut v: Vec<T>, x: T) -> Vec<T> {
        v.push(x);
        v
    }

    enum Step<'core> {
        More(State<'core>),
        Done(Value<'core>),
    }

    fn step(state: State<'_>) -> Step<'_> {
        match state {
            State::Node { expr, env, stack } => match expr {
                Expr::Int(n) => Step::More(State::Leaf {
                    stack,
                    value: Value::Int(n),
                }),

                // Look up local var:
                Expr::LocalVar(var) => Step::More(State::Leaf {
                    stack,
                    value: get_local(&env, var),
                }),

                // Evaluate let initializer and continue with the body later:
                Expr::Let { init, body } => Step::More(State::Node {
                    expr: *init,
                    env: env.clone(),
                    stack: cons(stack, Frame::LetBody((), *body, env)),
                }),

                // Evaluate a function literal:
                Expr::Fun { body } => {
                    let value = Value::Fun(Closure::new(env.clone(), *body));
                    Step::More(State::Leaf { stack, value })
                }

                // Evaluate the callee of a function application, and continue with the argument
                // later:
                Expr::App { callee, arg } => Step::More(State::Node {
                    expr: *callee,
                    env: env.clone(),
                    stack: cons(stack, Frame::FunArg((), *arg, env)),
                }),
            },
            State::Leaf { mut stack, value } => match stack.pop() {
                None => Step::Done(value),
                Some(cont) => match cont {
                    // Continue with the body of a let expression:
                    Frame::LetBody((), body, env) => {
                        let mut env = env.clone();
                        env.push(value);
                        Step::More(State::Node {
                            expr: body,
                            env,
                            stack,
                        })
                    }

                    // Continue evaluating a function argument, now that the head of the application
                    // has been evaluated
                    Frame::FunArg((), arg, env) => Step::More(State::Node {
                        expr: arg,
                        env,
                        stack: cons(stack, Frame::FunApp(value, ())),
                    }),

                    // Apply a function, now that the callee and the argument have been evaluated:
                    Frame::FunApp(callee, ()) => match callee {
                        Value::Fun(callee) => {
                            let mut env = callee.env.clone();
                            env.push(value);
                            Step::More(State::Node {
                                expr: callee.body,
                                env,
                                stack,
                            })
                        }
                        _ => panic!("Cannot apply non-function: {callee:?}"),
                    },
                },
            },
        }
    }

    fn eval<'core>(expr: &Expr<'core>, env: &LocalEnv<'core>) -> Value<'core> {
        let mut state = State::Node {
            expr: *expr,
            env: env.clone(),
            stack: vec![],
        };

        loop {
            match step(state) {
                Step::More(new_state) => state = new_state,
                Step::Done(value) => return value,
            }
        }
    }

    tests!(eval);
}
