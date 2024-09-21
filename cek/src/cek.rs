#[derive(Clone, Debug)]
enum Expr {
    LocalVar(usize),

    /// ```text
    /// let <name> = <init> in <body>
    /// ```
    Let {
        name: String,
        init: Box<Self>,
        body: Box<Self>,
    },

    ///```text
    /// fun <param> => <body>
    /// ```
    Fun {
        param: String,
        body: Box<Self>,
    },

    /// ```text
    /// <fun> <arg>
    /// ```
    App {
        fun: Box<Self>,
        arg: Box<Self>,
    },
}

#[derive(Clone, Debug)]
enum Value {
    Int(u32),
    Fun(Closure),
}

type Env = Vec<Value>;

#[derive(Clone, Debug)]
struct Closure {
    env: Env,
    expr: Expr,
}

struct Hole;

enum Cont {
    /// Finished with evaluation.
    Done,

    /// Evaluate the body of a let expression:
    /// ```text
    /// let <name> = <init> in <body>
    /// ```
    LetBody {
        name: String,
        init: Hole,
        body: Expr,
        env: Env,
        cont: Box<Cont>,
    },

    /// Evaluate the argument of a function application:
    /// ```text
    /// <fun> <arg>
    /// ```
    FunArg {
        fun: Hole,
        arg: Expr,
        env: Env,
        cont: Box<Cont>,
    },

    /// Apply a function to an argument:
    /// ```text
    /// <fun> <arg>
    /// ```
    FunApp {
        fun: Value,
        arg: Hole,
        cont: Box<Cont>,
    },
}

enum State {
    /// Evaluate an expression in an environment.
    Eval { expr: Expr, env: Env, cont: Cont },

    /// Return a value.
    Cont { cont: Cont, value: Value },
}

fn step(state: State) -> State {
    match state {
        // Look up local var
        State::Eval {
            expr: Expr::LocalVar(var),
            env,
            cont,
        } => State::Cont {
            cont,
            value: env[var].clone(),
        },

        // Evaluate the initializer of a let expression
        State::Eval {
            expr: Expr::Let { name, init, body },
            env,
            cont,
        } => State::Eval {
            expr: *init,
            env: env.clone(),
            cont: Cont::LetBody {
                name,
                init: Hole,
                body: *body,
                env,
                cont: Box::new(cont),
            },
        },

        // Evaluate a function literal
        State::Eval {
            expr: Expr::Fun { param, body },
            env,
            cont,
        } => State::Cont {
            cont,
            value: Value::Fun(Closure { env, expr: *body }),
        },

        // Evaluate a function application: Evaluate the head of the application
        // first, and continue the argument later
        State::Eval {
            expr: Expr::App { fun, arg },
            env,
            cont,
        } => State::Eval {
            expr: *fun,
            env: env.clone(),
            cont: Cont::FunArg {
                fun: Hole,
                arg: *arg,
                env,
                cont: Box::new(cont),
            },
        },

        State::Cont {
            cont:
                Cont::LetBody {
                    name,
                    init: Hole,
                    body,
                    env,
                    cont,
                },
            value,
        } => State::Eval {
            expr: body,
            env: {
                let mut env = env;
                env.push(value);
                env
            },
            cont: *cont,
        },

        // Evaluate a function argument, now that the head of the
        // application has been evaluated
        State::Cont {
            cont:
                Cont::FunArg {
                    fun: Hole,
                    arg,
                    env,
                    cont,
                },
            value,
        } => State::Eval {
            expr: arg,
            env,
            cont: Cont::FunApp {
                fun: value,
                arg: Hole,
                cont,
            },
        },

        // Apply a function, now that the function and the argument have been evaluated
        State::Cont {
            cont:
                Cont::FunApp {
                    fun,
                    arg: Hole,
                    cont,
                },
            value,
        } => match fun {
            Value::Fun(Closure { env, expr }) => State::Eval {
                expr,
                env: {
                    let mut env = env;
                    env.push(value);
                    env
                },
                cont: *cont,
            },
            _ => panic!("expected function, got {:?}", fun),
        },

        State::Cont {
            cont: Cont::Done, ..
        } => panic!("cannot continue after Done"),
    }
}

fn eval(expr: Expr) -> Value {
    let mut state = State::Eval {
        expr,
        env: vec![],
        cont: Cont::Done,
    };

    loop {
        match state {
            State::Cont {
                cont: Cont::Done,
                value,
            } => return value,
            s => state = step(s),
        }
    }
}
