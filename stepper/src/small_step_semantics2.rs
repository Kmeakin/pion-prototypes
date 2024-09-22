use crate::syntax::*;

#[derive(Debug, Clone)]
enum State<'core> {
    Value(Value<'core>),
    Int(u32),
    Bool(bool),
    Var(usize, &'core str),
    Fun(&'core str, &'core Expr<'core>),

    App0(&'core Expr<'core>, &'core Expr<'core>),
    Let0(&'core str, &'core Expr<'core>, &'core Expr<'core>),

    If0(&'core Expr<'core>, &'core Expr<'core>, &'core Expr<'core>),
}

#[derive(Debug, Clone)]
enum Frame<'core> {
    App1(&'core Expr<'core>),
    App2(Value<'core>),

    Let1(&'core str, &'core Expr<'core>),
    Let2(),

    If1(&'core Expr<'core>, &'core Expr<'core>),
}

impl<'core> State<'core> {
    fn from_expr(expr: Expr<'core>) -> Self {
        match expr {
            Expr::Int(n) => Self::Int(n),
            Expr::Bool(b) => Self::Bool(b),
            Expr::Var(var, name) => Self::Var(var, name),
            Expr::Fun(name, body) => Self::Fun(name, body),
            Expr::App(fun, arg) => Self::App0(fun, arg),
            Expr::Let(name, init, body) => Self::Let0(name, init, body),
            Expr::If(cond, then, r#else) => Self::If0(cond, then, r#else),
        }
    }
}

fn step<'core>(
    state: State<'core>,
    env: &mut Env<'core>,
    stack: &mut Vec<Frame<'core>>,
) -> State<'core> {
    match state {
        State::Value(value) => match stack.pop() {
            None => State::Value(value),
            Some(frame) => match frame {
                Frame::App1(arg) => {
                    stack.push(Frame::App2(value));
                    State::from_expr(*arg)
                }
                Frame::App2(fun) => match fun {
                    Value::Fun(_name, new_env, body) => {
                        *env = new_env;
                        env.push(value);
                        State::from_expr(*body)
                    }
                    _ => State::Value(Value::Error(Error::CalleeNotFun {})),
                },

                Frame::Let1(_name, body) => {
                    env.push(value);
                    stack.push(Frame::Let2());
                    State::from_expr(*body)
                }
                Frame::Let2() => {
                    env.pop().unwrap();
                    State::Value(value)
                }

                Frame::If1(then, r#else) => match value {
                    Value::Bool(true) => State::from_expr(*then),
                    Value::Bool(false) => State::from_expr(*r#else),
                    _ => State::Value(Value::Error(Error::CondNotBool)),
                },
            },
        },
        State::Int(n) => State::Value(Value::Int(n)),
        State::Bool(b) => State::Value(Value::Bool(b)),
        State::Var(var, name) => State::Value(get_local(env, name, var)),
        State::Fun(name, body) => State::Value(Value::Fun(name, env.clone(), body)),
        State::App0(fun, arg) => {
            stack.push(Frame::App1(arg));
            State::from_expr(*fun)
        }
        State::Let0(name, init, body) => {
            stack.push(Frame::Let1(name, body));
            State::from_expr(*init)
        }
        State::If0(cond, then, r#else) => {
            stack.push(Frame::If1(then, r#else));
            State::from_expr(*cond)
        }
    }
}

pub fn eval<'core>(expr: Expr<'core>, env: &mut Env<'core>) -> Value<'core> {
    let mut stack = Vec::new();
    let mut state = State::from_expr(expr);

    loop {
        match step(state, env, &mut stack) {
            State::Value(value) if stack.is_empty() => return value,
            new_state => state = new_state,
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::*;

    use super::*;

    fn assert_eval<'core>(expr: Expr<'core>, mut env: Env<'core>, expect: Expect) {
        let value = eval(expr, &mut env);
        expect.assert_eq(&format!("{value}"));
    }

    #[test]
    fn test_eval_int() {
        let expr = Expr::Int(42);
        assert_eval(expr, Env::default(), expect!["42"]);
    }

    #[test]
    fn test_eval_var() {
        let expr = Expr::Var(0, "x");
        assert_eval(expr, vec![Value::Int(42)], expect!["42"]);
    }

    #[test]
    fn test_eval_fun() {
        let expr = Expr::Fun("x", &Expr::Var(0, "x"));
        assert_eval(expr, Env::default(), expect!["fun x => x"]);
    }

    #[test]
    fn test_eval_app1() {
        // (fun x => x) 42
        let fun = Expr::Fun("x", &Expr::Var(0, "x"));
        let app = Expr::App(&fun, &Expr::Int(42));
        assert_eval(app, Env::default(), expect!["42"]);
    }

    #[test]
    fn test_eval_app2() {
        // (fun x y => x) 42
        let fun = Expr::Fun("x", &Expr::Fun("y", &Expr::Var(1, "x")));
        let app = Expr::App(&fun, &Expr::Int(42));
        assert_eval(app, Env::default(), expect!["fun y => x"]);
    }

    #[test]
    fn test_eval_app3() {
        // (fun x y => x) 42 99
        let fun = Expr::Fun("x", &Expr::Fun("y", &Expr::Var(1, "x")));
        let app = Expr::App(&fun, &Expr::Int(42));
        let app = Expr::App(&app, &Expr::Int(99));
        assert_eval(app, Env::default(), expect!["42"]);
    }

    #[test]
    fn test_eval_app4() {
        // let f = fun x y => x in f 42 99
        let fun = Expr::Fun("x", &Expr::Fun("y", &Expr::Var(1, "x")));
        let app = Expr::App(&Expr::Var(0, "f"), &Expr::Int(42));
        let app = Expr::App(&app, &Expr::Int(99));
        let expr = Expr::Let("f", &fun, &app);
        assert_eval(expr, Env::default(), expect!["42"]);
    }

    #[test]
    fn test_eval_app5() {
        // (let f = fun x => x in f) 99
        let fun = Expr::Fun("x", &Expr::Var(0, "x"));
        let r#let = Expr::Let("f", &fun, &Expr::Var(0, "f"));
        let expr = Expr::App(&r#let, &Expr::Int(99));
        assert_eval(expr, Env::default(), expect!["99"]);
    }

    #[test]
    fn test_eval_app6() {
        // (let f = fun x => x in f) (let a = 99 in a)
        let fun = Expr::Fun("x", &Expr::Var(0, "x"));
        let r#let1 = Expr::Let("f", &fun, &Expr::Var(0, "f"));
        let r#let2 = Expr::Let("a", &Expr::Int(99), &Expr::Var(0, "a"));
        let expr = Expr::App(&r#let1, &let2);
        assert_eval(expr, Env::default(), expect!["99"])
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
        assert_eval(expr, Env::default(), expect!["1"]);
    }

    #[test]
    fn test_eval_if_false() {
        let expr = Expr::If(&Expr::Bool(false), &Expr::Int(1), &Expr::Int(2));
        assert_eval(expr, Env::default(), expect!["2"]);
    }
}
