// Adapted from
// https://hackage.haskell.org/package/swarm-0.6.0.0/docs/src/Swarm.Game.CESK.html,
// with the Store removed
use crate::syntax::{Expr, Value, *};

/// A single component of a continuation stack, explaining what to do next after
/// we finish evaluating the currently focused expression.
#[derive(Clone, Debug)]
pub enum Frame<'core> {
    /// `Arg1 { arg, env }` says that we were evaluating the left-hand side of
    /// an application, so the next thing we should do is evaluate the
    /// expression `arg` (the right-hand side, i.e argument of the application)
    /// in environment `eenv`. We will also push an `App` frame on the stack.
    App1 { arg: Expr<'core>, env: Env<'core> },

    /// `App2 { fun }` says that we were evaluating the right-hand side of an
    /// application; once we are done, we should pass the resulting value as an
    /// argument to `fun`.
    App2 { fun: Value<'core> },

    /// `Let1 { name, body, env }` says that we were evaluating an
    /// expression `init` in an expression of the form `let name = init in
    /// body`, that is, we were evaluating the definition of `name`; the
    /// next thing we should do is evaluate `body` in the environment `env`
    /// extended with a binding for `name`.
    Let1 {
        name: &'core str,
        body: Expr<'core>,
        env: Env<'core>,
    },

    If1 {
        then: Expr<'core>,
        r#else: Expr<'core>,
        env: Env<'core>,
    },
}

pub type Cont<'core> = Vec<Frame<'core>>;

#[derive(Clone, Debug)]
pub enum State<'core, K = Cont<'core>> {
    /// When we are on our way "in/down" into an expression, we have a currently
    /// focused expression to evaluate in the environment, and a continuation.
    /// In this mode we generally pattern-match on the `Expr` to decide what to
    /// do next.
    In(Expr<'core>, Env<'core>, Cont<'core>),

    /// Once we finish evaluating a term, we end up with a `Value` and we switch
    /// into "out" mode, bringing the value back up out of the depths to the
    /// context that was expecting it. In this mode we generally pattern-match
    /// on the `Cont` to decide what to do next.
    ///
    /// Note that there is no `Env`, because we don't have anything with
    /// variables to evaluate at the moment, and we maintain the invariant that
    /// any unevaluated terms buried inside a `Value` or `Cont` must carry along
    /// their environment with them.
    Out(Value<'core>, K),
}

impl<'core> State<'core> {
    pub fn new(expr: Expr<'core>, env: Env<'core>) -> Self { Self::In(expr, env, Vec::new()) }

    /// Workaround for the inability to pattern match on `Vec`
    fn split(self) -> State<'core, Option<(Frame<'core>, Cont<'core>)>> {
        match self {
            State::In(expr, env, cont) => State::In(expr, env, cont),
            State::Out(value, mut cont) => match cont.pop() {
                None => State::Out(value, None),
                Some(frame) => State::Out(value, Some((frame, cont))),
            },
        }
    }
}

fn push_frame<'core>(mut kont: Cont<'core>, frame: Frame<'core>) -> Cont<'core> {
    kont.push(frame);
    kont
}

fn push_value<'core>(mut env: Env<'core>, value: Value<'core>) -> Env<'core> {
    env.push(value);
    env
}

#[rustfmt::skip]
pub fn step(state: State) -> State {
    match state.split() {
        State::Out(value, None) => State::Out(value, Cont::new()),

        State::In(Expr::Bool(b),         _,   kont) => State::Out(Value::Bool(b),              kont),
        State::In(Expr::Int(n),          _,   kont) => State::Out(Value::Int(n),               kont),
        State::In(Expr::Var(var, name),        env, kont) => State::Out(get_local(&env, name,var),          kont),
        State::In(Expr::Fun(name, body), env, kont) => State::Out(Value::Fun(name, env, body), kont),

        State::In(Expr::App(fun, arg), env, kont) => State::In(*fun, env.clone(), push_frame(kont, Frame::App1 { arg: *arg, env })),
        State::Out(fun, Some((Frame::App1 { arg, env}, kont))) => State::In(arg, env, push_frame(kont, Frame::App2 { fun })),
        State::Out(arg, Some((Frame::App2 { fun: Value::Fun(_, env, body) }, kont))) => State::In(*body, push_value(env, arg), kont),
        State::Out(_,   Some((Frame::App2 { .. },                          kont))) => State::Out(Value::Error(Error::CalleeNotFun {}), kont),

        State::In(Expr::Let(name, init, body),              env,   kont)   => State::In(*init, env.clone(),           push_frame(kont, Frame::Let1 { name, body: *body, env })),
        State::Out(init, Some((Frame::Let1 { name: _, body, env }, kont))) => State::In(body,  push_value(env, init), kont),

        State::In(Expr::If(cond, then, r#else),                            env,   kont)   => State::In(*cond, env.clone(), push_frame(kont, Frame::If1 { then: *then, r#else: *r#else, env })),
        State::Out(Value::Bool(true),  Some((Frame::If1 { then, r#else: _, env }, kont))) => State::In(then,   env, kont),
        State::Out(Value::Bool(false), Some((Frame::If1 { then: _, r#else, env }, kont))) => State::In(r#else, env, kont),
        State::Out(_,       Some((Frame::If1 { .. }, kont)))   => State::Out(Value::Error(Error::CondNotBool), kont),
    }
}

pub fn eval<'core>(expr: Expr<'core>, env: Env<'core>) -> Value<'core> {
    let mut state = State::new(expr, env);

    loop {
        state = step(state);
        match state {
            State::Out(value, cont) if cont.is_empty() => return value,
            _ => continue,
        }
    }
}

pub fn eval_trace<'core>(expr: Expr<'core>, env: Env<'core>) -> (Value<'core>, String) {
    let mut trace = String::new();
    let mut state = State::new(expr, env);
    trace.push_str(&format!("{state:?}\n"));

    loop {
        state = step(state);
        trace.push_str(&format!("{state:?}\n"));
        match state {
            State::Out(value, cont) if cont.is_empty() => return (value, trace),
            _ => continue,
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::*;

    use super::*;

    fn assert_eval<'core>(expr: Expr<'core>, env: Env<'core>, expect: Expect) {
        let (_value, trace) = eval_trace(expr, env);
        expect.assert_eq(&trace);
    }

    #[test]
    fn test_eval_int() {
        let expr = Expr::Int(42);
        assert_eval(
            expr,
            Env::default(),
            expect![[r#"
                In(Int(42), [], [])
                Out(Int(42), [])
            "#]],
        );
    }

    #[test]
    fn test_eval_var() {
        let expr = Expr::Var(0, "x");
        assert_eval(
            expr,
            vec![Value::Int(42)],
            expect![[r#"
                In(Var(0, "x"), [Int(42)], [])
                Out(Int(42), [])
            "#]],
        );
    }

    #[test]
    fn test_eval_fun() {
        let expr = Expr::Fun("x", &Expr::Var(0, "x"));
        assert_eval(
            expr,
            Env::default(),
            expect![[r#"
                In(Fun("x", Var(0, "x")), [], [])
                Out(Fun("x", [], Var(0, "x")), [])
            "#]],
        );
    }

    #[test]
    fn test_eval_app1() {
        // (fun x => x) 42
        let fun = Expr::Fun("x", &Expr::Var(0, "x"));
        let app = Expr::App(&fun, &Expr::Int(42));
        assert_eval(
            app,
            Env::default(),
            expect![[r#"
                In(App(Fun("x", Var(0, "x")), Int(42)), [], [])
                In(Fun("x", Var(0, "x")), [], [App1 { arg: Int(42), env: [] }])
                Out(Fun("x", [], Var(0, "x")), [App1 { arg: Int(42), env: [] }])
                In(Int(42), [], [App2 { fun: Fun("x", [], Var(0, "x")) }])
                Out(Int(42), [App2 { fun: Fun("x", [], Var(0, "x")) }])
                In(Var(0, "x"), [Int(42)], [])
                Out(Int(42), [])
            "#]],
        );
    }

    #[test]
    fn test_eval_app2() {
        // (fun x y => x) 42
        let fun = Expr::Fun("x", &Expr::Fun("y", &Expr::Var(1, "x")));
        let app = Expr::App(&fun, &Expr::Int(42));
        assert_eval(
            app,
            Env::default(),
            expect![[r#"
                In(App(Fun("x", Fun("y", Var(1, "x"))), Int(42)), [], [])
                In(Fun("x", Fun("y", Var(1, "x"))), [], [App1 { arg: Int(42), env: [] }])
                Out(Fun("x", [], Fun("y", Var(1, "x"))), [App1 { arg: Int(42), env: [] }])
                In(Int(42), [], [App2 { fun: Fun("x", [], Fun("y", Var(1, "x"))) }])
                Out(Int(42), [App2 { fun: Fun("x", [], Fun("y", Var(1, "x"))) }])
                In(Fun("y", Var(1, "x")), [Int(42)], [])
                Out(Fun("y", [Int(42)], Var(1, "x")), [])
            "#]],
        );
    }

    #[test]
    fn test_eval_app3() {
        // (fun x y => x) 42 99
        let fun = Expr::Fun("x", &Expr::Fun("y", &Expr::Var(1, "x")));
        let app = Expr::App(&fun, &Expr::Int(42));
        let app = Expr::App(&app, &Expr::Int(99));
        assert_eval(
            app,
            Env::default(),
            expect![[r#"
                In(App(App(Fun("x", Fun("y", Var(1, "x"))), Int(42)), Int(99)), [], [])
                In(App(Fun("x", Fun("y", Var(1, "x"))), Int(42)), [], [App1 { arg: Int(99), env: [] }])
                In(Fun("x", Fun("y", Var(1, "x"))), [], [App1 { arg: Int(99), env: [] }, App1 { arg: Int(42), env: [] }])
                Out(Fun("x", [], Fun("y", Var(1, "x"))), [App1 { arg: Int(99), env: [] }, App1 { arg: Int(42), env: [] }])
                In(Int(42), [], [App1 { arg: Int(99), env: [] }, App2 { fun: Fun("x", [], Fun("y", Var(1, "x"))) }])
                Out(Int(42), [App1 { arg: Int(99), env: [] }, App2 { fun: Fun("x", [], Fun("y", Var(1, "x"))) }])
                In(Fun("y", Var(1, "x")), [Int(42)], [App1 { arg: Int(99), env: [] }])
                Out(Fun("y", [Int(42)], Var(1, "x")), [App1 { arg: Int(99), env: [] }])
                In(Int(99), [], [App2 { fun: Fun("y", [Int(42)], Var(1, "x")) }])
                Out(Int(99), [App2 { fun: Fun("y", [Int(42)], Var(1, "x")) }])
                In(Var(1, "x"), [Int(42), Int(99)], [])
                Out(Int(42), [])
            "#]],
        );
    }

    #[test]
    fn test_eval_app4() {
        // let f = fun x y => x in f 42 99
        let fun = Expr::Fun("x", &Expr::Fun("y", &Expr::Var(1, "x")));
        let app = Expr::App(&Expr::Var(0, "f"), &Expr::Int(42));
        let app = Expr::App(&app, &Expr::Int(99));
        let expr = Expr::Let("f", &fun, &app);
        assert_eval(
            expr,
            Env::default(),
            expect![[r#"
                In(Let("f", Fun("x", Fun("y", Var(1, "x"))), App(App(Var(0, "f"), Int(42)), Int(99))), [], [])
                In(Fun("x", Fun("y", Var(1, "x"))), [], [Let1 { name: "f", body: App(App(Var(0, "f"), Int(42)), Int(99)), env: [] }])
                Out(Fun("x", [], Fun("y", Var(1, "x"))), [Let1 { name: "f", body: App(App(Var(0, "f"), Int(42)), Int(99)), env: [] }])
                In(App(App(Var(0, "f"), Int(42)), Int(99)), [Fun("x", [], Fun("y", Var(1, "x")))], [])
                In(App(Var(0, "f"), Int(42)), [Fun("x", [], Fun("y", Var(1, "x")))], [App1 { arg: Int(99), env: [Fun("x", [], Fun("y", Var(1, "x")))] }])
                In(Var(0, "f"), [Fun("x", [], Fun("y", Var(1, "x")))], [App1 { arg: Int(99), env: [Fun("x", [], Fun("y", Var(1, "x")))] }, App1 { arg: Int(42), env: [Fun("x", [], Fun("y", Var(1, "x")))] }])
                Out(Fun("x", [], Fun("y", Var(1, "x"))), [App1 { arg: Int(99), env: [Fun("x", [], Fun("y", Var(1, "x")))] }, App1 { arg: Int(42), env: [Fun("x", [], Fun("y", Var(1, "x")))] }])
                In(Int(42), [Fun("x", [], Fun("y", Var(1, "x")))], [App1 { arg: Int(99), env: [Fun("x", [], Fun("y", Var(1, "x")))] }, App2 { fun: Fun("x", [], Fun("y", Var(1, "x"))) }])
                Out(Int(42), [App1 { arg: Int(99), env: [Fun("x", [], Fun("y", Var(1, "x")))] }, App2 { fun: Fun("x", [], Fun("y", Var(1, "x"))) }])
                In(Fun("y", Var(1, "x")), [Int(42)], [App1 { arg: Int(99), env: [Fun("x", [], Fun("y", Var(1, "x")))] }])
                Out(Fun("y", [Int(42)], Var(1, "x")), [App1 { arg: Int(99), env: [Fun("x", [], Fun("y", Var(1, "x")))] }])
                In(Int(99), [Fun("x", [], Fun("y", Var(1, "x")))], [App2 { fun: Fun("y", [Int(42)], Var(1, "x")) }])
                Out(Int(99), [App2 { fun: Fun("y", [Int(42)], Var(1, "x")) }])
                In(Var(1, "x"), [Int(42), Int(99)], [])
                Out(Int(42), [])
            "#]],
        );
    }

    #[test]
    fn test_eval_app5() {
        // (let f = fun x => x in f) 99
        let fun = Expr::Fun("x", &Expr::Var(0, "x"));
        let r#let = Expr::Let("f", &fun, &Expr::Var(0, "f"));
        let expr = Expr::App(&r#let, &Expr::Int(99));

        assert_eval(
            expr,
            Env::default(),
            expect![[r#"
                In(App(Let("f", Fun("x", Var(0, "x")), Var(0, "f")), Int(99)), [], [])
                In(Let("f", Fun("x", Var(0, "x")), Var(0, "f")), [], [App1 { arg: Int(99), env: [] }])
                In(Fun("x", Var(0, "x")), [], [App1 { arg: Int(99), env: [] }, Let1 { name: "f", body: Var(0, "f"), env: [] }])
                Out(Fun("x", [], Var(0, "x")), [App1 { arg: Int(99), env: [] }, Let1 { name: "f", body: Var(0, "f"), env: [] }])
                In(Var(0, "f"), [Fun("x", [], Var(0, "x"))], [App1 { arg: Int(99), env: [] }])
                Out(Fun("x", [], Var(0, "x")), [App1 { arg: Int(99), env: [] }])
                In(Int(99), [], [App2 { fun: Fun("x", [], Var(0, "x")) }])
                Out(Int(99), [App2 { fun: Fun("x", [], Var(0, "x")) }])
                In(Var(0, "x"), [Int(99)], [])
                Out(Int(99), [])
            "#]],
        );
    }

    #[test]
    fn test_eval_app6() {
        // (let f = fun x => x in f) (let a = 99 in a)
        let fun = Expr::Fun("x", &Expr::Var(0, "x"));
        let r#let1 = Expr::Let("f", &fun, &Expr::Var(0, "f"));
        let r#let2 = Expr::Let("a", &Expr::Int(99), &Expr::Var(0, "a"));
        let expr = Expr::App(&r#let1, &let2);
        assert_eval(
            expr,
            Env::default(),
            expect![[r#"
                In(App(Let("f", Fun("x", Var(0, "x")), Var(0, "f")), Let("a", Int(99), Var(0, "a"))), [], [])
                In(Let("f", Fun("x", Var(0, "x")), Var(0, "f")), [], [App1 { arg: Let("a", Int(99), Var(0, "a")), env: [] }])
                In(Fun("x", Var(0, "x")), [], [App1 { arg: Let("a", Int(99), Var(0, "a")), env: [] }, Let1 { name: "f", body: Var(0, "f"), env: [] }])
                Out(Fun("x", [], Var(0, "x")), [App1 { arg: Let("a", Int(99), Var(0, "a")), env: [] }, Let1 { name: "f", body: Var(0, "f"), env: [] }])
                In(Var(0, "f"), [Fun("x", [], Var(0, "x"))], [App1 { arg: Let("a", Int(99), Var(0, "a")), env: [] }])
                Out(Fun("x", [], Var(0, "x")), [App1 { arg: Let("a", Int(99), Var(0, "a")), env: [] }])
                In(Let("a", Int(99), Var(0, "a")), [], [App2 { fun: Fun("x", [], Var(0, "x")) }])
                In(Int(99), [], [App2 { fun: Fun("x", [], Var(0, "x")) }, Let1 { name: "a", body: Var(0, "a"), env: [] }])
                Out(Int(99), [App2 { fun: Fun("x", [], Var(0, "x")) }, Let1 { name: "a", body: Var(0, "a"), env: [] }])
                In(Var(0, "a"), [Int(99)], [App2 { fun: Fun("x", [], Var(0, "x")) }])
                Out(Int(99), [App2 { fun: Fun("x", [], Var(0, "x")) }])
                In(Var(0, "x"), [Int(99)], [])
                Out(Int(99), [])
            "#]],
        )
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
        assert_eval(
            expr,
            Env::default(),
            expect![[r#"
                In(Let("x", Int(5), Let("y", Int(10), Let("f", Fun("a", Var(2, "x")), Let("z", App(Var(0, "f"), Int(42)), Var(2, "y"))))), [], [])
                In(Int(5), [], [Let1 { name: "x", body: Let("y", Int(10), Let("f", Fun("a", Var(2, "x")), Let("z", App(Var(0, "f"), Int(42)), Var(2, "y")))), env: [] }])
                Out(Int(5), [Let1 { name: "x", body: Let("y", Int(10), Let("f", Fun("a", Var(2, "x")), Let("z", App(Var(0, "f"), Int(42)), Var(2, "y")))), env: [] }])
                In(Let("y", Int(10), Let("f", Fun("a", Var(2, "x")), Let("z", App(Var(0, "f"), Int(42)), Var(2, "y")))), [Int(5)], [])
                In(Int(10), [Int(5)], [Let1 { name: "y", body: Let("f", Fun("a", Var(2, "x")), Let("z", App(Var(0, "f"), Int(42)), Var(2, "y"))), env: [Int(5)] }])
                Out(Int(10), [Let1 { name: "y", body: Let("f", Fun("a", Var(2, "x")), Let("z", App(Var(0, "f"), Int(42)), Var(2, "y"))), env: [Int(5)] }])
                In(Let("f", Fun("a", Var(2, "x")), Let("z", App(Var(0, "f"), Int(42)), Var(2, "y"))), [Int(5), Int(10)], [])
                In(Fun("a", Var(2, "x")), [Int(5), Int(10)], [Let1 { name: "f", body: Let("z", App(Var(0, "f"), Int(42)), Var(2, "y")), env: [Int(5), Int(10)] }])
                Out(Fun("a", [Int(5), Int(10)], Var(2, "x")), [Let1 { name: "f", body: Let("z", App(Var(0, "f"), Int(42)), Var(2, "y")), env: [Int(5), Int(10)] }])
                In(Let("z", App(Var(0, "f"), Int(42)), Var(2, "y")), [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x"))], [])
                In(App(Var(0, "f"), Int(42)), [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x"))], [Let1 { name: "z", body: Var(2, "y"), env: [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x"))] }])
                In(Var(0, "f"), [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x"))], [Let1 { name: "z", body: Var(2, "y"), env: [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x"))] }, App1 { arg: Int(42), env: [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x"))] }])
                Out(Fun("a", [Int(5), Int(10)], Var(2, "x")), [Let1 { name: "z", body: Var(2, "y"), env: [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x"))] }, App1 { arg: Int(42), env: [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x"))] }])
                In(Int(42), [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x"))], [Let1 { name: "z", body: Var(2, "y"), env: [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x"))] }, App2 { fun: Fun("a", [Int(5), Int(10)], Var(2, "x")) }])
                Out(Int(42), [Let1 { name: "z", body: Var(2, "y"), env: [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x"))] }, App2 { fun: Fun("a", [Int(5), Int(10)], Var(2, "x")) }])
                In(Var(2, "x"), [Int(5), Int(10), Int(42)], [Let1 { name: "z", body: Var(2, "y"), env: [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x"))] }])
                Out(Int(5), [Let1 { name: "z", body: Var(2, "y"), env: [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x"))] }])
                In(Var(2, "y"), [Int(5), Int(10), Fun("a", [Int(5), Int(10)], Var(2, "x")), Int(5)], [])
                Out(Int(10), [])
            "#]],
        )
    }

    #[test]
    fn test_eval_if_true() {
        let expr = Expr::If(&(Expr::Bool(true)), &Expr::Int(1), &Expr::Int(2));
        assert_eval(
            expr,
            Env::default(),
            expect![[r#"
                In(If(Bool(true), Int(1), Int(2)), [], [])
                In(Bool(true), [], [If1 { then: Int(1), else: Int(2), env: [] }])
                Out(Bool(true), [If1 { then: Int(1), else: Int(2), env: [] }])
                In(Int(1), [], [])
                Out(Int(1), [])
            "#]],
        );
    }

    #[test]
    fn test_eval_if_false() {
        let expr = Expr::If(&Expr::Bool(false), &Expr::Int(1), &Expr::Int(2));
        assert_eval(
            expr,
            Env::default(),
            expect![[r#"
                In(If(Bool(false), Int(1), Int(2)), [], [])
                In(Bool(false), [], [If1 { then: Int(1), else: Int(2), env: [] }])
                Out(Bool(false), [If1 { then: Int(1), else: Int(2), env: [] }])
                In(Int(2), [], [])
                Out(Int(2), [])
            "#]],
        );
    }

    #[test]
    fn test_eval_let() {
        let expr = Expr::Let("x", &Expr::Int(42), &Expr::Var(0, "x"));
        assert_eval(
            expr,
            Env::default(),
            expect![[r#"
                In(Let("x", Int(42), Var(0, "x")), [], [])
                In(Int(42), [], [Let1 { name: "x", body: Var(0, "x"), env: [] }])
                Out(Int(42), [Let1 { name: "x", body: Var(0, "x"), env: [] }])
                In(Var(0, "x"), [Int(42)], [])
                Out(Int(42), [])
            "#]],
        );
    }
}
