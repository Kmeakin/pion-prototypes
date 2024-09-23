// big step tracing evaluator
// adapted from https://arxiv.org/pdf/1906.11422
// TODO: rewrite as small step, clean up the code

use core::panic;

use crate::syntax::*;

enum Frame<'core> {
    /// [.] e
    App1(Expr<'core>),

    /// v [.]
    App2(Value<'core>),

    /// let x = [.] in e
    Let1(&'core str, Expr<'core>),

    /// if [.] then e1 else e2
    If1(&'core Expr<'core>, &'core Expr<'core>),
}

fn eval<'core>(
    expr: Expr<'core>,
    env: &mut Env<'core>,
    stack: &mut Vec<Frame<'core>>,
    output: &mut String,
) -> Value<'core> {
    match expr {
        Expr::Int(n) => Value::Int(n),
        Expr::Bool(b) => Value::Bool(b),
        Expr::Var(var, name) => {
            let value = get_local(env, name, var);
            output.push_str(&format!("[var]=> {}\n", &plug(quote(&value), stack)));
            value
        }
        Expr::Fun(name, body) => Value::Fun(name, env.clone(), body),
        Expr::App(fun, arg) => {
            stack.push(Frame::App1(*arg));
            let fun = eval(*fun, env, stack, output);
            stack.pop();

            stack.push(Frame::App2(fun.clone()));
            let arg = eval(*arg, env, stack, output);
            stack.pop();

            match fun {
                Value::Fun(_name, mut env, body) => {
                    output.push_str(&format!("[app]=> {}\n", plug(*body, stack)));
                    env.push(arg);
                    eval(*body, &mut env, stack, output)
                }
                _ => panic!("Expected a function"),
            }
        }
        Expr::Let(name, init, body) => {
            stack.push(Frame::Let1(name, *body));
            let init = eval(*init, env, stack, output);
            stack.pop();

            output.push_str(format!("[let]=> {}\n", plug(*body, stack)).as_str());

            env.push(init);
            let body = eval(*body, env, stack, output);
            env.pop();

            body
        }
        Expr::If(cond, then, r#else) => {
            stack.push(Frame::If1(then, r#else));

            let cond = eval(*cond, env, stack, output);
            stack.pop();

            match cond {
                Value::Bool(true) => {
                    output.push_str(&format!("[if-true]=> {}\n", plug(*then, stack)));
                    eval(*then, env, stack, output)
                }
                Value::Bool(false) => {
                    output.push_str(&format!("[if-false]=> {}\n", plug(*r#else, stack)));
                    eval(*r#else, env, stack, output)
                }
                _ => panic!("Expected a boolean"),
            }
        }
    }
}

fn plug<'core>(expr: Expr<'core>, stack: &[Frame<'core>]) -> String {
    let mut expr = format!("{expr}");

    for frame in stack.iter().rev() {
        match frame {
            Frame::App1(arg) => expr = format!("({expr}) ({arg})"),
            Frame::App2(fun) => expr = format!("({fun}) ({expr})"),
            Frame::Let1(name, body) => expr = format!("let {name} = {expr} in {body}"),
            Frame::If1(then, r#else) => expr = format!("if ({expr}) then ({then}) else ({else})"),
        }
    }

    expr
}

fn quote<'core>(value: &Value<'core>) -> Expr<'core> {
    match value {
        Value::Error(_) => panic!(),
        Value::Int(n) => Expr::Int(*n),
        Value::Bool(b) => Expr::Bool(*b),
        Value::Fun(name, _, body) => Expr::Fun(name, body),
    }
}

#[cfg(test)]
mod tests {
    use expect_test::*;

    use super::*;

    #[track_caller]
    fn assert_trace(expr: Expr, expect: Expect) {
        let mut output = format!("{expr}\n");
        let mut env = Env::new();
        let mut stack = Vec::new();
        eval(expr, &mut env, &mut stack, &mut output);

        expect.assert_eq(output.trim_end());
    }

    #[test]
    fn if_true() {
        let expr = Expr::If(&Expr::Bool(true), &Expr::Int(1), &Expr::Int(0));
        assert_trace(
            expr,
            expect![[r#"
                if true then 1 else 0
                [if-true]=> 1"#]],
        );
    }

    #[test]
    fn if_false() {
        let expr = Expr::If(&Expr::Bool(false), &Expr::Int(1), &Expr::Int(0));
        assert_trace(
            expr,
            expect![[r#"
                if false then 1 else 0
                [if-false]=> 0"#]],
        );
    }

    #[test]
    fn if_nested() {
        let expr = Expr::If(
            &Expr::If(&Expr::Bool(true), &Expr::Bool(false), &Expr::Bool(true)),
            &Expr::Int(1),
            &Expr::Int(0),
        );
        assert_trace(
            expr,
            expect![[r#"
                if (if true then false else true) then 1 else 0
                [if-true]=> if (false) then (1) else (0)
                [if-false]=> 0"#]],
        );
    }

    #[test]
    fn if_nested2() {
        let expr = Expr::If(
            &Expr::If(&Expr::Bool(true), &Expr::Bool(false), &Expr::Bool(true)),
            &Expr::If(&Expr::Bool(false), &Expr::Int(10), &Expr::Int(20)),
            &Expr::If(&Expr::Bool(true), &Expr::Int(30), &Expr::Int(40)),
        );
        assert_trace(
            expr,
            expect![[r#"
                if (if true then false else true) then (if false then 10 else 20) else (if true then 30 else 40)
                [if-true]=> if (false) then (if false then 10 else 20) else (if true then 30 else 40)
                [if-false]=> if true then 30 else 40
                [if-true]=> 30"#]],
        );
    }

    #[test]
    fn let1() {
        let expr = Expr::Let("x", &Expr::Int(5), &Expr::Var(0, "x"));
        assert_trace(
            expr,
            expect![[r#"
                let x = 5 in x
                [let]=> x
                [var]=> 5"#]],
        );
    }

    #[test]
    fn let2() {
        let expr = Expr::Let(
            "x",
            &Expr::Int(5),
            &Expr::Let("y", &Expr::Int(10), &Expr::Var(1, "x")),
        );
        assert_trace(
            expr,
            expect![[r#"
                let x = 5 in let y = 10 in x
                [let]=> let y = 10 in x
                [let]=> x
                [var]=> 5"#]],
        );
    }

    #[test]
    fn if_and_let() {
        let expr = Expr::If(
            &Expr::Bool(true),
            &Expr::Let("x", &Expr::Int(5), &Expr::Var(0, "x")),
            &Expr::Let("y", &Expr::Int(10), &Expr::Var(1, "y")),
        );
        assert_trace(
            expr,
            expect![[r#"
                if true then (let x = 5 in x) else (let y = 10 in y)
                [if-true]=> let x = 5 in x
                [let]=> x
                [var]=> 5"#]],
        );
    }

    #[test]
    fn if_and_let2() {
        let expr = Expr::Let(
            "b",
            &Expr::Bool(true),
            &Expr::If(&Expr::Var(0, "b"), &Expr::Int(1), &Expr::Int(0)),
        );
        assert_trace(
            expr,
            expect![[r#"
                let b = true in if b then 1 else 0
                [let]=> if b then 1 else 0
                [var]=> if (true) then (1) else (0)
                [if-true]=> 1"#]],
        );
    }

    #[test]
    fn app() {
        let expr = Expr::App(&Expr::Fun("x", &Expr::Var(0, "x")), &Expr::Int(10));
        assert_trace(
            expr,
            expect![[r#"
                (fun x => x) 10
                [app]=> x
                [var]=> 10"#]],
        );
    }

    #[test]
    fn app2() {
        let expr = Expr::App(
            &Expr::Fun("x", &Expr::Fun("y", &Expr::Var(1, "x"))),
            &Expr::Int(10),
        );
        assert_trace(
            expr,
            expect![[r#"
                (fun x => fun y => x) 10
                [app]=> fun y => x"#]],
        );
    }

    #[test]
    fn app7() {
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
        assert_trace(
            expr,
            expect![[r#"
                let x = 5 in let y = 10 in let f = fun a => x in let z = f 42 in y
                [let]=> let y = 10 in let f = fun a => x in let z = f 42 in y
                [let]=> let f = fun a => x in let z = f 42 in y
                [let]=> let z = f 42 in y
                [var]=> let z = (fun a => x) (42) in y
                [app]=> let z = x in y
                [var]=> let z = 5 in y
                [let]=> y
                [var]=> 10"#]],
        )
    }

    #[test]
    fn app_if() {
        // (fun x => x) (if true then 1 else 0)
        let expr = Expr::App(
            &Expr::Fun("x", &Expr::Var(0, "x")),
            &Expr::If(&Expr::Bool(true), &Expr::Int(1), &Expr::Int(0)),
        );
        assert_trace(
            expr,
            expect![[r#"
            (fun x => x) (if true then 1 else 0)
            [if-true]=> (fun x => x) (1)
            [app]=> x
            [var]=> 1"#]],
        );
    }

    #[test]
    fn app_if2() {
        // (if true then fun x => x else fun x => 0) (if false then 10 else 20)
        let expr = Expr::App(
            &Expr::If(
                &Expr::Bool(true),
                &Expr::Fun("x", &Expr::Var(0, "x")),
                &Expr::Fun("x", &Expr::Int(0)),
            ),
            &Expr::If(&Expr::Bool(false), &Expr::Int(10), &Expr::Int(20)),
        );
        assert_trace(
            expr,
            expect![[r#"
            (if true then fun x => x else fun x => 0) (if false then 10 else 20)
            [if-true]=> (fun x => x) (if false then 10 else 20)
            [if-false]=> (fun x => x) (20)
            [app]=> x
            [var]=> 20"#]],
        );
    }
}
