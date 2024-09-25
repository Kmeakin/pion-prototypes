use super::*;

pub enum Control<'core> {
    Expr(Expr<'core>),
    Value(Value<'core>),
    Reduction(Reduction<'core>),
}

pub type Kont<'core> = Vec<Frame<'core>>;

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

pub enum Reduction<'core> {
    Var(usize, &'core str, Value<'core>),
    App(&'core str, Env<'core>, &'core Expr<'core>, Value<'core>),
    Binop(Binop, Value<'core>),
    Let(&'core str, Value<'core>, &'core Expr<'core>),
    If(bool, &'core Expr<'core>, &'core Expr<'core>),
}

impl<'core> Reduction<'core> {
    pub fn result(&self) -> Expr<'core> {
        match self {
            Reduction::Var(_, _, value) => quote(value),
            Reduction::App(_, _, body, _) => **body,
            Reduction::Binop(_, value) => quote(value),
            Reduction::Let(_, _, body) => **body,
            Reduction::If(true, then, _) => **then,
            Reduction::If(false, _, r#else) => **r#else,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Reduction::Var(..) => "var",
            Reduction::App(..) => "app",
            Reduction::Binop(..) => "binop",
            Reduction::Let(..) => "let",
            Reduction::If(..) => "if",
        }
    }
}

pub fn step_expr<'core>(
    expr: Expr<'core>,
    env: &mut Env<'core>,
    kont: &mut Kont<'core>,
) -> Control<'core> {
    match expr {
        Expr::Int(n) => Control::Value(Value::Int(n)),
        Expr::Bool(b) => Control::Value(Value::Bool(b)),
        Expr::Var(var, name) => {
            let value = get_local(env, var, name);
            Control::Reduction(Reduction::Var(var, name, value))
        }
        Expr::Fun(name, body) => Control::Value(Value::Fun(name, env.clone(), body)),
        Expr::App(fun, arg) => {
            kont.push(Frame::App1(arg));
            Control::Expr(*fun)
        }
        Expr::Binop(op, lhs, rhs) => {
            kont.push(Frame::Binop1(op, rhs));
            Control::Expr(*lhs)
        }
        Expr::Let(name, init, body) => {
            kont.push(Frame::Let1(name, body));
            Control::Expr(*init)
        }
        Expr::If(cond, then, r#else) => {
            kont.push(Frame::If1(then, r#else));
            Control::Expr(*cond)
        }
    }
}

fn step_value<'core>(
    value: Value<'core>,
    env: &mut Env<'core>,
    kont: &mut Kont<'core>,
) -> Control<'core> {
    match kont.pop() {
        None => Control::Value(value),
        Some(Frame::App1(arg)) => {
            kont.push(Frame::App2(value));
            Control::Expr(*arg)
        }
        Some(Frame::App2(fun)) => match fun {
            Value::Fun(name, env, body) => {
                Control::Reduction(Reduction::App(name, env, body, value))
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
        Some(Frame::Binop2(op, lhs)) => match (&lhs, &value) {
            (Value::Int(lhs), Value::Int(rhs)) => {
                let value = op.apply(*lhs, *rhs);
                step_value(value, env, kont)
            }
            _ => panic!("Expected integers, got {lhs} and {value}"),
        },
        Some(Frame::Let1(name, body)) => {
            kont.push(Frame::Let2);
            Control::Reduction(Reduction::Let(name, value, body))
        }
        Some(Frame::Let2) => {
            env.pop();
            Control::Value(value)
        }
        Some(Frame::If1(then, r#else)) => match value {
            Value::Bool(b) => Control::Reduction(Reduction::If(b, then, r#else)),
            _ => panic!("Expected boolean, got {value}"),
        },
    }
}

pub fn step_reduction<'core>(
    reduction: Reduction<'core>,
    env: &mut Env<'core>,
    kont: &mut Kont<'core>,
) -> Control<'core> {
    match reduction {
        Reduction::Var(_var, _name, value) => Control::Value(value),
        Reduction::App(_name, new_env, body, arg) => {
            let old_env = std::mem::take(env);
            *env = new_env;
            env.push(arg);
            kont.push(Frame::App3(old_env));
            Control::Expr(*body)
        }
        Reduction::Binop(_op, value) => Control::Value(value),
        Reduction::Let(_name, init, body) => {
            env.push(init);
            Control::Expr(*body)
        }
        Reduction::If(true, then, _) => Control::Expr(*then),
        Reduction::If(false, _, r#else) => Control::Expr(*r#else),
    }
}

pub fn step_control<'core>(
    control: Control<'core>,
    env: &mut Env<'core>,
    kont: &mut Kont<'core>,
) -> Control<'core> {
    match control {
        Control::Expr(expr) => step_expr(expr, env, kont),
        Control::Value(value) => step_value(value, env, kont),
        Control::Reduction(reduction) => step_reduction(reduction, env, kont),
    }
}

pub fn eval<'core>(
    expr: &Expr<'core>,
    env: &mut Env<'core>,
    kont: &mut Kont<'core>,
) -> Value<'core> {
    let mut control = Control::Expr(*expr);
    loop {
        match step_control(control, env, kont) {
            Control::Value(value) if kont.is_empty() => return value,
            new_control => control = new_control,
        }
    }
}

pub fn trace<'core>(expr: &Expr<'core>, env: &mut Env<'core>, kont: &mut Kont<'core>) -> String {
    let mut trace = format!("{expr}\n");
    let mut control = Control::Expr(*expr);
    loop {
        match step_control(control, env, kont) {
            Control::Value(_) if kont.is_empty() => break,
            Control::Reduction(reduction) => {
                trace += &format!(
                    "[{}]=> {}\n",
                    reduction.name(),
                    plug(&reduction.result(), kont)
                );
                control = Control::Reduction(reduction);
            }
            new_control => control = new_control,
        }
    }
    trace
}

fn plug<'core>(expr: &Expr<'core>, stack: &[Frame<'core>]) -> String {
    let mut expr = format!("{expr}");

    for frame in stack.iter().rev() {
        match frame {
            Frame::App1(arg) => expr = format!("({expr}) ({arg})"),
            Frame::App2(fun) => expr = format!("({fun}) ({expr})"),
            Frame::Let1(name, body) => expr = format!("let {name} = {expr} in {body}"),
            Frame::If1(then, r#else) => expr = format!("if ({expr}) then ({then}) else ({else})"),
            Frame::Binop2(op, rhs) => expr = format!("{expr} {op} {rhs}"),
            Frame::App3(..) | Frame::Binop1(..) | Frame::Let2 => {}
        }
    }

    expr
}

#[cfg(test)]
mod tests {
    use expect_test::*;

    use super::*;

    #[track_caller]
    fn assert_eval(expr: &Expr, expect: Expect) {
        let mut env = Env::new();
        let mut kont = Kont::new();
        let value = eval(expr, &mut env, &mut kont);
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
    fn test_eval_fun() { assert_eval(&Expr::Fun("x", &Expr::Var(1, "x")), expect!["fun x => x"]); }

    #[test]
    fn test_eval_app() {
        assert_eval(
            &Expr::App(&Expr::Fun("x", &Expr::Var(0, "x")), &(Expr::Int(42))),
            expect!["42"],
        );
    }

    #[track_caller]
    fn assert_trace(expr: &Expr, expect: Expect) {
        let mut env = Env::new();
        let mut kont = Kont::new();
        let output = trace(expr, &mut env, &mut kont);
        expect.assert_eq(output.trim_end());
    }

    #[test]
    fn test_trace_bool() {
        assert_trace(&Expr::Bool(true), expect!["true"]);
        assert_trace(&Expr::Bool(false), expect!["false"]);
    }

    #[test]
    fn test_trace_fun() {
        assert_trace(&Expr::Fun("x", &Expr::Var(1, "x")), expect!["fun x => x"]);
    }

    #[test]
    fn test_trace_app() {
        assert_trace(
            &Expr::App(&Expr::Fun("x", &Expr::Var(0, "x")), &(Expr::Int(42))),
            expect![[r#"
                (fun x => x) 42
                [app]=> x
                [var]=> 42"#]],
        );
    }

    #[test]
    fn test_trace_if_nested() {
        let expr = Expr::If(
            &Expr::If(&Expr::Bool(true), &Expr::Bool(false), &Expr::Bool(true)),
            &Expr::Int(1),
            &Expr::Int(0),
        );
        assert_trace(
            &expr,
            expect![[r#"
                if (if true then false else true) then 1 else 0
                [if]=> if (false) then (1) else (0)
                [if]=> 0"#]],
        );
    }

    #[test]
    fn test_trace_if_nested2() {
        let expr = Expr::If(
            &Expr::If(&Expr::Bool(true), &Expr::Bool(false), &Expr::Bool(true)),
            &Expr::If(&Expr::Bool(false), &Expr::Int(10), &Expr::Int(20)),
            &Expr::If(&Expr::Bool(true), &Expr::Int(30), &Expr::Int(40)),
        );
        assert_trace(
            &expr,
            expect![[r#"
                if (if true then false else true) then (if false then 10 else 20) else (if true then 30 else 40)
                [if]=> if (false) then (if false then 10 else 20) else (if true then 30 else 40)
                [if]=> if true then 30 else 40
                [if]=> 30"#]],
        );
    }

    #[test]
    fn test_trace_let1() {
        let expr = Expr::Let("x", &Expr::Int(5), &Expr::Var(0, "x"));
        assert_trace(
            &expr,
            expect![[r#"
                let x = 5 in x
                [let]=> x
                [var]=> 5"#]],
        );
    }

    #[test]
    fn test_trace_let2() {
        let expr = Expr::Let(
            "x",
            &Expr::Int(5),
            &Expr::Let("y", &Expr::Int(10), &Expr::Var(1, "x")),
        );
        assert_trace(
            &expr,
            expect![[r#"
                let x = 5 in let y = 10 in x
                [let]=> let y = 10 in x
                [let]=> x
                [var]=> 5"#]],
        );
    }

    #[test]
    fn test_trace_if_and_let() {
        let expr = Expr::If(
            &Expr::Bool(true),
            &Expr::Let("x", &Expr::Int(5), &Expr::Var(0, "x")),
            &Expr::Let("y", &Expr::Int(10), &Expr::Var(1, "y")),
        );
        assert_trace(
            &expr,
            expect![[r#"
                if true then (let x = 5 in x) else (let y = 10 in y)
                [if]=> let x = 5 in x
                [let]=> x
                [var]=> 5"#]],
        );
    }

    #[test]
    fn test_trace_if_and_let2() {
        let expr = Expr::Let(
            "b",
            &Expr::Bool(true),
            &Expr::If(&Expr::Var(0, "b"), &Expr::Int(1), &Expr::Int(0)),
        );
        assert_trace(
            &expr,
            expect![[r#"
                let b = true in (if b then 1 else 0)
                [let]=> if b then 1 else 0
                [var]=> if (true) then (1) else (0)
                [if]=> 1"#]],
        );
    }

    #[test]
    fn test_trace_app2() {
        let expr = Expr::App(
            &Expr::Fun("x", &Expr::Fun("y", &Expr::Var(1, "x"))),
            &Expr::Int(10),
        );
        assert_trace(
            &expr,
            expect![[r#"
                (fun x => fun y => x) 10
                [app]=> fun y => x"#]],
        );
    }

    #[test]
    fn test_trace_app7() {
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
            &expr,
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
    fn test_trace_app_if() {
        // (fun x => x) (if true then 1 else 0)
        let expr = Expr::App(
            &Expr::Fun("x", &Expr::Var(0, "x")),
            &Expr::If(&Expr::Bool(true), &Expr::Int(1), &Expr::Int(0)),
        );
        assert_trace(
            &expr,
            expect![[r#"
                (fun x => x) (if true then 1 else 0)
                [if]=> (fun x => x) (1)
                [app]=> x
                [var]=> 1"#]],
        );
    }

    #[test]
    fn test_trace_app_if2() {
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
            &expr,
            expect![[r#"
                (if true then fun x => x else fun x => 0) (if false then 10 else 20)
                [if]=> (fun x => x) (if false then 10 else 20)
                [if]=> (fun x => x) (20)
                [app]=> x
                [var]=> 20"#]],
        );
    }
}
