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

pub type Kont<'core> = Vec<Frame<'core>>;

pub fn push_kont<'core>(mut kont: Kont<'core>, frame: Frame<'core>) -> Kont<'core> {
    kont.push(frame);
    kont
}

pub fn push_env<'core>(mut env: Env<'core>, value: Value<'core>) -> Env<'core> {
    env.push(value);
    env
}

pub trait EvalVisitor<'core>: Sized {
    fn step(
        &mut self,
        control: Control<'core>,
        env: Env<'core>,
        kont: Kont<'core>,
    ) -> (Control<'core>, Env<'core>, Kont<'core>) {
        step(self, control, env, kont)
    }

    fn eval(&mut self, expr: Expr<'core>, env: Env<'core>, kont: Kont<'core>) -> Value<'core> {
        eval(self, expr, env, kont)
    }

    fn var_reduced(
        &mut self,
        _var: (usize, &'core str),
        value: Value<'core>,
        env: Env<'core>,
        kont: Kont<'core>,
    ) -> (Control<'core>, Env<'core>, Kont<'core>) {
        (Control::Value(value), env, kont)
    }

    fn app_reduced(
        &mut self,
        (_name, new_env, body): (&'core str, Env<'core>, &'core Expr<'core>),
        arg: Value<'core>,
        env: Env<'core>,
        kont: Kont<'core>,
    ) -> (Control<'core>, Env<'core>, Kont<'core>) {
        (
            Control::Expr(*body),
            push_env(new_env, arg),
            push_kont(kont, Frame::App3(env)),
        )
    }

    fn binop_reduced(
        &mut self,
        (op, lhs, rhs): (Binop, u32, u32),
        env: Env<'core>,
        kont: Kont<'core>,
    ) -> (Control<'core>, Env<'core>, Kont<'core>) {
        (Control::Value(op.apply(lhs, rhs)), env, kont)
    }

    fn let_reduced(
        &mut self,
        (_name, init, body): (&'core str, Value<'core>, &'core Expr<'core>),
        env: Env<'core>,
        kont: Kont<'core>,
    ) -> (Control<'core>, Env<'core>, Kont<'core>) {
        (
            Control::Expr(*body),
            push_env(env, init),
            push_kont(kont, Frame::Let2),
        )
    }

    fn if_reduced(
        &mut self,
        cond: bool,
        then: &'core Expr<'core>,
        r#else: &'core Expr<'core>,
        env: Env<'core>,
        kont: Kont<'core>,
    ) -> (Control<'core>, Env<'core>, Kont<'core>) {
        match cond {
            true => (Control::Expr(*then), env, kont),
            false => (Control::Expr(*r#else), env, kont),
        }
    }
}

pub fn eval<'core, V: EvalVisitor<'core>>(
    visitor: &mut V,
    expr: Expr<'core>,
    mut env: Env<'core>,
    mut kont: Kont<'core>,
) -> Value<'core> {
    let mut control = Control::Expr(expr);
    loop {
        match visitor.step(control, env, kont) {
            (Control::Value(value), _, kont) if kont.is_empty() => return value,
            cek => (control, env, kont) = cek,
        }
    }
}

pub fn step<'core, V: EvalVisitor<'core>>(
    visitor: &mut V,
    control: Control<'core>,
    mut env: Env<'core>,
    mut kont: Kont<'core>,
) -> (Control<'core>, Env<'core>, Kont<'core>) {
    match control {
        Control::Expr(Expr::Int(n)) => (Control::Value(Value::Int(n)), env, kont),
        Control::Expr(Expr::Bool(b)) => (Control::Value(Value::Bool(b)), env, kont),
        Control::Expr(Expr::Var(var, name)) => {
            let value = get_local(&env, var, name);
            visitor.var_reduced((var, name), value, env, kont)
        }
        Control::Expr(Expr::Fun(name, body)) => (
            Control::Value(Value::Fun(name, env.clone(), body)),
            env,
            kont,
        ),
        Control::Expr(Expr::App(fun, arg)) => {
            (Control::Expr(*fun), env, push_kont(kont, Frame::App1(arg)))
        }
        Control::Expr(Expr::Binop(op, lhs, rhs)) => (
            Control::Expr(*lhs),
            env,
            push_kont(kont, Frame::Binop1(op, rhs)),
        ),
        Control::Expr(Expr::Let(name, init, body)) => (
            Control::Expr(*init),
            env,
            push_kont(kont, Frame::Let1(name, body)),
        ),
        Control::Expr(Expr::If(cond, then, r#else)) => (
            Control::Expr(*cond),
            env,
            push_kont(kont, Frame::If1(then, r#else)),
        ),
        Control::Value(value) => match kont.pop() {
            None => (Control::Value(value), env, kont),
            Some(Frame::App1(arg)) => (
                Control::Expr(*arg),
                env,
                push_kont(kont, Frame::App2(value)),
            ),
            Some(Frame::App2(fun)) => match fun {
                Value::Fun(name, new_env, body) => {
                    visitor.app_reduced((name, new_env, body), value, env, kont)
                }
                _ => panic!("Expected function, got {fun}"),
            },
            Some(Frame::App3(old_env)) => (Control::Value(value), old_env, kont),
            Some(Frame::Binop1(op, rhs)) => (
                Control::Expr(*rhs),
                env,
                push_kont(kont, Frame::Binop2(op, value)),
            ),
            Some(Frame::Binop2(op, lhs)) => {
                let rhs = value;
                let (Value::Int(lhs), Value::Int(rhs)) = (&lhs, &rhs) else {
                    panic!("Expected integers, got {lhs} and {rhs}");
                };
                let (lhs, rhs) = (*lhs, *rhs);
                visitor.binop_reduced((op, lhs, rhs), env, kont)
            }
            Some(Frame::Let1(name, body)) => visitor.let_reduced((name, value, body), env, kont),
            Some(Frame::Let2) => {
                env.pop();
                (Control::Value(value), env, kont)
            }
            Some(Frame::If1(then, r#else)) => match value {
                Value::Bool(cond) => visitor.if_reduced(cond, then, r#else, env, kont),
                _ => panic!("Expected boolean, got {value}"),
            },
        },
    }
}

pub struct DefaultVisitor;
impl<'core> EvalVisitor<'core> for DefaultVisitor {}

#[cfg(test)]
mod tests {
    use expect_test::*;

    use super::*;

    #[track_caller]
    fn assert_eval(expr: &Expr, expect: Expect) {
        let mut visitor = DefaultVisitor;
        let env = Env::new();
        let kont = Kont::new();
        let value = eval(&mut visitor, *expr, env, kont);
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
}
