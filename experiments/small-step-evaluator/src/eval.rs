use crate::syntax::*;

pub mod big_step;
pub mod small_step;

#[track_caller]
fn get_local<'core>(env: &Env<'core>, var: usize, name: &str) -> Value<'core> {
    let len = env.len();
    let index = len.checked_sub(var).and_then(|index| index.checked_sub(1));

    match index.and_then(|index| env.get(index)) {
        Some(value) => value.clone(),
        None => panic!("Unbound variable `{name}` (index {var} in environment of length {len})"),
    }
}

impl Binop {
    pub fn apply<'core>(self, lhs: u32, rhs: u32) -> Value<'core> {
        match self {
            Self::Add => Value::Int(u32::wrapping_add(lhs, rhs)),
            Self::Sub => Value::Int(u32::wrapping_sub(lhs, rhs)),
            Self::Mul => Value::Int(u32::wrapping_mul(lhs, rhs)),
            Self::Eq => Value::Bool(u32::eq(&lhs, &rhs)),
            Self::Ne => Value::Bool(u32::ne(&lhs, &rhs)),
            Self::Le => Value::Bool(u32::le(&lhs, &rhs)),
            Self::Ge => Value::Bool(u32::ge(&lhs, &rhs)),
            Self::Lt => Value::Bool(u32::lt(&lhs, &rhs)),
            Self::Gt => Value::Bool(u32::gt(&lhs, &rhs)),
        }
    }
}
