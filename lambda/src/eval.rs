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
