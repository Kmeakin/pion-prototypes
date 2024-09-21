use crate::syntax::*;

fn get_var<'core>(env: &Env<'core>, var: usize) -> Value<'core> {
    let index = env.len() - var - 1;
    match env.get(index) {
        None => panic!("Unbound local variable: {var:?} in len {}", env.len()),
        Some(value) => value.clone(),
    }
}

pub fn eval<'core>(expr: &Expr<'core>, env: &mut Env<'core>) -> Value<'core> {
    todo!()
}
