#![no_main]

use libfuzzer_sys::fuzz_target;
use stepper::syntax::*;

fuzz_target!(|expr: Expr| {
    let mut env = Env::default();
    let value1 = stepper::big_step_semantics::eval(&expr, &mut env);

    let env = Env::default();
    let value2 = stepper::small_step_semantics::eval(expr, env);
    assert_eq!(value1, value2);

    let mut env = Env::default();
    let value3 = stepper::small_step_semantics2::eval(expr, &mut env);
    assert_eq!(value1, value3);
});
