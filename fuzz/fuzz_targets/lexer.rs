#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &str| {
    std::hint::black_box(pion_lexer::lex(data).count());
});
