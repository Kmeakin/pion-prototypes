#![no_main]

use libfuzzer_sys::fuzz_target;
use pion_parser::parse_file;

fuzz_target!(|data: &str| {
    let mut errors = Vec::new();
    let mut tokens = pion_lexer::lex(data);
    let bump = bumpalo::Bump::default();
    let tts = parse_file(&bump, &mut tokens, &mut errors);
    std::hint::black_box(tts);
});
