use text_size::TextRange;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Location {
    pub file: usize,
    pub range: TextRange,
}

impl Location {
    pub const fn new(file: usize, range: TextRange) -> Self { Self { file, range } }
}
