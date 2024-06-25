#![feature(allocator_api)]

//! Context-free syntax
//! ```text
//! TokenTree ::=
//!     | '(' TokenTree* ')'
//!     | '[' TokenTree* ']'
//!     | '{' TokenTree* '}'
//!     | Punct
//!     | Ident
//!     | Literal
//! ```

use pion_lexer::{Delimiter, Literal, TokenKind};
use text_size::TextRange;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Located<T> {
    pub range: TextRange,
    pub data: T,
}

impl<T> Located<T> {
    pub const fn new(range: TextRange, data: T) -> Self { Self { range, data } }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenTree<'tt> {
    Ident,
    Punct(char),
    Literal(Literal),
    Group(Delimiter, &'tt [Located<Self>]),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    UnknownChar {
        c: Located<char>,
    },
    UnclosedOpenDelimiter {
        delim: Located<Delimiter>,
    },
    UnopenedCloseDelimiter {
        delim: Located<Delimiter>,
    },
    DelimiterMismatch {
        start: Located<Delimiter>,
        end: Located<Delimiter>,
    },
}

pub type Token = (TokenKind, TextRange);

pub fn parse_file<'tt>(
    bump: &'tt bumpalo::Bump,
    tokens: &mut impl Iterator<Item = Token>,
    errors: &mut Vec<Error>,
) -> &'tt [Located<TokenTree<'tt>>] {
    let mut tts = Vec::new_in(bump);
    while let Some(tt) = parse_token_tree(bump, tokens, errors) {
        tts.push(tt);
    }
    tts.leak()
}

pub fn parse_token_tree<'tt>(
    bump: &'tt bumpalo::Bump,
    tokens: &mut impl Iterator<Item = Token>,
    errors: &mut Vec<Error>,
) -> Option<Located<TokenTree<'tt>>> {
    let (token, range) = tokens.next()?;
    let tt = match token {
        TokenKind::UnknownChar(c) => {
            errors.push(Error::UnknownChar {
                c: Located::new(range, c),
            });
            return None;
        }
        TokenKind::CloseDelim(delim) => {
            errors.push(Error::UnopenedCloseDelimiter {
                delim: Located::new(range, delim),
            });
            return None;
        }

        TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment => {
            return parse_token_tree(bump, tokens, errors)
        }

        TokenKind::OpenDelim(start_delimiter) => {
            parse_group(bump, tokens, errors, Located::new(range, start_delimiter))
        }
        TokenKind::Punct(c) => Located::new(range, TokenTree::Punct(c)),
        TokenKind::Ident => Located::new(range, TokenTree::Ident),
        TokenKind::Lit(lit) => Located::new(range, TokenTree::Literal(lit)),
    };
    Some(tt)
}

fn parse_group<'tt>(
    bump: &'tt bumpalo::Bump,
    tokens: &mut impl Iterator<Item = Token>,
    errors: &mut Vec<Error>,
    start_delimiter: Located<Delimiter>,
) -> Located<TokenTree<'tt>> {
    let contents = parse_group_contents(bump, tokens, errors, start_delimiter);
    let range = contents.range;
    let contents = contents.data;
    Located::new(range, TokenTree::Group(start_delimiter.data, contents))
}

fn parse_group_contents<'tt>(
    bump: &'tt bumpalo::Bump,
    tokens: &mut impl Iterator<Item = Token>,
    errors: &mut Vec<Error>,
    start_delimiter: Located<Delimiter>,
) -> Located<&'tt [Located<TokenTree<'tt>>]> {
    let mut contents = Vec::new_in(bump);
    let start_range = start_delimiter.range;
    let mut end_range = start_range;

    loop {
        match tokens.next() {
            None => {
                errors.push(Error::UnclosedOpenDelimiter {
                    delim: start_delimiter,
                });
                break;
            }
            Some((kind, range)) => {
                end_range = range;
                let tt = match kind {
                    TokenKind::UnknownChar(c) => {
                        errors.push(Error::UnknownChar {
                            c: Located::new(range, c),
                        });
                        continue;
                    }
                    TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment => {
                        continue
                    }
                    TokenKind::OpenDelim(delim) => {
                        parse_group(bump, tokens, errors, Located::new(range, delim))
                    }
                    TokenKind::CloseDelim(end_delimiter) => {
                        if end_delimiter != start_delimiter.data {
                            errors.push(Error::DelimiterMismatch {
                                end: Located::new(range, end_delimiter),
                                start: start_delimiter,
                            });
                        }
                        break;
                    }
                    TokenKind::Punct(c) => Located::new(range, TokenTree::Punct(c)),
                    TokenKind::Ident => Located::new(range, TokenTree::Ident),
                    TokenKind::Lit(lit) => Located::new(range, TokenTree::Literal(lit)),
                };
                contents.push(tt);
            }
        }
    }

    let contents = contents.leak();
    Located::new(
        TextRange::new(start_range.start(), end_range.end()),
        &*contents,
    )
}

#[cfg(test)]
mod tests {
    use text_size::{TextRange, TextSize};

    use super::*;

    #[track_caller]
    fn check_ok(text: &str, expected: Option<Located<TokenTree>>) {
        let mut tokens = pion_lexer::lex(text).map(|(kind, range)| {
            let start = TextSize::from(range.start as u32);
            let end = TextSize::from(range.end as u32);
            let range = TextRange::new(start, end);
            (kind, range)
        });
        let bump = bumpalo::Bump::new();
        let mut errors = Vec::new();
        let got = parse_token_tree(&bump, &mut tokens, &mut errors);
        assert_eq!(got, expected);
        assert_eq!(errors, []);
    }

    #[track_caller]
    fn check_errors(
        text: &str,
        expected_tt: Option<Located<TokenTree>>,
        expected_errors: &[Error],
    ) {
        let mut tokens = pion_lexer::lex(text).map(|(kind, range)| {
            let start = TextSize::from(range.start as u32);
            let end = TextSize::from(range.end as u32);
            let range = TextRange::new(start, end);
            (kind, range)
        });
        let bump = bumpalo::Bump::new();
        let mut errors = Vec::new();
        let got = parse_token_tree(&bump, &mut tokens, &mut errors);
        assert_eq!(got, expected_tt);
        assert_eq!(errors, expected_errors);
    }

    #[test]
    fn empty() { check_ok("", None); }

    #[test]
    fn ident() {
        check_ok(
            "abc",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(3)),
                TokenTree::Ident,
            )),
        );
    }

    #[test]
    fn punct() {
        check_ok(
            ",",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(1)),
                TokenTree::Punct(','),
            )),
        );
    }

    #[test]
    fn literal() {
        check_ok(
            "1234",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(4)),
                TokenTree::Literal(Literal::Number),
            )),
        );

        check_ok(
            "'a'",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(3)),
                TokenTree::Literal(Literal::Char),
            )),
        );

        check_ok(
            "\"a\"",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(3)),
                TokenTree::Literal(Literal::String),
            )),
        );
    }

    #[test]
    fn group_empty() {
        check_ok(
            "()",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(2)),
                TokenTree::Group(Delimiter::Round, &[]),
            )),
        );

        check_ok(
            "[]",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(2)),
                TokenTree::Group(Delimiter::Square, &[]),
            )),
        );

        check_ok(
            "{}",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(2)),
                TokenTree::Group(Delimiter::Curly, &[]),
            )),
        );
    }

    #[test]
    fn group_singleton() {
        check_ok(
            "(a)",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(3)),
                TokenTree::Group(
                    Delimiter::Round,
                    &[Located::new(
                        TextRange::new(TextSize::from(1), TextSize::from(2)),
                        TokenTree::Ident,
                    )],
                ),
            )),
        );

        check_ok(
            "[a]",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(3)),
                TokenTree::Group(
                    Delimiter::Square,
                    &[Located::new(
                        TextRange::new(TextSize::from(1), TextSize::from(2)),
                        TokenTree::Ident,
                    )],
                ),
            )),
        );

        check_ok(
            "{a}",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(3)),
                TokenTree::Group(
                    Delimiter::Curly,
                    &[Located::new(
                        TextRange::new(TextSize::from(1), TextSize::from(2)),
                        TokenTree::Ident,
                    )],
                ),
            )),
        );
    }

    #[test]
    fn group_many() {
        check_ok(
            "(a, b)",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(6)),
                TokenTree::Group(
                    Delimiter::Round,
                    &[
                        Located::new(
                            TextRange::new(TextSize::from(1), TextSize::from(2)),
                            TokenTree::Ident,
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(2), TextSize::from(3)),
                            TokenTree::Punct(','),
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(4), TextSize::from(5)),
                            TokenTree::Ident,
                        ),
                    ],
                ),
            )),
        );

        check_ok(
            "[a, b]",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(6)),
                TokenTree::Group(
                    Delimiter::Square,
                    &[
                        Located::new(
                            TextRange::new(TextSize::from(1), TextSize::from(2)),
                            TokenTree::Ident,
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(2), TextSize::from(3)),
                            TokenTree::Punct(','),
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(4), TextSize::from(5)),
                            TokenTree::Ident,
                        ),
                    ],
                ),
            )),
        );

        check_ok(
            "{a, b}",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(6)),
                TokenTree::Group(
                    Delimiter::Curly,
                    &[
                        Located::new(
                            TextRange::new(TextSize::from(1), TextSize::from(2)),
                            TokenTree::Ident,
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(2), TextSize::from(3)),
                            TokenTree::Punct(','),
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(4), TextSize::from(5)),
                            TokenTree::Ident,
                        ),
                    ],
                ),
            )),
        );
    }

    #[test]
    fn group_nested() {
        check_ok(
            "([a], {b})",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(10)),
                TokenTree::Group(
                    Delimiter::Round,
                    &[
                        Located::new(
                            TextRange::new(TextSize::from(1), TextSize::from(4)),
                            TokenTree::Group(
                                Delimiter::Square,
                                &[Located::new(
                                    TextRange::new(TextSize::from(2), TextSize::from(3)),
                                    TokenTree::Ident,
                                )],
                            ),
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(4), TextSize::from(5)),
                            TokenTree::Punct(','),
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(6), TextSize::from(9)),
                            TokenTree::Group(
                                Delimiter::Curly,
                                &[Located::new(
                                    TextRange::new(TextSize::from(7), TextSize::from(8)),
                                    TokenTree::Ident,
                                )],
                            ),
                        ),
                    ],
                ),
            )),
        );
    }

    #[test]
    fn error_unknown_char() {
        check_errors(
            "\0",
            None,
            &[Error::UnknownChar {
                c: Located::new(TextRange::new(TextSize::from(0), TextSize::from(1)), '\0'),
            }],
        );
    }

    #[test]
    fn error_unclosed_open_delim() {
        check_errors(
            "(",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(1)),
                TokenTree::Group(Delimiter::Round, &[]),
            )),
            &[Error::UnclosedOpenDelimiter {
                delim: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    Delimiter::Round,
                ),
            }],
        );

        check_errors(
            "[",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(1)),
                TokenTree::Group(Delimiter::Square, &[]),
            )),
            &[Error::UnclosedOpenDelimiter {
                delim: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    Delimiter::Square,
                ),
            }],
        );

        check_errors(
            "{",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(1)),
                TokenTree::Group(Delimiter::Curly, &[]),
            )),
            &[Error::UnclosedOpenDelimiter {
                delim: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    Delimiter::Curly,
                ),
            }],
        );
    }

    #[test]
    fn error_unopened_close_delim() {
        check_errors(
            ")",
            None,
            &[Error::UnopenedCloseDelimiter {
                delim: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    Delimiter::Round,
                ),
            }],
        );

        check_errors(
            "]",
            None,
            &[Error::UnopenedCloseDelimiter {
                delim: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    Delimiter::Square,
                ),
            }],
        );

        check_errors(
            "}",
            None,
            &[Error::UnopenedCloseDelimiter {
                delim: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    Delimiter::Curly,
                ),
            }],
        );
    }

    #[test]
    fn error_delim_mismatch() {
        check_errors(
            "(]",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(2)),
                TokenTree::Group(Delimiter::Round, &[]),
            )),
            &[Error::DelimiterMismatch {
                start: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    Delimiter::Round,
                ),
                end: Located::new(
                    TextRange::new(TextSize::from(1), TextSize::from(2)),
                    Delimiter::Square,
                ),
            }],
        );

        check_errors(
            "[}",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(2)),
                TokenTree::Group(Delimiter::Square, &[]),
            )),
            &[Error::DelimiterMismatch {
                start: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    Delimiter::Square,
                ),
                end: Located::new(
                    TextRange::new(TextSize::from(1), TextSize::from(2)),
                    Delimiter::Curly,
                ),
            }],
        );

        check_errors(
            "{)",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(2)),
                TokenTree::Group(Delimiter::Curly, &[]),
            )),
            &[Error::DelimiterMismatch {
                start: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    Delimiter::Curly,
                ),
                end: Located::new(
                    TextRange::new(TextSize::from(1), TextSize::from(2)),
                    Delimiter::Round,
                ),
            }],
        );
    }
}
