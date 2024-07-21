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

use pion_lexer::{DelimiterKind, LiteralKind, Token, TokenKind};
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
pub enum TokenTree<'text, 'tt> {
    Ident(&'text str),
    Punct(char),
    Literal(Literal<'text>),
    Group(DelimiterKind, &'tt [Located<Self>]),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Literal<'text> {
    Number(&'text str),
    Char(&'text str),
    String(&'text str),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    UnknownChar {
        c: Located<char>,
    },
    UnclosedOpenDelimiter {
        delim: Located<DelimiterKind>,
    },
    UnopenedCloseDelimiter {
        delim: Located<DelimiterKind>,
    },
    DelimiterMismatch {
        start: Located<DelimiterKind>,
        end: Located<DelimiterKind>,
    },
}

pub fn parse_file<'text, 'tt>(
    bump: &'tt bumpalo::Bump,
    tokens: &mut impl Iterator<Item = Token<'text>>,
    errors: &mut Vec<Error>,
) -> &'tt [Located<TokenTree<'text, 'tt>>] {
    let mut tts = Vec::new_in(bump);
    while let Some(tt) = parse_token_tree(bump, tokens, errors) {
        tts.push(tt);
    }
    tts.leak()
}

pub fn parse_token_tree<'text, 'tt>(
    bump: &'tt bumpalo::Bump,
    tokens: &mut impl Iterator<Item = Token<'text>>,
    errors: &mut Vec<Error>,
) -> Option<Located<TokenTree<'text, 'tt>>> {
    let token = tokens.next()?;
    let range = token.range;
    let tt = match token.kind {
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
            return parse_token_tree(bump, tokens, errors);
        }

        TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment => {
            return parse_token_tree(bump, tokens, errors)
        }

        TokenKind::OpenDelim(start_delimiter) => {
            parse_group(bump, tokens, errors, Located::new(range, start_delimiter))
        }
        TokenKind::Punct(c) => Located::new(range, TokenTree::Punct(c)),
        TokenKind::Ident => Located::new(range, TokenTree::Ident(token.text)),
        TokenKind::Literal(lit) => Located::new(
            range,
            TokenTree::Literal(match lit {
                LiteralKind::Number => Literal::Number(token.text),
                LiteralKind::Char => Literal::Char(token.text),
                LiteralKind::String => Literal::String(token.text),
            }),
        ),
    };
    Some(tt)
}

fn parse_group<'text, 'tt>(
    bump: &'tt bumpalo::Bump,
    tokens: &mut impl Iterator<Item = Token<'text>>,
    errors: &mut Vec<Error>,
    start_delimiter: Located<DelimiterKind>,
) -> Located<TokenTree<'text, 'tt>> {
    let contents = parse_group_contents(bump, tokens, errors, start_delimiter);
    let range = contents.range;
    let contents = contents.data;
    Located::new(range, TokenTree::Group(start_delimiter.data, contents))
}

fn parse_group_contents<'text, 'tt>(
    bump: &'tt bumpalo::Bump,
    tokens: &mut impl Iterator<Item = Token<'text>>,
    errors: &mut Vec<Error>,
    start_delimiter: Located<DelimiterKind>,
) -> Located<&'tt [Located<TokenTree<'text, 'tt>>]> {
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
            Some(token) => {
                end_range = token.range;
                let tt = match token.kind {
                    TokenKind::UnknownChar(c) => {
                        errors.push(Error::UnknownChar {
                            c: Located::new(token.range, c),
                        });
                        continue;
                    }
                    TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment => {
                        continue
                    }
                    TokenKind::OpenDelim(delim) => {
                        parse_group(bump, tokens, errors, Located::new(token.range, delim))
                    }
                    TokenKind::CloseDelim(end_delimiter) => {
                        if end_delimiter != start_delimiter.data {
                            errors.push(Error::DelimiterMismatch {
                                end: Located::new(token.range, end_delimiter),
                                start: start_delimiter,
                            });
                        }
                        break;
                    }
                    TokenKind::Punct(c) => Located::new(token.range, TokenTree::Punct(c)),
                    TokenKind::Ident => Located::new(token.range, TokenTree::Ident(token.text)),
                    TokenKind::Literal(lit) => Located::new(
                        token.range,
                        TokenTree::Literal(match lit {
                            LiteralKind::Number => Literal::Number(token.text),
                            LiteralKind::Char => Literal::Char(token.text),
                            LiteralKind::String => Literal::String(token.text),
                        }),
                    ),
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
    fn assert_parse(text: &str, expected: Option<Located<TokenTree>>) {
        let mut tokens = pion_lexer::lex(text);
        let bump = bumpalo::Bump::new();
        let mut errors = Vec::new();
        let got = parse_token_tree(&bump, &mut tokens, &mut errors);
        assert_eq!(got, expected);
        assert_eq!(errors, []);
    }

    #[track_caller]
    fn assert_parse_errors(
        text: &str,
        expected_tt: Option<Located<TokenTree>>,
        expected_errors: &[Error],
    ) {
        let mut tokens = pion_lexer::lex(text);
        let bump = bumpalo::Bump::new();
        let mut errors = Vec::new();
        let got = parse_token_tree(&bump, &mut tokens, &mut errors);
        assert_eq!(got, expected_tt);
        assert_eq!(errors, expected_errors);
    }

    #[test]
    fn empty() { assert_parse("", None); }

    #[test]
    fn ident() {
        assert_parse(
            "abc",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(3)),
                TokenTree::Ident("abc"),
            )),
        );
    }

    #[test]
    fn punct() {
        assert_parse(
            ",",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(1)),
                TokenTree::Punct(','),
            )),
        );
    }

    #[test]
    fn literal() {
        assert_parse(
            "1234",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(4)),
                TokenTree::Literal(Literal::Number("1234")),
            )),
        );

        assert_parse(
            "'a'",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(3)),
                TokenTree::Literal(Literal::Char("'a'")),
            )),
        );

        assert_parse(
            "\"a\"",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(3)),
                TokenTree::Literal(Literal::String("\"a\"")),
            )),
        );
    }

    #[test]
    fn group_empty() {
        assert_parse(
            "()",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(2)),
                TokenTree::Group(DelimiterKind::Round, &[]),
            )),
        );

        assert_parse(
            "[]",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(2)),
                TokenTree::Group(DelimiterKind::Square, &[]),
            )),
        );

        assert_parse(
            "{}",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(2)),
                TokenTree::Group(DelimiterKind::Curly, &[]),
            )),
        );
    }

    #[test]
    fn group_singleton() {
        assert_parse(
            "(a)",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(3)),
                TokenTree::Group(
                    DelimiterKind::Round,
                    &[Located::new(
                        TextRange::new(TextSize::from(1), TextSize::from(2)),
                        TokenTree::Ident("a"),
                    )],
                ),
            )),
        );

        assert_parse(
            "[a]",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(3)),
                TokenTree::Group(
                    DelimiterKind::Square,
                    &[Located::new(
                        TextRange::new(TextSize::from(1), TextSize::from(2)),
                        TokenTree::Ident("a"),
                    )],
                ),
            )),
        );

        assert_parse(
            "{a}",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(3)),
                TokenTree::Group(
                    DelimiterKind::Curly,
                    &[Located::new(
                        TextRange::new(TextSize::from(1), TextSize::from(2)),
                        TokenTree::Ident("a"),
                    )],
                ),
            )),
        );
    }

    #[test]
    fn group_many() {
        assert_parse(
            "(a, b)",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(6)),
                TokenTree::Group(
                    DelimiterKind::Round,
                    &[
                        Located::new(
                            TextRange::new(TextSize::from(1), TextSize::from(2)),
                            TokenTree::Ident("a"),
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(2), TextSize::from(3)),
                            TokenTree::Punct(','),
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(4), TextSize::from(5)),
                            TokenTree::Ident("b"),
                        ),
                    ],
                ),
            )),
        );

        assert_parse(
            "[a, b]",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(6)),
                TokenTree::Group(
                    DelimiterKind::Square,
                    &[
                        Located::new(
                            TextRange::new(TextSize::from(1), TextSize::from(2)),
                            TokenTree::Ident("a"),
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(2), TextSize::from(3)),
                            TokenTree::Punct(','),
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(4), TextSize::from(5)),
                            TokenTree::Ident("b"),
                        ),
                    ],
                ),
            )),
        );

        assert_parse(
            "{a, b}",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(6)),
                TokenTree::Group(
                    DelimiterKind::Curly,
                    &[
                        Located::new(
                            TextRange::new(TextSize::from(1), TextSize::from(2)),
                            TokenTree::Ident("a"),
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(2), TextSize::from(3)),
                            TokenTree::Punct(','),
                        ),
                        Located::new(
                            TextRange::new(TextSize::from(4), TextSize::from(5)),
                            TokenTree::Ident("b"),
                        ),
                    ],
                ),
            )),
        );
    }

    #[test]
    fn group_nested() {
        assert_parse(
            "([a], {b})",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(10)),
                TokenTree::Group(
                    DelimiterKind::Round,
                    &[
                        Located::new(
                            TextRange::new(TextSize::from(1), TextSize::from(4)),
                            TokenTree::Group(
                                DelimiterKind::Square,
                                &[Located::new(
                                    TextRange::new(TextSize::from(2), TextSize::from(3)),
                                    TokenTree::Ident("a"),
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
                                DelimiterKind::Curly,
                                &[Located::new(
                                    TextRange::new(TextSize::from(7), TextSize::from(8)),
                                    TokenTree::Ident("b"),
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
        assert_parse_errors(
            "\0",
            None,
            &[Error::UnknownChar {
                c: Located::new(TextRange::new(TextSize::from(0), TextSize::from(1)), '\0'),
            }],
        );
    }

    #[test]
    fn error_unclosed_open_delim() {
        assert_parse_errors(
            "(",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(1)),
                TokenTree::Group(DelimiterKind::Round, &[]),
            )),
            &[Error::UnclosedOpenDelimiter {
                delim: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    DelimiterKind::Round,
                ),
            }],
        );

        assert_parse_errors(
            "[",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(1)),
                TokenTree::Group(DelimiterKind::Square, &[]),
            )),
            &[Error::UnclosedOpenDelimiter {
                delim: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    DelimiterKind::Square,
                ),
            }],
        );

        assert_parse_errors(
            "{",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(1)),
                TokenTree::Group(DelimiterKind::Curly, &[]),
            )),
            &[Error::UnclosedOpenDelimiter {
                delim: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    DelimiterKind::Curly,
                ),
            }],
        );
    }

    #[test]
    fn error_unopened_close_delim() {
        assert_parse_errors(
            ")",
            None,
            &[Error::UnopenedCloseDelimiter {
                delim: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    DelimiterKind::Round,
                ),
            }],
        );

        assert_parse_errors(
            "]",
            None,
            &[Error::UnopenedCloseDelimiter {
                delim: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    DelimiterKind::Square,
                ),
            }],
        );

        assert_parse_errors(
            "}",
            None,
            &[Error::UnopenedCloseDelimiter {
                delim: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    DelimiterKind::Curly,
                ),
            }],
        );
    }

    #[test]
    fn error_delim_mismatch() {
        assert_parse_errors(
            "(]",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(2)),
                TokenTree::Group(DelimiterKind::Round, &[]),
            )),
            &[Error::DelimiterMismatch {
                start: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    DelimiterKind::Round,
                ),
                end: Located::new(
                    TextRange::new(TextSize::from(1), TextSize::from(2)),
                    DelimiterKind::Square,
                ),
            }],
        );

        assert_parse_errors(
            "[}",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(2)),
                TokenTree::Group(DelimiterKind::Square, &[]),
            )),
            &[Error::DelimiterMismatch {
                start: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    DelimiterKind::Square,
                ),
                end: Located::new(
                    TextRange::new(TextSize::from(1), TextSize::from(2)),
                    DelimiterKind::Curly,
                ),
            }],
        );

        assert_parse_errors(
            "{)",
            Some(Located::new(
                TextRange::new(TextSize::from(0), TextSize::from(2)),
                TokenTree::Group(DelimiterKind::Curly, &[]),
            )),
            &[Error::DelimiterMismatch {
                start: Located::new(
                    TextRange::new(TextSize::from(0), TextSize::from(1)),
                    DelimiterKind::Curly,
                ),
                end: Located::new(
                    TextRange::new(TextSize::from(1), TextSize::from(2)),
                    DelimiterKind::Round,
                ),
            }],
        );
    }
}
