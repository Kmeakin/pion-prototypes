use std::fmt;
use std::ops::Range;

use pion_lexer::token::TokenKind;
use pion_utils::location::ByteSpan;
use pion_utils::numeric_conversions::{TruncateFrom, ZeroExtendFrom};

use crate::syntax::NodeKind;

#[derive(Clone)]
pub struct SyntaxTree {
    text: String,
    data: Vec<TreeDatum>,
}

impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.root().fmt(f) }
}

#[derive(Debug, Copy, Clone)]
pub enum ParseEvent {
    Token {
        kind: TokenKind,
        span_len: u32,
    },
    Node {
        kind: NodeKind,
        num_descendents: u32,
    },
}

#[derive(Debug, Copy, Clone)]
enum TreeDatum {
    Token {
        kind: TokenKind,
        span: ByteSpan,
    },
    Node {
        kind: NodeKind,
        parent_index: u32,
        /// Number of descendents, including self
        num_descendents: u32,
    },
}

#[derive(Copy, Clone)]
pub struct SyntaxToken<'tree> {
    tree: &'tree SyntaxTree,
    kind: TokenKind,
    span: ByteSpan,
    self_index: u32,
    parent_index: u32,
}

#[derive(Copy, Clone)]
pub struct SyntaxNode<'tree> {
    tree: &'tree SyntaxTree,
    kind: NodeKind,
    self_index: u32,
    parent_index: u32,
    /// Number of descendents, including self
    num_descendents: u32,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NodeOrToken<Node, Token> {
    Node(Node),
    Token(Token),
}

pub type SyntaxElement<'tree> = NodeOrToken<SyntaxNode<'tree>, SyntaxToken<'tree>>;

impl<Node, Token> NodeOrToken<Node, Token> {
    pub fn into_node(self) -> Option<Node> {
        match self {
            Self::Node(node) => Some(node),
            Self::Token(_) => None,
        }
    }

    pub fn into_token(self) -> Option<Token> {
        match self {
            Self::Token(token) => Some(token),
            Self::Node(_) => None,
        }
    }
}

impl SyntaxTree {
    pub(crate) fn from_events(text: String, events: &[ParseEvent]) -> Self {
        let Some(root) = events.last() else {
            unreachable!()
        };

        let mut data = Vec::with_capacity(events.len());
        let mut stack = vec![(root, 0, u32::truncate_from(events.len() - 1))];
        let mut span_start = 0;

        while let Some((event, parent_index, self_index)) = stack.pop() {
            let datum = match event {
                ParseEvent::Token { kind, span_len } => {
                    let token = TreeDatum::Token {
                        kind: *kind,
                        span: ByteSpan::from(span_start..span_start + *span_len),
                    };
                    span_start += span_len;
                    token
                }
                ParseEvent::Node {
                    kind,
                    num_descendents,
                } => TreeDatum::Node {
                    kind: *kind,
                    parent_index,
                    num_descendents: *num_descendents,
                },
            };
            data.push(datum);

            // Has children, so we descend. We append the children in RPO here as
            // well because they will get reversed when popped off the stack.
            #[allow(clippy::as_conversions)]
            #[allow(clippy::cast_possible_wrap)]
            #[allow(clippy::cast_sign_loss)]
            if let ParseEvent::Node {
                num_descendents, ..
            } = event
            {
                let parent_index: u32 = u32::truncate_from(data.len().saturating_sub(1));
                let len: i32 = *num_descendents as i32;
                let start_index: i32 = self_index as i32 - len;
                let end_index: i32 = self_index as i32 - 1;
                let mut self_index: i32 = end_index;

                while self_index > start_index {
                    let event = &events[self_index as usize];
                    stack.push((event, parent_index, self_index as u32));
                    match event {
                        ParseEvent::Token { .. } => self_index -= 1,
                        ParseEvent::Node {
                            num_descendents, ..
                        } => {
                            let len = *num_descendents as i32;
                            self_index -= len;
                        }
                    }
                }
            }
        }

        debug_assert_eq!(data.len(), events.len());
        Self { text, data }
    }

    pub fn root(&self) -> SyntaxNode<'_> {
        match self.data.first() {
            None | Some(TreeDatum::Token { .. }) => unreachable!(),
            Some(TreeDatum::Node {
                kind,
                num_descendents,
                parent_index,
            }) => {
                debug_assert_eq!(*kind, NodeKind::Root);
                debug_assert_eq!(*parent_index, 0);
                SyntaxNode {
                    tree: self,
                    kind: NodeKind::Root,
                    self_index: 0,
                    parent_index: 0,
                    num_descendents: *num_descendents,
                }
            }
        }
    }

    fn get_data(&self, index: u32) -> Option<&TreeDatum> { self.data.get(usize::zext_from(index)) }
}

impl<'tree> SyntaxToken<'tree> {
    pub fn kind(&self) -> TokenKind { self.kind }
    pub fn span(&self) -> ByteSpan { self.span }
    pub fn text(&self) -> &'tree str { &self.tree.text[self.span()] }
    pub fn parent(&self) -> SyntaxNode<'tree> {
        match self.tree.get_data(self.parent_index) {
            None | Some(TreeDatum::Token { .. }) => unreachable!(),
            Some(TreeDatum::Node {
                kind,
                parent_index,
                num_descendents,
            }) => SyntaxNode {
                tree: self.tree,
                kind: *kind,
                self_index: self.parent_index,
                parent_index: *parent_index,
                num_descendents: *num_descendents,
            },
        }
    }
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode<'tree>> {
        std::iter::successors(Some(self.parent()), SyntaxNode::parent)
    }

    // TODO: tests
    pub fn left_sibling(&self) -> Option<SyntaxElement<'tree>> {
        if !self
            .parent()
            .descendants_range()
            .contains(&(self.self_index - 1))
        {
            return None;
        }

        match self.tree.get_data(self.self_index - 1) {
            None => unreachable!(),
            Some(TreeDatum::Token { kind, span }) => Some(SyntaxElement::Token(SyntaxToken {
                tree: self.tree,
                kind: *kind,
                span: *span,
                self_index: self.self_index - 1,
                parent_index: self.parent_index,
            })),
            Some(TreeDatum::Node {
                kind,
                parent_index,
                num_descendents,
            }) => {
                debug_assert_eq!(*parent_index, self.parent_index);
                Some(SyntaxElement::Node(SyntaxNode {
                    tree: self.tree,
                    kind: *kind,
                    self_index: self.self_index - 1,
                    parent_index: *parent_index,
                    num_descendents: *num_descendents,
                }))
            }
        }
    }

    // TODO: tests
    pub fn right_sibling(&self) -> Option<SyntaxElement<'tree>> {
        if !self
            .parent()
            .descendants_range()
            .contains(&(self.self_index + 1))
        {
            return None;
        }

        match self.tree.get_data(self.self_index + 1) {
            None => unreachable!(),
            Some(TreeDatum::Token { kind, span }) => Some(SyntaxElement::Token(SyntaxToken {
                tree: self.tree,
                kind: *kind,
                span: *span,
                self_index: self.self_index + 1,
                parent_index: self.parent_index,
            })),
            Some(TreeDatum::Node {
                kind,
                parent_index,
                num_descendents,
            }) => {
                debug_assert_eq!(*parent_index, self.parent_index);
                Some(SyntaxElement::Node(SyntaxNode {
                    tree: self.tree,
                    kind: *kind,
                    self_index: self.self_index - 1,
                    parent_index: *parent_index,
                    num_descendents: *num_descendents,
                }))
            }
        }
    }

    fn print(&self, writer: &mut impl fmt::Write, depth: usize) -> fmt::Result {
        writeln!(
            writer,
            "{}{:?} {:?} {:?}",
            " ".repeat(depth),
            self.span(),
            self.kind(),
            self.text()
        )
    }
}

impl<'tree> fmt::Debug for SyntaxToken<'tree> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.print(f, 0) }
}

impl<'tree> SyntaxNode<'tree> {
    pub fn kind(&self) -> NodeKind { self.kind }
    pub fn span(&self) -> ByteSpan {
        match Option::zip(self.first_token(), self.last_token()) {
            None => ByteSpan::default(),
            Some((first_token, last_token)) => {
                ByteSpan::new(first_token.span().start, last_token.span().end)
            }
        }
    }
    pub fn text(&self) -> &'tree str { &self.tree.text[self.span()] }
    pub fn parent(&self) -> Option<SyntaxNode<'tree>> {
        if self.self_index == 0 {
            None
        } else {
            match self.tree.get_data(self.parent_index) {
                None | Some(TreeDatum::Token { .. }) => unreachable!(),
                Some(TreeDatum::Node {
                    kind,
                    parent_index,
                    num_descendents,
                }) => Some(SyntaxNode {
                    tree: self.tree,
                    kind: *kind,
                    self_index: self.parent_index,
                    parent_index: *parent_index,
                    num_descendents: *num_descendents,
                }),
            }
        }
    }

    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode<'tree>> {
        std::iter::successors(self.parent(), SyntaxNode::parent)
    }

    fn descendants_range(&self) -> Range<u32> {
        let start = self.self_index + 1;
        let len = self.num_descendents - 1;
        (start)..(start + len)
    }

    pub fn descendants(
        self,
    ) -> impl ExactSizeIterator<Item = SyntaxElement<'tree>>
           + DoubleEndedIterator<Item = SyntaxElement<'tree>> {
        let range = self.descendants_range();
        let mut parent_index = self.self_index;
        self.tree.data[Range::<usize>::zext_from(range.clone())]
            .iter()
            .zip(range)
            .map(move |(datum, self_index)| match datum {
                TreeDatum::Token { kind, span } => SyntaxElement::Token(SyntaxToken {
                    tree: self.tree,
                    kind: *kind,
                    span: *span,
                    self_index,
                    parent_index,
                }),
                TreeDatum::Node {
                    kind,
                    num_descendents,
                    parent_index: parent,
                } => {
                    parent_index = *parent;
                    SyntaxElement::Node(SyntaxNode {
                        tree: self.tree,
                        kind: *kind,
                        self_index,
                        parent_index,
                        num_descendents: *num_descendents,
                    })
                }
            })
    }

    pub fn tokens(&self) -> impl DoubleEndedIterator<Item = SyntaxToken<'tree>> + '_ {
        self.descendants().filter_map(SyntaxElement::into_token)
    }

    pub fn first_token(&self) -> Option<SyntaxToken<'tree>> { self.tokens().next() }
    pub fn last_token(&self) -> Option<SyntaxToken<'tree>> { self.tokens().next_back() }

    pub fn children(self) -> impl Iterator<Item = SyntaxElement<'tree>> {
        let parent_index = self.self_index;
        let Range { start, end } = self.descendants_range();
        let mut index = start;
        std::iter::from_fn(move || {
            if index < end {
                let datum = self.tree.get_data(index);
                let self_index = index;
                match datum {
                    None => unreachable!(),
                    Some(TreeDatum::Token { kind, span }) => {
                        index += 1;
                        Some(SyntaxElement::Token(SyntaxToken {
                            tree: self.tree,
                            kind: *kind,
                            span: *span,
                            self_index,
                            parent_index,
                        }))
                    }
                    Some(TreeDatum::Node {
                        kind,
                        num_descendents,
                        parent_index: parent,
                    }) => {
                        debug_assert_eq!(parent_index, *parent);
                        index += *num_descendents;
                        Some(SyntaxElement::Node(SyntaxNode {
                            tree: self.tree,
                            kind: *kind,
                            self_index,
                            parent_index,
                            num_descendents: *num_descendents,
                        }))
                    }
                }
            } else {
                None
            }
        })
    }

    pub fn child_nodes(self) -> impl Iterator<Item = SyntaxNode<'tree>> {
        self.children().filter_map(SyntaxElement::into_node)
    }

    pub fn child_tokens(&self) -> impl Iterator<Item = SyntaxToken<'tree>> + '_ {
        self.children().filter_map(SyntaxElement::into_token)
    }

    fn print(&self, writer: &mut impl fmt::Write, mut depth: usize) -> fmt::Result {
        writeln!(writer, "{}{:?}", " ".repeat(depth), self.kind())?;
        depth += 2;
        for child in self.children() {
            match child {
                NodeOrToken::Node(node) => node.print(writer, depth)?,
                NodeOrToken::Token(token) => token.print(writer, depth)?,
            }
        }
        Ok(())
    }
}

impl<'tree> fmt::Debug for SyntaxNode<'tree> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.print(f, 0) }
}

#[cfg(test)]
mod tests {
    use pion_lexer::token::TokenKind;
    use pion_lexer::T;
    use pion_utils::location::ByteSpan;

    use super::*;
    use crate::syntax::NodeKind;

    #[track_caller]
    fn check_token(
        token: &SyntaxToken,
        kind: TokenKind,
        self_index: u32,
        parent_index: u32,
        span: ByteSpan,
        text: &str,
    ) {
        assert_eq!(token.kind(), kind);
        assert_eq!(token.self_index, self_index);
        assert_eq!(token.parent_index, parent_index);
        assert_eq!(token.span(), span);
        assert_eq!(token.text(), text);
    }

    #[track_caller]
    fn check_node(
        node: &SyntaxNode,
        kind: NodeKind,
        self_index: u32,
        parent_index: u32,
        num_descendents: u32,
        span: ByteSpan,
        text: &str,
    ) {
        assert_eq!(node.kind(), kind);
        assert_eq!(node.self_index, self_index);
        assert_eq!(node.parent_index, parent_index);
        assert_eq!(node.num_descendents, num_descendents);
        assert_eq!(node.span(), span);
        assert_eq!(node.text(), text);
    }

    #[test]
    fn descendents() {
        let text = "[a, b]";
        let (tree, _) = crate::parse_expr(text);

        let root = tree.root();
        check_node(
            &root,
            NodeKind::Root,
            0,
            0,
            10,
            ByteSpan::from(0..6),
            "[a, b]",
        );

        let mut descendents = root.descendants();
        let elem = descendents
            .next()
            .expect("expected elem")
            .into_node()
            .expect("expected node");
        check_node(
            &elem,
            NodeKind::ArrayLitExpr,
            1,
            0,
            9,
            ByteSpan::from(0..6),
            "[a, b]",
        );

        let elem = descendents
            .next()
            .expect("expected elem")
            .into_token()
            .expect("expected token");
        check_token(&elem, T!['['], 2, 0, ByteSpan::from(0..1), "[");

        let elem = descendents
            .next()
            .expect("expected elem")
            .into_node()
            .expect("expected node");
        check_node(
            &elem,
            NodeKind::IdentExpr,
            3,
            1,
            2,
            ByteSpan::from(1..2),
            "a",
        );

        let elem = descendents
            .next()
            .expect("expected elem")
            .into_token()
            .expect("expected token");
        check_token(&elem, T![ident], 4, 1, ByteSpan::from(1..2), "a");

        let elem = descendents
            .next()
            .expect("expected elem")
            .into_token()
            .expect("expected token");
        check_token(&elem, T![,], 5, 1, ByteSpan::from(2..3), ",");

        let elem = descendents
            .next()
            .expect("expected elem")
            .into_token()
            .expect("expected token");
        check_token(
            &elem,
            TokenKind::Whitespace,
            6,
            1,
            ByteSpan::from(3..4),
            " ",
        );

        let elem = descendents
            .next()
            .expect("expected elem")
            .into_node()
            .expect("expected node");
        check_node(
            &elem,
            NodeKind::IdentExpr,
            7,
            1,
            2,
            ByteSpan::from(4..5),
            "b",
        );

        let elem = descendents
            .next()
            .expect("expected elem")
            .into_token()
            .expect("expected token");
        check_token(&elem, T![ident], 8, 1, ByteSpan::from(4..5), "b");

        let elem = descendents
            .next()
            .expect("expected elem")
            .into_token()
            .expect("expected token");
        check_token(&elem, T![']'], 9, 1, ByteSpan::from(5..6), "]");

        let elem = descendents.next();
        assert!(elem.is_none());
    }

    #[test]
    fn children() {
        let text = "[a, b]";
        let (tree, _) = crate::parse_expr(text);

        let root = tree.root();
        check_node(
            &root,
            NodeKind::Root,
            0,
            0,
            10,
            ByteSpan::from(0..6),
            "[a, b]",
        );

        let mut children = root.children();

        let child = children
            .next()
            .expect("expected child")
            .into_node()
            .expect("expected node");
        check_node(
            &child,
            NodeKind::ArrayLitExpr,
            1,
            0,
            9,
            ByteSpan::from(0..6),
            "[a, b]",
        );

        {
            let mut children = child.children();
            let child = children
                .next()
                .expect("expected child")
                .into_token()
                .expect("expected token");
            check_token(&child, T!['['], 2, 1, ByteSpan::from(0..1), "[");

            let child = children
                .next()
                .expect("expected child")
                .into_node()
                .expect("expected node");

            check_node(
                &child,
                NodeKind::IdentExpr,
                3,
                1,
                2,
                ByteSpan::from(1..2),
                "a",
            );

            let child = children
                .next()
                .expect("expected elem")
                .into_token()
                .expect("expected token");
            check_token(&child, T![,], 5, 1, ByteSpan::from(2..3), ",");

            let child = children
                .next()
                .expect("expected elem")
                .into_token()
                .expect("expected token");
            check_token(
                &child,
                TokenKind::Whitespace,
                6,
                1,
                ByteSpan::from(3..4),
                " ",
            );

            let child = children
                .next()
                .expect("expected child")
                .into_node()
                .expect("expected node");
            check_node(
                &child,
                NodeKind::IdentExpr,
                7,
                1,
                2,
                ByteSpan::from(4..5),
                "b",
            );

            let child = children
                .next()
                .expect("expected child")
                .into_token()
                .expect("expected token");
            check_token(&child, T![']'], 9, 1, ByteSpan::from(5..6), "]");

            let child = children.next();
            assert!(child.is_none());
        }

        assert!(children.next().is_none());
    }

    #[test]
    fn ancestors() {
        let text = "[a, b]";
        let (tree, _) = crate::parse_expr(text);
        let root = tree.root();
        check_node(
            &root,
            NodeKind::Root,
            0,
            0,
            10,
            ByteSpan::from(0..6),
            "[a, b]",
        );

        let parent = root.parent();
        assert!(parent.is_none());

        let mut children = root.children();

        let child = children
            .next()
            .expect("expected child")
            .into_node()
            .expect("expected node");
        check_node(
            &child,
            NodeKind::ArrayLitExpr,
            1,
            0,
            9,
            ByteSpan::from(0..6),
            "[a, b]",
        );

        let parent = child.parent().expect("expected parent");
        check_node(
            &parent,
            NodeKind::Root,
            0,
            0,
            10,
            ByteSpan::from(0..6),
            "[a, b]",
        );

        let mut children = child.children();
        let child = children
            .next()
            .expect("expected child")
            .into_token()
            .expect("expected token");
        check_token(&child, T!['['], 2, 1, ByteSpan::from(0..1), "[");
        let parent = child.parent();
        check_node(
            &parent,
            NodeKind::ArrayLitExpr,
            1,
            0,
            9,
            ByteSpan::from(0..6),
            "[a, b]",
        );
    }
}
