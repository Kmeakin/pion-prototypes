use std::marker::PhantomData;

use pion_lexer::token::TokenKind;
use pion_lexer::T;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PionLanguage {}

pub type SyntaxNode = rowan::SyntaxNode<PionLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<PionLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<PionLanguage>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NodeKind {
    Error,
    Root,

    Module,
    DefItem,

    BoolLit,
    IntLit,

    LitExpr,
    UnderscoreExpr,
    IdentExpr,
    ParenExpr,
    AnnExpr,

    TupleLitExpr,
    ArrayLitExpr,

    MatchExpr,
    MatchCase,
    MatchGuard,

    RecordExpr,
    RecordExprField,

    IfExpr,
    ThenExpr,
    ElseExpr,

    LetExpr,
    LetInit,
    TypeAnn,

    FunLitExpr,
    FunTypeExpr,
    FunParamList,
    FunParam,

    FunArrowExpr,
    RetType,

    FieldProjExpr,
    FunCallExpr,
    MethodCallExpr,

    FunArgList,
    FunArg,

    LitPat,
    UnderscorePat,
    IdentPat,
    ParenPat,
    TupleLitPat,

    RecordLitPat,
    RecordPatField,

    OrPat,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntaxKind {
    Token(TokenKind),
    Node(NodeKind),
}

impl From<TokenKind> for SyntaxKind {
    fn from(kind: TokenKind) -> Self { Self::Token(kind) }
}
impl From<NodeKind> for SyntaxKind {
    fn from(kind: NodeKind) -> Self { Self::Node(kind) }
}

impl rowan::Language for PionLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        unsafe { std::mem::transmute::<rowan::SyntaxKind, Self::Kind>(raw) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        unsafe { std::mem::transmute::<Self::Kind, rowan::SyntaxKind>(kind) }
    }
}

/// The main trait to go from untyped `SyntaxNode`  to a typed ast. The
/// conversion itself has zero runtime cost: ast and syntax nodes have exactly
/// the same representation: a pointer to the tree root and a pointer to the
/// node itself.
pub trait AstNode: Sized {
    fn can_cast(kind: SyntaxKind) -> bool;
    fn cast(syntax: SyntaxNode) -> Option<Self>;
    fn syntax(&self) -> &SyntaxNode;
}

/// An iterator over `SyntaxNode` children of a particular AST type.
#[derive(Debug, Clone)]
pub struct AstChildren<N> {
    inner: SyntaxNodeChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildren<N> {
    fn new(parent: &SyntaxNode) -> Self {
        Self {
            inner: parent.children(),
            ph: PhantomData,
        }
    }
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;
    fn next(&mut self) -> Option<N> { self.inner.find_map(N::cast) }
}

mod support {
    use super::*;

    pub(super) fn child<N: AstNode>(parent: &SyntaxNode) -> Option<N> {
        parent.children().find_map(|node| N::cast(node))
    }

    pub(super) fn children<N: AstNode>(parent: &SyntaxNode) -> AstChildren<N> {
        AstChildren::new(parent)
    }

    pub(super) fn token(parent: &SyntaxNode, kind: TokenKind) -> Option<SyntaxToken> {
        parent
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|it| it.kind() == SyntaxKind::Token(kind))
    }
}

macro_rules! ast_node {
    ($name:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name {
            syntax: SyntaxNode,
        }
        impl AstNode for $name {
            fn can_cast(kind: SyntaxKind) -> bool {
                matches!(kind, SyntaxKind::Node(NodeKind::$name))
            }
            fn cast(syntax: SyntaxNode) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self { syntax })
                } else {
                    None
                }
            }
            fn syntax(&self) -> &SyntaxNode { &self.syntax }
        }
    };
}

macro_rules! ast_enum {
    (enum $name:ident { $($variants:ident,)* }) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $name {
            $($variants($variants)),*
        }

        impl AstNode for $name {
            fn can_cast(kind: SyntaxKind) -> bool {
                matches!(kind, SyntaxKind::Node($(NodeKind::$variants)|*))
            }
            fn cast(syntax: SyntaxNode) -> Option<Self> {
               match syntax.kind() {
                $(SyntaxKind::Node(NodeKind::$variants) => Some(Self::$variants($variants { syntax })),)*
                _ => None,
               }
            }
            fn syntax(&self) -> &SyntaxNode {
                match self {
                    $(Self::$variants(it) => &it.syntax,)*
                }
            }
        }

        $(
            impl From<$variants> for $name{
                fn from(node: $variants) -> Self {
                    Self::$variants(node)
                }
            }
        )*

        $(
            ast_node!{$variants}
        )*
    };
}

ast_node!(Root);
impl Root {
    pub fn module(&self) -> Option<Module> { support::child(&self.syntax) }
}

ast_node!(Module);
impl Module {
    pub fn items(&self) -> AstChildren<Item> { support::children(&self.syntax) }
}

ast_enum!(
    enum Item {
        DefItem,
    }
);

impl DefItem {
    pub fn def_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![_]) }
    pub fn ident_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![ident]) }
    pub fn type_ann(&self) -> Option<TypeAnn> { support::child(&self.syntax) }
    pub fn eq_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![=]) }
    pub fn body(&self) -> Option<Expr> { support::child(&self.syntax) }
    pub fn semicolon_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![;]) }
}

ast_enum!(
    enum Lit {
        BoolLit,
        IntLit,
    }
);
impl BoolLit {
    pub fn true_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![true]) }
    pub fn false_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![false]) }
}
impl IntLit {
    pub fn dec_int_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![dec_int]) }
    pub fn bin_int_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![bin_int]) }
    pub fn hex_int_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![hex_int]) }
}

ast_enum!(
    enum Expr {
        LitExpr,
        UnderscoreExpr,
        IdentExpr,
        ParenExpr,
        AnnExpr,
        TupleLitExpr,
        ArrayLitExpr,
        MatchExpr,
        RecordExpr,
        IfExpr,
        LetExpr,
        FunLitExpr,
        FunTypeExpr,
        FunArrowExpr,
        FieldProjExpr,
        FunCallExpr,
        MethodCallExpr,
    }
);

impl LitExpr {
    pub fn lit(&self) -> Option<Lit> { support::child(&self.syntax) }
}
impl UnderscoreExpr {
    pub fn underscore_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![_]) }
}
impl IdentExpr {
    pub fn ident_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![ident]) }
}
impl ParenExpr {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['(']) }
    pub fn expr(&self) -> Option<Expr> { support::child(&self.syntax) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![')']) }
}
impl AnnExpr {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['(']) }
    pub fn expr(&self) -> Option<Expr> { support::child(&self.syntax) }
    pub fn type_ann(&self) -> Option<TypeAnn> { support::child(&self.syntax) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![')']) }
}
impl TupleLitExpr {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['(']) }
    pub fn exprs(&self) -> AstChildren<Expr> { support::children(&self.syntax) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![')']) }
}
impl ArrayLitExpr {
    pub fn l_square_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['[']) }
    pub fn exprs(&self) -> AstChildren<Expr> { support::children(&self.syntax) }
    pub fn r_square_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![']']) }
}
impl MatchExpr {
    pub fn match_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![match]) }
    pub fn scrut(&self) -> Option<Expr> { support::child(&self.syntax) }
    pub fn l_curly_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['{']) }
    pub fn cases(&self) -> AstChildren<MatchCase> { support::children(&self.syntax) }
    pub fn r_curly_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['}']) }
}
impl RecordExpr {
    pub fn l_curly_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['{']) }
    pub fn fields(&self) -> AstChildren<RecordExprField> { support::children(&self.syntax) }
    pub fn r_curly_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['}']) }
}
impl IfExpr {
    pub fn if_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![if]) }
    pub fn scrut(&self) -> Option<Expr> { support::child(&self.syntax) }
    pub fn then_expr(&self) -> Option<ThenExpr> { support::child(&self.syntax) }
    pub fn else_expr(&self) -> Option<ElseExpr> { support::child(&self.syntax) }
}
ast_node!(ThenExpr);
impl ThenExpr {
    pub fn then_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![then]) }
    pub fn expr(&self) -> Option<Expr> { support::child(&self.syntax) }
}
ast_node!(ElseExpr);
impl ElseExpr {
    pub fn else_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![else]) }
    pub fn expr(&self) -> Option<Expr> { support::child(&self.syntax) }
}

impl LetExpr {
    pub fn let_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![let]) }
    pub fn pat(&self) -> Option<Pat> { support::child(&self.syntax) }
    pub fn type_ann(&self) -> Option<TypeAnn> { support::child(&self.syntax) }
    pub fn init(&self) -> Option<LetInit> { support::child(&self.syntax) }
    pub fn semicolon_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![;]) }
    pub fn body(&self) -> Option<Expr> { support::child(&self.syntax) }
}
impl FunLitExpr {
    pub fn fun_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![fun]) }
    pub fn param_list(&self) -> Option<FunParamList> { support::child(&self.syntax) }
    pub fn fat_arrow_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![=>]) }
    pub fn body(&self) -> Option<Expr> { support::child(&self.syntax) }
}
impl FunTypeExpr {
    pub fn fun_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![fun]) }
    pub fn param_list(&self) -> Option<FunParamList> { support::child(&self.syntax) }
    pub fn thin_arrow_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![->]) }
    pub fn body(&self) -> Option<Expr> { support::child(&self.syntax) }
}
impl FunArrowExpr {
    pub fn lhs(&self) -> Option<Expr> { support::child(&self.syntax) }
    pub fn rhs(&self) -> Option<RetType> { support::child(&self.syntax) }
}
impl FieldProjExpr {
    pub fn scrut(&self) -> Option<Expr> { support::child(&self.syntax) }
    pub fn dot(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![.]) }
    pub fn ident_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![ident]) }
}
impl FunCallExpr {
    pub fn fun(&self) -> Option<Expr> { support::child(&self.syntax) }
    pub fn arg_list(&self) -> Option<FunArgList> { support::child(&self.syntax) }
}
impl MethodCallExpr {
    pub fn scrut(&self) -> Option<Expr> { support::child(&self.syntax) }
    pub fn dot(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![.]) }
    pub fn ident_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![ident]) }
    pub fn arg_list(&self) -> Option<FunArgList> { support::child(&self.syntax) }
}

ast_node!(RecordExprField);
impl RecordExprField {
    pub fn ident_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![ident]) }
    pub fn eq_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![=]) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![:]) }
    pub fn expr(&self) -> Option<Expr> { support::child(&self.syntax) }
}

ast_node!(MatchCase);
impl MatchCase {
    pub fn pat(&self) -> Option<Pat> { support::child(&self.syntax) }
    pub fn guard(&self) -> Option<MatchGuard> { support::child(&self.syntax) }
    pub fn fat_arrow_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![=>]) }
    pub fn expr(&self) -> Option<Expr> { support::child(&self.syntax) }
}

ast_node!(MatchGuard);
impl MatchGuard {
    pub fn if_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![if]) }
    pub fn expr(&self) -> Option<Expr> { support::child(&self.syntax) }
}

ast_node!(FunParamList);
impl FunParamList {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['(']) }
    pub fn params(&self) -> AstChildren<FunParam> { support::children(&self.syntax) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![')']) }
}

ast_node!(FunParam);
impl FunParam {
    pub fn at_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![@]) }
    pub fn pat(&self) -> Option<Pat> { support::child(&self.syntax) }
    pub fn type_ann(&self) -> Option<TypeAnn> { support::child(&self.syntax) }
}

ast_node!(TypeAnn);
impl TypeAnn {
    pub fn colon_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![:]) }
    pub fn expr(&self) -> Option<Expr> { support::child(&self.syntax) }
}

ast_node!(LetInit);
impl LetInit {
    pub fn eq_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![=]) }
    pub fn expr(&self) -> Option<Expr> { support::child(&self.syntax) }
}

ast_node!(RetType);
impl RetType {
    pub fn thin_arrow_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![->]) }
    pub fn expr(&self) -> Option<Expr> { support::child(&self.syntax) }
}

ast_node!(FunArgList);
impl FunArgList {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['(']) }
    pub fn args(&self) -> AstChildren<FunArg> { support::children(&self.syntax) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![')']) }
}

ast_node!(FunArg);
impl FunArg {
    pub fn at_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![@]) }
    pub fn expr(&self) -> Option<Expr> { support::child(&self.syntax) }
}

ast_enum!(
    enum Pat {
        LitPat,
        UnderscorePat,
        IdentPat,
        ParenPat,
        TupleLitPat,
        RecordLitPat,
        OrPat,
    }
);

impl LitPat {
    pub fn lit(&self) -> Option<Lit> { support::child(&self.syntax) }
}
impl UnderscorePat {
    pub fn underscore_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![_]) }
}
impl IdentPat {
    pub fn ident_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![ident]) }
}
impl ParenPat {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['(']) }
    pub fn pat(&self) -> Option<Pat> { support::child(&self.syntax) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![')']) }
}
impl TupleLitPat {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['(']) }
    pub fn pats(&self) -> AstChildren<Pat> { support::children(&self.syntax) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![')']) }
}
impl RecordLitPat {
    pub fn l_curly_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['{']) }
    pub fn fields(&self) -> AstChildren<RecordPatField> { support::children(&self.syntax) }
    pub fn r_curly_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T!['}']) }
}
ast_node!(RecordPatField);
impl RecordPatField {
    pub fn ident_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![ident]) }
    pub fn eq_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, T![=]) }
    pub fn pat(&self) -> Option<Pat> { support::child(&self.syntax) }
}

impl OrPat {
    pub fn pats(&self) -> AstChildren<Pat> { support::children(&self.syntax) }
}
