use pion_lexer::T;

use crate::tree::{SyntaxNode, SyntaxToken};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NodeKind {
    Root,
    Error,

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

pub trait CstNode<'tree>: Sized + 'tree {
    fn cast(node: SyntaxNode<'tree>) -> Option<Self>;
    fn syntax(&self) -> SyntaxNode<'tree>;
}

mod support {
    use pion_lexer::token::TokenKind;

    use super::*;
    use crate::tree::SyntaxToken;

    pub(super) fn child<'tree, N: CstNode<'tree>>(parent: SyntaxNode<'tree>) -> Option<N> {
        parent.child_nodes().find_map(|node| N::cast(node))
    }

    pub(super) fn children<'tree, N: CstNode<'tree>>(
        parent: SyntaxNode<'tree>,
    ) -> impl Iterator<Item = N> + 'tree {
        parent.child_nodes().filter_map(N::cast)
    }

    #[allow(clippy::needless_lifetimes)]
    pub(super) fn token<'tree>(
        parent: SyntaxNode<'tree>,
        kind: TokenKind,
    ) -> Option<SyntaxToken<'tree>> {
        parent.child_tokens().find(|it| it.kind() == kind)
    }
}

macro_rules! cst_node {
    ($name:ident) => {
        #[derive(Debug, Copy, Clone)]
        pub struct $name<'tree> {
            syntax: SyntaxNode<'tree>,
        }
        impl<'tree> CstNode<'tree> for $name<'tree> {
            fn cast(syntax: SyntaxNode<'tree>) -> Option<Self> {
                if matches!(syntax.kind(), NodeKind::$name) {
                    Some(Self { syntax })
                } else {
                    None
                }
            }
            fn syntax(&self) -> SyntaxNode<'tree> { self.syntax }
        }
    };
}

macro_rules! cst_enum {
    (enum $name:ident { $($variants:ident,)* }) => {
        #[derive(Debug, Copy, Clone)]
        pub enum $name<'tree> {
            $($variants($variants<'tree>)),*
        }

        impl<'tree> CstNode<'tree> for $name<'tree> {
            fn cast(syntax: SyntaxNode<'tree>) -> Option<Self> {
               match syntax.kind() {
                $(NodeKind::$variants => Some(Self::$variants($variants { syntax })),)*
                _ => None,
               }
            }
            fn syntax<>(&self) -> SyntaxNode<'tree> {
                match self {
                    $(Self::$variants(it) => it.syntax,)*
                }
            }
        }

        $(
            impl<'tree> From<$variants<'tree>> for $name<'tree>{
                fn from(node: $variants<'tree>) -> Self {
                    Self::$variants(node)
                }
            }
        )*

        $(
            cst_node!{$variants}
        )*
    };
}

cst_node!(Root);
impl<'tree> Root<'tree> {
    pub fn module(self) -> Option<Module<'tree>> { support::child(self.syntax) }
}

cst_node!(Module);
impl<'tree> Module<'tree> {
    pub fn items(self) -> impl Iterator<Item = Item<'tree>> { support::children(self.syntax) }
}

cst_enum!(
    enum Item {
        DefItem,
    }
);

impl<'tree> DefItem<'tree> {
    pub fn def_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![_]) }
    pub fn ident_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![ident])
    }
    pub fn type_ann(self) -> Option<TypeAnn<'tree>> { support::child(self.syntax) }
    pub fn eq_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![=]) }
    pub fn body(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
    pub fn semicolon_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![;])
    }
}

cst_enum!(
    enum Lit {
        BoolLit,
        IntLit,
    }
);
impl<'tree> BoolLit<'tree> {
    pub fn true_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![true]) }
    pub fn false_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![false])
    }
}
impl<'tree> IntLit<'tree> {
    pub fn dec_int_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![dec_int])
    }
    pub fn bin_int_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![bin_int])
    }
    pub fn hex_int_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![hex_int])
    }
}

cst_enum!(
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

impl<'tree> LitExpr<'tree> {
    pub fn lit(self) -> Option<Lit<'tree>> { support::child(self.syntax) }
}
impl<'tree> UnderscoreExpr<'tree> {
    pub fn underscore_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![_])
    }
}
impl<'tree> IdentExpr<'tree> {
    pub fn ident_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![ident])
    }
}
impl<'tree> ParenExpr<'tree> {
    pub fn l_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['('])
    }
    pub fn expr(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
    pub fn r_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![')'])
    }
}
impl<'tree> AnnExpr<'tree> {
    pub fn l_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['('])
    }
    pub fn expr(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
    pub fn type_ann(self) -> Option<TypeAnn<'tree>> { support::child(self.syntax) }
    pub fn r_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![')'])
    }
}
impl<'tree> TupleLitExpr<'tree> {
    pub fn l_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['('])
    }
    pub fn exprs(self) -> impl Iterator<Item = Expr<'tree>> { support::children(self.syntax) }
    pub fn r_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![')'])
    }
}
impl<'tree> ArrayLitExpr<'tree> {
    pub fn l_square_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['['])
    }
    pub fn exprs(self) -> impl Iterator<Item = Expr<'tree>> { support::children(self.syntax) }
    pub fn r_square_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![']'])
    }
}
impl<'tree> MatchExpr<'tree> {
    pub fn match_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![match])
    }
    pub fn scrut(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
    pub fn l_curly_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['{'])
    }
    pub fn cases(self) -> impl Iterator<Item = MatchCase<'tree>> { support::children(self.syntax) }
    pub fn r_curly_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['}'])
    }
}
impl<'tree> RecordExpr<'tree> {
    pub fn l_curly_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['{'])
    }
    pub fn fields(self) -> impl Iterator<Item = RecordExprField<'tree>> {
        support::children(self.syntax)
    }
    pub fn r_curly_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['}'])
    }
}
impl<'tree> IfExpr<'tree> {
    pub fn if_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![if]) }
    pub fn scrut(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
    pub fn then_expr(self) -> Option<ThenExpr<'tree>> { support::child(self.syntax) }
    pub fn else_expr(self) -> Option<ElseExpr<'tree>> { support::child(self.syntax) }
}
cst_node!(ThenExpr);
impl<'tree> ThenExpr<'tree> {
    pub fn then_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![then]) }
    pub fn expr(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
}
cst_node!(ElseExpr);
impl<'tree> ElseExpr<'tree> {
    pub fn else_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![else]) }
    pub fn expr(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
}

impl<'tree> LetExpr<'tree> {
    pub fn let_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![let]) }
    pub fn pat(self) -> Option<Pat<'tree>> { support::child(self.syntax) }
    pub fn type_ann(self) -> Option<TypeAnn<'tree>> { support::child(self.syntax) }
    pub fn init(self) -> Option<LetInit<'tree>> { support::child(self.syntax) }
    pub fn semicolon_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![;])
    }
    pub fn body(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
}
impl<'tree> FunLitExpr<'tree> {
    pub fn fun_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![fun]) }
    pub fn param_list(self) -> Option<FunParamList<'tree>> { support::child(self.syntax) }
    pub fn fat_arrow_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![=>])
    }
    pub fn body(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
}
impl<'tree> FunTypeExpr<'tree> {
    pub fn fun_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![fun]) }
    pub fn param_list(self) -> Option<FunParamList<'tree>> { support::child(self.syntax) }
    pub fn thin_arrow_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![->])
    }
    pub fn body(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
}
impl<'tree> FunArrowExpr<'tree> {
    pub fn lhs(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
    pub fn rhs(self) -> Option<RetType<'tree>> { support::child(self.syntax) }
}
impl<'tree> FieldProjExpr<'tree> {
    pub fn scrut(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
    pub fn dot(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![.]) }
    pub fn ident_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![ident])
    }
}
impl<'tree> FunCallExpr<'tree> {
    pub fn fun(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
    pub fn arg_list(self) -> Option<FunArgList<'tree>> { support::child(self.syntax) }
}
impl<'tree> MethodCallExpr<'tree> {
    pub fn scrut(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
    pub fn dot(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![.]) }
    pub fn ident_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![ident])
    }
    pub fn arg_list(self) -> Option<FunArgList<'tree>> { support::child(self.syntax) }
}

cst_node!(RecordExprField);
impl<'tree> RecordExprField<'tree> {
    pub fn ident_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![ident])
    }
    pub fn eq_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![=]) }
    pub fn colon_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![:]) }
    pub fn expr(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
}

cst_node!(MatchCase);
impl<'tree> MatchCase<'tree> {
    pub fn pat(self) -> Option<Pat<'tree>> { support::child(self.syntax) }
    pub fn guard(self) -> Option<MatchGuard<'tree>> { support::child(self.syntax) }
    pub fn fat_arrow_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![=>])
    }
    pub fn expr(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
}

cst_node!(MatchGuard);
impl<'tree> MatchGuard<'tree> {
    pub fn if_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![if]) }
    pub fn expr(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
}

cst_node!(FunParamList);
impl<'tree> FunParamList<'tree> {
    pub fn l_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['('])
    }
    pub fn params(self) -> impl Iterator<Item = FunParam<'tree>> { support::children(self.syntax) }
    pub fn r_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![')'])
    }
}

cst_node!(FunParam);
impl<'tree> FunParam<'tree> {
    pub fn at_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![@]) }
    pub fn pat(self) -> Option<Pat<'tree>> { support::child(self.syntax) }
    pub fn type_ann(self) -> Option<TypeAnn<'tree>> { support::child(self.syntax) }
}

cst_node!(TypeAnn);
impl<'tree> TypeAnn<'tree> {
    pub fn colon_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![:]) }
    pub fn expr(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
}

cst_node!(LetInit);
impl<'tree> LetInit<'tree> {
    pub fn eq_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![=]) }
    pub fn expr(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
}

cst_node!(RetType);
impl<'tree> RetType<'tree> {
    pub fn thin_arrow_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![->])
    }
    pub fn expr(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
}

cst_node!(FunArgList);
impl<'tree> FunArgList<'tree> {
    pub fn l_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['('])
    }
    pub fn args(self) -> impl Iterator<Item = FunArg<'tree>> { support::children(self.syntax) }
    pub fn r_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![')'])
    }
}

cst_node!(FunArg);
impl<'tree> FunArg<'tree> {
    pub fn at_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![@]) }
    pub fn expr(self) -> Option<Expr<'tree>> { support::child(self.syntax) }
}

cst_enum!(
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

impl<'tree> LitPat<'tree> {
    pub fn lit(self) -> Option<Lit<'tree>> { support::child(self.syntax) }
}
impl<'tree> UnderscorePat<'tree> {
    pub fn underscore_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![_])
    }
}
impl<'tree> IdentPat<'tree> {
    pub fn ident_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![ident])
    }
}
impl<'tree> ParenPat<'tree> {
    pub fn l_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['('])
    }
    pub fn pat(self) -> Option<Pat<'tree>> { support::child(self.syntax) }
    pub fn r_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![')'])
    }
}
impl<'tree> TupleLitPat<'tree> {
    pub fn l_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['('])
    }
    pub fn pats(self) -> impl Iterator<Item = Pat<'tree>> { support::children(self.syntax) }
    pub fn r_paren_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![')'])
    }
}
impl<'tree> RecordLitPat<'tree> {
    pub fn l_curly_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['{'])
    }
    pub fn fields(self) -> impl Iterator<Item = RecordPatField<'tree>> {
        support::children(self.syntax)
    }
    pub fn r_curly_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T!['}'])
    }
}
cst_node!(RecordPatField);
impl<'tree> RecordPatField<'tree> {
    pub fn ident_token(self) -> Option<SyntaxToken<'tree>> {
        support::token(self.syntax, T![ident])
    }
    pub fn eq_token(self) -> Option<SyntaxToken<'tree>> { support::token(self.syntax, T![=]) }
    pub fn pat(self) -> Option<Pat<'tree>> { support::child(self.syntax) }
}

impl<'tree> OrPat<'tree> {
    pub fn pats(self) -> impl Iterator<Item = Pat<'tree>> { support::children(self.syntax) }
}
