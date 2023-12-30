use pion_lexer::token::TokenKind;
use pion_lexer::T;

use super::{MarkClosed, Parser};
use crate::syntax::NodeKind;

// Module = Def*
pub fn module(p: &mut Parser) {
    let m = p.start();

    while !p.at_eof() {
        if p.at(T![def]) {
            def(p);
        } else {
            p.advance_with_error("expected a definition");
        }
    }

    p.close(m, NodeKind::Module);
}

// Def = "def" "name" TypeAnn? = Expr ";"
fn def(p: &mut Parser) {
    let m = p.start();
    p.assert_advance(T![def]);

    p.expect(T![ident]);
    type_ann_opt(p);
    p.expect(T![=]);
    expr(p);
    p.expect(T![;]);

    p.close(m, NodeKind::DefItem);
}

// TypeAnnOpt = TypeAnn?
fn type_ann_opt(p: &mut Parser) {
    if p.at(T![:]) {
        type_ann(p);
    }
}

// TypeAnn = ":" Expr
fn type_ann(p: &mut Parser) {
    let m = p.start();
    p.assert_advance(T![:]);
    expr(p);
    p.close(m, NodeKind::TypeAnn);
}

pub fn expr(p: &mut Parser) {
    match p.peek() {
        Some(T![let]) => return let_expr(p),
        Some(T![if]) => return if_expr(p),
        Some(T![fun]) => return fun_expr(p),
        _ => {}
    }

    let mut lhs = atom_expr(p);

    loop {
        match p.peek() {
            // FunCallExpr = Expr ArgList
            Some(T!['(']) => {
                let m = p.start_before(lhs);
                fun_arg_list(p);
                lhs = p.close(m, NodeKind::FunCallExpr);
            }
            // FieldProjExpr = Expr "." "Ident"
            // MethodCallExpr = Expr "." "Ident" ArgList
            Some(T![.]) => {
                let m = p.start_before(lhs);
                p.assert_advance(T![.]);
                p.expect(T![ident]);

                let kind = if p.at(T!['(']) {
                    fun_arg_list(p);
                    NodeKind::MethodCallExpr
                } else {
                    NodeKind::FieldProjExpr
                };
                lhs = p.close(m, kind);
            }
            _ => break,
        }
    }

    loop {
        match p.peek() {
            Some(T![->]) => {
                let m = p.start_before(lhs);
                {
                    let m = p.start();
                    p.assert_advance(T![->]);
                    expr(p);
                    p.close(m, NodeKind::RetType);
                }
                p.close(m, NodeKind::FunArrowExpr);
            }
            Some(T![:]) => {
                let m = p.start_before(lhs);
                type_ann(p);
                p.close(m, NodeKind::AnnExpr);
            }
            _ => break,
        }
    }
}

// FunArgList = "(" FunArg* ")"
fn fun_arg_list(p: &mut Parser) {
    let m = p.start();
    p.assert_advance(T!['(']);
    list(p, T![')'], T![,], fun_arg);
    p.close(m, NodeKind::FunArgList);
}

// FunArg = "@"? Expr
fn fun_arg(p: &mut Parser) {
    let m = p.start();

    p.advance_if_at(T![@]);
    expr(p);

    p.close(m, NodeKind::FunArg);
}

fn atom_expr(p: &mut Parser) -> MarkClosed {
    let m = p.start();
    match p.peek() {
        Some(T![true] | T![false]) => {
            {
                let m = p.start();
                p.advance();
                p.close(m, NodeKind::BoolLit);
            }
            p.close(m, NodeKind::LitExpr)
        }
        Some(T![dec_int] | T![bin_int] | T![hex_int]) => {
            {
                let m = p.start();
                p.advance();
                p.close(m, NodeKind::IntLit);
            }
            p.close(m, NodeKind::LitExpr)
        }
        Some(T![_]) => {
            p.advance();
            p.close(m, NodeKind::UnderscoreExpr)
        }
        Some(T![ident]) => {
            p.advance();
            p.close(m, NodeKind::IdentExpr)
        }
        Some(T![match]) => match_expr(p),
        Some(T!['[']) => array_expr(p),
        Some(T!['(']) => paren_or_tuple_expr(p),
        Some(T!['{']) => record_expr(p),
        _ => {
            p.advance_with_error("expected expression");
            p.close(m, NodeKind::Error)
        }
    }
}

// LetExpr = "let" Pat TypeAnn? "=" Expr ";" Expr
fn let_expr(p: &mut Parser) {
    let m = p.start();

    {
        p.assert_advance(T![let]);
        pat(p);
    }

    {
        type_ann_opt(p);
    }

    {
        let m = p.start();
        p.expect(T![=]);
        expr(p);
        p.close(m, NodeKind::LetInit);
    }

    {
        p.expect(T![;]);
        expr(p);
    }

    p.close(m, NodeKind::LetExpr);
}

// IfExpr = "if" Expr "then" Expr "else" Expr
fn if_expr(p: &mut Parser) {
    let m = p.start();

    {
        p.assert_advance(T![if]);
        expr(p);
    }

    {
        let m = p.start();
        p.expect(T![then]);
        expr(p);
        p.close(m, NodeKind::ThenExpr);
    }
    {
        let m = p.start();
        p.expect(T![else]);
        expr(p);
        p.close(m, NodeKind::ElseExpr);
    }

    p.close(m, NodeKind::IfExpr);
}

// FunLitExpr = "fun" FunParamList "=>" Expr
// FunTypeExpr = "fun" FunParamList "->" Expr
fn fun_expr(p: &mut Parser) {
    let m = p.start();

    {
        p.assert_advance(T![fun]);
        fun_param_list(p);
    }

    {
        let kind = match p.peek() {
            Some(T![=>]) => {
                p.advance();
                NodeKind::FunLitExpr
            }
            Some(T![->]) => {
                p.advance();
                NodeKind::FunTypeExpr
            }
            _ => {
                p.advance_with_error("expected arrow");
                NodeKind::FunLitExpr
            }
        };
        expr(p);
        p.close(m, kind);
    }
}

// FunParamList = "(" FunParam ")"
fn fun_param_list(p: &mut Parser) {
    let m = p.start();
    p.expect(T!['(']);
    list(p, T![')'], T![,], fun_param);
    p.close(m, NodeKind::FunParamList);
}

// FunParam = "@"? Pat TypeAnn?
fn fun_param(p: &mut Parser) {
    let m = p.start();
    p.advance_if_at(T![@]);
    pat(p);
    type_ann_opt(p);
    p.close(m, NodeKind::FunParam);
}

// MatchExpr = "match" Expr "{" MatchCase "}"
fn match_expr(p: &mut Parser) -> MarkClosed {
    let m = p.start();

    {
        p.assert_advance(T![match]);
        expr(p);
    }

    {
        p.expect(T!['{']);
        list(p, T!['}'], T![,], match_case);
    }

    p.close(m, NodeKind::MatchExpr)
}

// MatchCase = Pat ("if" Expr) "=>" Expr
fn match_case(p: &mut Parser) {
    let m = p.start();

    pat(p);

    if p.at(T![if]) {
        let m = p.start();
        p.assert_advance(T![if]);
        expr(p);
        p.close(m, NodeKind::MatchGuard);
    }

    {
        p.expect(T![=>]);
        expr(p);
    }

    p.close(m, NodeKind::MatchCase);
}

// ArrayExpr = "[" Expr* "]"
fn array_expr(p: &mut Parser) -> MarkClosed {
    let m = p.start();
    p.assert_advance(T!['[']);
    list(p, T![']'], T![,], expr);
    p.close(m, NodeKind::ArrayLitExpr)
}

// ParenExpr = "(" Expr ")"
// TupleExpr = "(" ")"
//          | "(" Expr ("," Expr)+ ","? ")"
fn paren_or_tuple_expr(p: &mut Parser) -> MarkClosed {
    let m = p.start();
    p.assert_advance(T!['(']);

    let mut seen_comma = false;
    let mut seen_expr = false;

    loop {
        match p.peek() {
            None | Some(T![')']) => break,
            Some(T![,]) => {
                p.assert_advance(T![,]);
                seen_comma = true;
                continue;
            }
            _ => {
                expr(p);
                seen_expr = true;
                if !p.at(T![')']) {
                    seen_comma |= p.expect(T![,]);
                }
            }
        }
    }
    p.expect(T![')']);

    p.close(
        m,
        if seen_expr && !seen_comma {
            NodeKind::ParenExpr
        } else {
            NodeKind::TupleLitExpr
        },
    )
}

// RecordExpr = "{" ExprField* "}"
fn record_expr(p: &mut Parser) -> MarkClosed {
    let m = p.start();
    p.assert_advance(T!['{']);
    list(p, T!['}'], T![,], record_expr_field);
    p.close(m, NodeKind::RecordExpr)
}

// RecordExprField = "Ident"
//           | "Ident" "=" Expr
//           | "Ident" ":" Expr
fn record_expr_field(p: &mut Parser) {
    let m = p.start();
    p.expect(T![ident]);

    match p.peek() {
        Some(T![=]) => {
            p.assert_advance(T![=]);
            expr(p);
        }
        Some(T![:]) => {
            p.assert_advance(T![:]);
            expr(p);
        }
        _ => {}
    };

    p.close(m, NodeKind::RecordExprField);
}

// OrPat = AtomPat ("|" AtomPat)+
pub fn pat(p: &mut Parser) {
    let lhs = atom_pat(p);

    if !p.at(T![|]) {
        return;
    }
    let m = p.start_before(lhs);
    while p.at(T![|]) {
        p.assert_advance(T![|]);
        atom_pat(p);
    }
    p.close(m, NodeKind::OrPat);
}

// AtomPat = LitPat
//         | IdentPat
//         | UnderscorePat
//         | ParenPat
//         | TuplePat
//         | RecordPat
fn atom_pat(p: &mut Parser) -> MarkClosed {
    let m = p.start();
    match p.peek() {
        Some(T![true] | T![false]) => {
            {
                let m = p.start();
                p.advance();
                p.close(m, NodeKind::BoolLit);
            }
            p.close(m, NodeKind::LitPat)
        }
        Some(T![dec_int] | T![bin_int] | T![hex_int]) => {
            {
                let m = p.start();
                p.advance();
                p.close(m, NodeKind::IntLit);
            }
            p.close(m, NodeKind::LitPat)
        }
        Some(T![_]) => {
            p.advance();
            p.close(m, NodeKind::UnderscorePat)
        }
        Some(T![ident]) => {
            p.advance();
            p.close(m, NodeKind::IdentPat)
        }
        Some(T!['(']) => paren_or_tuple_pat(p),
        Some(T!['{']) => record_pat(p),
        _ => {
            p.advance_with_error("expected pattern");
            p.close(m, NodeKind::Error)
        }
    }
}

// ParenPat = "(" Pat ")"
// TuplePat = "(" ")"
//          | "(" Pat ("," Pat)+ ","? ")"
fn paren_or_tuple_pat(p: &mut Parser) -> MarkClosed {
    let m = p.start();
    p.assert_advance(T!['(']);

    let mut seen_comma = false;
    let mut seen_pat = false;

    loop {
        match p.peek() {
            None | Some(T![')']) => break,
            Some(T![,]) => {
                p.assert_advance(T![,]);
                seen_comma = true;
                continue;
            }
            _ => {
                pat(p);
                seen_pat = true;
                if !p.at(T![')']) {
                    seen_comma |= p.expect(T![,]);
                }
            }
        }
    }
    p.expect(T![')']);

    p.close(
        m,
        if seen_pat && !seen_comma {
            NodeKind::ParenPat
        } else {
            NodeKind::TupleLitPat
        },
    )
}

// RecordPat = "{" "}"
//           | "{" PatField ("," PatField)* ","? "}"
fn record_pat(p: &mut Parser) -> MarkClosed {
    let m = p.start();
    p.assert_advance(T!['{']);
    list(p, T!['}'], T![,], pat_field);
    p.close(m, NodeKind::RecordLitPat)
}

// PatField = "ident" ("=" Pat)?
fn pat_field(p: &mut Parser) {
    let m = p.start();
    p.expect(T![ident]);
    if p.advance_if_at(T![=]) {
        pat(p);
    }
    p.close(m, NodeKind::RecordPatField);
}

fn list(p: &mut Parser, end: TokenKind, sep: TokenKind, elem: impl Fn(&mut Parser)) {
    loop {
        match p.peek() {
            None => break,
            Some(kind) if kind == end => break,
            Some(kind) if kind == sep => p.assert_advance(sep),
            _ => {
                elem(p);
                if !p.at(end) {
                    p.expect(sep);
                }
            }
        }
    }
    p.expect(end);
}
