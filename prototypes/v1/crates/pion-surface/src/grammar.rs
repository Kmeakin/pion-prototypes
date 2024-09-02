use pion_lexer::token::TokenKind;
use pion_lexer::T;

use crate::parser::Parser;
use crate::syntax::NodeKind;

// Module = Def*
pub fn module(p: &mut Parser) {
    let start = p.start_node();

    while !p.at_eof() {
        if p.at(T![def]) {
            def(p);
        } else {
            p.advance_with_error("expected a definition");
        }
    }

    p.end_node(NodeKind::Module, start);
}

// Def = "def" "name" TypeAnn? = Expr ";"
fn def(p: &mut Parser) {
    let start = p.start_node();

    p.assert_advance(T![def]);
    p.expect(T![ident]);
    type_ann_opt(p);
    p.expect(T![=]);
    expr(p);
    p.expect(T![;]);

    p.end_node(NodeKind::DefItem, start);
}

// TypeAnnOpt = TypeAnn?
fn type_ann_opt(p: &mut Parser) {
    if p.at(T![:]) {
        type_ann(p);
    }
}

// TypeAnn = ":" Expr
fn type_ann(p: &mut Parser) {
    let start = p.start_node();

    p.assert_advance(T![:]);
    expr(p);

    p.end_node(NodeKind::TypeAnn, start);
}

pub fn expr(p: &mut Parser) {
    match p.peek() {
        Some(T![let]) => return let_expr(p),
        Some(T![if]) => return if_expr(p),
        Some(T![fun]) => return fun_expr(p),
        _ => {}
    }

    let start = p.start_node();
    atom_expr(p);

    loop {
        match p.peek() {
            // FunCallExpr = Expr ArgList
            Some(T!['(']) => {
                fun_arg_list(p);
                p.end_node(NodeKind::FunCallExpr, start);
            }
            // FieldProjExpr = Expr "." "Ident"
            // MethodCallExpr = Expr "." "Ident" ArgList
            Some(T![.]) => {
                p.assert_advance(T![.]);
                p.expect(T![ident]);

                let kind = if p.at(T!['(']) {
                    fun_arg_list(p);
                    NodeKind::MethodCallExpr
                } else {
                    NodeKind::FieldProjExpr
                };
                p.end_node(kind, start);
            }
            _ => break,
        }
    }

    loop {
        match p.peek() {
            Some(T![->]) => {
                {
                    let start = p.start_node();
                    p.assert_advance(T![->]);
                    expr(p);
                    p.end_node(NodeKind::RetType, start);
                }
                p.end_node(NodeKind::FunArrowExpr, start);
            }
            Some(T![:]) => {
                type_ann(p);
                p.end_node(NodeKind::AnnExpr, start);
            }
            _ => break,
        }
    }
}

// FunArgList = "(" FunArg* ")"
fn fun_arg_list(p: &mut Parser) {
    let start = p.start_node();

    p.assert_advance(T!['(']);
    list(p, T![')'], T![,], fun_arg);

    p.end_node(NodeKind::FunArgList, start);
}

// FunArg = "@"? Expr
fn fun_arg(p: &mut Parser) {
    let start = p.start_node();

    p.advance_if_at(T![@]);
    expr(p);

    p.end_node(NodeKind::FunArg, start);
}

fn atom_expr(p: &mut Parser) {
    match p.peek() {
        Some(T![true] | T![false]) => {
            let start = p.start_node();
            {
                let start = p.start_node();
                p.advance();
                p.end_node(NodeKind::BoolLit, start);
            }
            p.end_node(NodeKind::LitExpr, start);
        }
        Some(T![dec_int] | T![bin_int] | T![hex_int]) => {
            let start = p.start_node();
            {
                let start = p.start_node();
                p.advance();
                p.end_node(NodeKind::IntLit, start);
            }
            p.end_node(NodeKind::LitExpr, start);
        }
        Some(T![_]) => {
            let start = p.start_node();
            p.advance();
            p.end_node(NodeKind::UnderscoreExpr, start);
        }
        Some(T![ident]) => {
            let start = p.start_node();
            p.advance();
            p.end_node(NodeKind::IdentExpr, start);
        }
        Some(T![match]) => match_expr(p),
        Some(T!['[']) => array_expr(p),
        Some(T!['(']) => paren_or_tuple_expr(p),
        Some(T!['{']) => record_expr(p),
        _ => {
            let start = p.start_node();
            p.advance_with_error("expected expression");
            p.end_node(NodeKind::Error, start);
        }
    }
}

// LetExpr = "let" Pat TypeAnn? "=" Expr ";" Expr
fn let_expr(p: &mut Parser) {
    let start = p.start_node();

    p.assert_advance(T![let]);
    pat(p);
    type_ann_opt(p);

    {
        let start = p.start_node();
        p.expect(T![=]);
        expr(p);
        p.end_node(NodeKind::LetInit, start);
    }

    p.expect(T![;]);
    expr(p);

    p.end_node(NodeKind::LetExpr, start);
}

// IfExpr = "if" Expr "then" Expr "else" Expr
fn if_expr(p: &mut Parser) {
    let start = p.start_node();

    p.assert_advance(T![if]);
    expr(p);

    {
        let start = p.start_node();
        p.expect(T![then]);
        expr(p);
        p.end_node(NodeKind::ThenExpr, start);
    }

    {
        let start = p.start_node();
        p.expect(T![else]);
        expr(p);
        p.end_node(NodeKind::ElseExpr, start);
    }

    p.end_node(NodeKind::IfExpr, start);
}

// FunLitExpr = "fun" FunParamList "=>" Expr
// FunTypeExpr = "fun" FunParamList "->" Expr
fn fun_expr(p: &mut Parser) {
    let start = p.start_node();

    p.assert_advance(T![fun]);
    fun_param_list(p);

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

    p.end_node(kind, start);
}

// FunParamList = "(" FunParam ")"
fn fun_param_list(p: &mut Parser) {
    let start = p.start_node();

    p.expect(T!['(']);
    list(p, T![')'], T![,], fun_param);

    p.end_node(NodeKind::FunParamList, start);
}

// FunParam = "@"? Pat TypeAnn?
fn fun_param(p: &mut Parser) {
    let start = p.start_node();

    p.advance_if_at(T![@]);
    pat(p);
    type_ann_opt(p);

    p.end_node(NodeKind::FunParam, start);
}

// MatchExpr = "match" Expr "{" MatchCase "}"
fn match_expr(p: &mut Parser) {
    let start = p.start_node();

    p.assert_advance(T![match]);
    expr(p);

    p.expect(T!['{']);
    list(p, T!['}'], T![,], match_case);

    p.end_node(NodeKind::MatchExpr, start);
}

// MatchCase = Pat ("if" Expr) "=>" Expr
fn match_case(p: &mut Parser) {
    let start = p.start_node();

    pat(p);

    if p.at(T![if]) {
        let start = p.start_node();
        p.assert_advance(T![if]);
        expr(p);
        p.end_node(NodeKind::MatchGuard, start);
    }

    p.expect(T![=>]);
    expr(p);

    p.end_node(NodeKind::MatchCase, start);
}

// ArrayExpr = "[" Expr* "]"
fn array_expr(p: &mut Parser) {
    let start = p.start_node();

    p.assert_advance(T!['[']);
    list(p, T![']'], T![,], expr);

    p.end_node(NodeKind::ArrayLitExpr, start);
}

// ParenExpr = "(" Expr ")"
// TupleExpr = "(" ")"
//          | "(" Expr ("," Expr)+ ","? ")"
fn paren_or_tuple_expr(p: &mut Parser) {
    let start = p.start_node();
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
    let kind = if seen_expr && !seen_comma {
        NodeKind::ParenExpr
    } else {
        NodeKind::TupleLitExpr
    };

    p.expect(T![')']);

    p.end_node(kind, start);
}

// RecordExpr = "{" ExprField* "}"
fn record_expr(p: &mut Parser) {
    let start = p.start_node();

    p.assert_advance(T!['{']);
    list(p, T!['}'], T![,], record_expr_field);

    p.end_node(NodeKind::RecordExpr, start);
}

// RecordExprField = "Ident"
//           | "Ident" "=" Expr
//           | "Ident" ":" Expr
fn record_expr_field(p: &mut Parser) {
    let start = p.start_node();
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

    p.end_node(NodeKind::RecordExprField, start);
}

// OrPat = AtomPat ("|" AtomPat)+
pub fn pat(p: &mut Parser) {
    let start = p.start_node();
    atom_pat(p);

    if !p.at(T![|]) {
        return;
    }
    while p.at(T![|]) {
        p.assert_advance(T![|]);
        atom_pat(p);
    }

    p.end_node(NodeKind::OrPat, start);
}

// AtomPat = LitPat
//         | IdentPat
//         | UnderscorePat
//         | ParenPat
//         | TuplePat
//         | RecordPat
fn atom_pat(p: &mut Parser) {
    match p.peek() {
        Some(T![true] | T![false]) => {
            let start = p.start_node();
            {
                let start = p.start_node();
                p.advance();
                p.end_node(NodeKind::BoolLit, start);
            }
            p.end_node(NodeKind::LitPat, start);
        }
        Some(T![dec_int] | T![bin_int] | T![hex_int]) => {
            let start = p.start_node();
            {
                let start = p.start_node();
                p.advance();
                p.end_node(NodeKind::IntLit, start);
            }
            p.end_node(NodeKind::LitPat, start);
        }
        Some(T![_]) => {
            let start = p.start_node();
            p.advance();
            p.end_node(NodeKind::UnderscorePat, start);
        }
        Some(T![ident]) => {
            let start = p.start_node();
            p.advance();
            p.end_node(NodeKind::IdentPat, start);
        }
        Some(T!['(']) => paren_or_tuple_pat(p),
        Some(T!['{']) => record_pat(p),
        _ => {
            let start = p.start_node();
            p.advance_with_error("expected pattern");
            p.end_node(NodeKind::Error, start);
        }
    };
}

fn paren_or_tuple_pat(p: &mut Parser) {
    let start = p.start_node();
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
    let kind = if seen_pat && !seen_comma {
        NodeKind::ParenPat
    } else {
        NodeKind::TupleLitPat
    };

    p.expect(T![')']);

    p.end_node(kind, start);
}

// RecordPat = "{" "}"
//           | "{" PatField ("," PatField)* ","? "}"
fn record_pat(p: &mut Parser) {
    let start = p.start_node();

    p.assert_advance(T!['{']);
    list(p, T!['}'], T![,], pat_field);

    p.end_node(NodeKind::RecordLitPat, start);
}

// PatField = "ident" ("=" Pat)?
fn pat_field(p: &mut Parser) {
    let start = p.start_node();

    p.expect(T![ident]);
    if p.advance_if_at(T![=]) {
        pat(p);
    }

    p.end_node(NodeKind::RecordPatField, start);
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
