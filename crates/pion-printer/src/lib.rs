use pion_symbol::Symbol;
pub use pretty::{docs, Doc, DocAllocator, DocPtr, Pretty, RefDoc};

mod bump;

pub use self::bump::BumpDocAllocator;

pub type DocBuilder<'bump> = pretty::DocBuilder<'bump, BumpDocAllocator<'bump>>;

pub const INDENT: isize = 4;

/// Exprs
impl<'bump> BumpDocAllocator<'bump> {
    pub fn ann_expr(
        &'bump self,
        expr: impl Pretty<'bump, Self>,
        r#type: impl Pretty<'bump, Self>,
    ) -> DocBuilder<'bump> {
        docs![self, expr, " : ", r#type]
    }

    pub fn do_expr(&'bump self, block: impl Pretty<'bump, Self>) -> DocBuilder<'bump> {
        let block = block.pretty(self).nest(INDENT);
        docs![self, "do", " {", block, "}"].group()
    }

    pub fn block<I>(
        &'bump self,
        stmts: I,
        expr: Option<impl Pretty<'bump, Self>>,
    ) -> DocBuilder<'bump>
    where
        I: IntoIterator,
        I::IntoIter: ExactSizeIterator,
        I::Item: Pretty<'bump, Self>,
    {
        let stmts = stmts.into_iter();
        match (stmts.len(), expr) {
            (0, Some(expr)) => expr.pretty(self),
            (_, Some(expr)) => {
                let stmts = self.intersperse(stmts, self.hardline());
                docs![
                    self,
                    self.hardline(),
                    stmts,
                    self.hardline(),
                    expr,
                    self.hardline()
                ]
            }

            (0, None) => self.nil(),
            (_, None) => self.intersperse(stmts, self.hardline()),
        }
    }

    pub fn let_stmt(
        &'bump self,
        rec: bool,
        pat: impl Pretty<'bump, Self>,
        r#type: Option<impl Pretty<'bump, Self>>,
        init: impl Pretty<'bump, Self>,
    ) -> DocBuilder<'bump> {
        let rec = if rec {
            docs![self, self.text("rec"), self.space()]
        } else {
            self.nil()
        };

        let r#type = match r#type {
            None => self.nil(),
            Some(r#type) => docs![self, " : ", r#type],
        };

        docs![
            self,
            "let",
            self.space(),
            rec,
            pat,
            r#type,
            docs![self, self.line(), "= ", init, ";"]
                .group()
                .nest(INDENT),
        ]
    }

    pub fn let_expr(
        &'bump self,
        pat: impl Pretty<'bump, Self>,
        r#type: Option<impl Pretty<'bump, Self>>,
        init: impl Pretty<'bump, Self>,
        body: impl Pretty<'bump, Self>,
    ) -> DocBuilder<'bump> {
        let r#type = match r#type {
            None => self.nil(),
            Some(r#type) => docs![self, " : ", r#type],
        };

        docs![
            self,
            "let ",
            pat,
            r#type,
            docs![self, self.line(), "= ", init, ";"]
                .group()
                .nest(INDENT),
            docs![self, self.hardline(), body]
        ]
    }

    pub fn if_expr(
        &'bump self,
        cond: impl Pretty<'bump, Self>,
        then: impl Pretty<'bump, Self>,
        r#else: impl Pretty<'bump, Self>,
    ) -> DocBuilder<'bump> {
        docs![self, "if ", cond, " then ", then, " else ", r#else]
    }

    pub fn match_expr(
        &'bump self,
        scrut: impl Pretty<'bump, Self>,
        cases: impl IntoIterator<Item = impl Pretty<'bump, Self>>,
    ) -> DocBuilder<'bump> {
        let cases = cases
            .into_iter()
            .map(|case| self.hardline().append(case).append(","));
        let cases = self.concat(cases).nest(INDENT);
        docs![self, "match ", scrut, " {", cases, self.line_(), "}"].group()
    }

    pub fn match_case(
        &'bump self,
        pat: impl Pretty<'bump, Self>,
        guard: impl Pretty<'bump, Self>,
        expr: impl Pretty<'bump, Self>,
    ) -> DocBuilder<'bump> {
        docs![self, pat, guard, " => ", expr]
    }

    pub fn arrow_expr(
        &'bump self,
        plicity: impl Pretty<'bump, Self>,
        lhs: impl Pretty<'bump, Self>,
        rhs: impl Pretty<'bump, Self>,
    ) -> DocBuilder<'bump> {
        docs![self, plicity, lhs, " -> ", rhs]
    }

    pub fn fun_type_expr(
        &'bump self,
        params: impl IntoIterator<Item = impl Pretty<'bump, Self>>,
        body: impl Pretty<'bump, Self>,
    ) -> DocBuilder<'bump> {
        let params = self.intersperse(params, self.space());
        docs![
            self,
            "forall ",
            params,
            " ->",
            self.line().append(body).group().nest(INDENT)
        ]
    }

    pub fn fun_lit_expr(
        &'bump self,
        params: impl IntoIterator<Item = impl Pretty<'bump, Self>>,
        body: impl Pretty<'bump, Self>,
    ) -> DocBuilder<'bump> {
        let params = self.intersperse(params, self.space());
        docs![
            self,
            "fun ",
            params,
            " =>",
            self.line().append(body).group().nest(INDENT)
        ]
    }

    pub fn fun_app_expr(
        &'bump self,
        fun: impl Pretty<'bump, Self>,
        args: impl IntoIterator<Item = impl Pretty<'bump, Self>>,
    ) -> DocBuilder<'bump> {
        let args = self.intersperse(args, self.space());
        docs![self, fun, self.space(), args]
    }

    pub fn list_lit_expr(
        &'bump self,
        exprs: impl IntoIterator<Item = impl Pretty<'bump, Self>>,
    ) -> DocBuilder<'bump> {
        let exprs = self.intersperse(exprs, ", ");
        docs![self, "[", exprs, "]"]
    }

    pub fn record_type_field(
        &'bump self,
        name: impl Pretty<'bump, Self>,
        r#type: impl Pretty<'bump, Self>,
    ) -> DocBuilder<'bump> {
        docs![self, name, " : ", r#type]
    }

    pub fn record_lit_field(
        &'bump self,
        name: impl Pretty<'bump, Self>,
        expr: impl Pretty<'bump, Self>,
    ) -> DocBuilder<'bump> {
        docs![self, name, " = ", expr]
    }

    pub fn record_proj_expr(
        &'bump self,
        scrut: impl Pretty<'bump, Self>,
        field: impl Pretty<'bump, Self>,
    ) -> DocBuilder<'bump> {
        docs![self, scrut, ".", field]
    }
}

/// Pats
impl<'bump> BumpDocAllocator<'bump> {
    pub fn or_pat(
        &'bump self,
        pats: impl IntoIterator<Item = impl Pretty<'bump, Self>>,
    ) -> DocBuilder<'bump> {
        self.intersperse(pats, self.text(" | ")).group()
    }
}

/// Function arguments and parameters
impl<'bump> BumpDocAllocator<'bump> {
    pub fn fun_arg(
        &'bump self,
        plicity: impl Pretty<'bump, Self>,
        expr: impl Pretty<'bump, Self>,
    ) -> DocBuilder<'bump> {
        docs![self, plicity, expr]
    }

    pub fn fun_param(
        &'bump self,
        plicity: impl Pretty<'bump, Self>,
        pat: impl Pretty<'bump, Self>,
        r#type: Option<impl Pretty<'bump, Self>>,
    ) -> DocBuilder<'bump> {
        match r#type {
            None => docs![self, plicity, pat],
            Some(r#type) => docs![self, "(", plicity, pat, " : ", r#type, ")"],
        }
    }

    pub fn bool(&'bump self, value: bool) -> DocBuilder<'bump> {
        match value {
            true => self.text("true"),
            false => self.text("false"),
        }
    }
}

/// Definitions shared between expressions and patterns
impl<'bump> BumpDocAllocator<'bump> {
    pub fn paren(&'bump self, inner: impl Pretty<'bump, Self>) -> DocBuilder<'bump> {
        docs![self, "(", inner, ")"]
    }

    pub fn tuple<I>(&'bump self, elems: I) -> DocBuilder<'bump>
    where
        I: IntoIterator,
        I::Item: Pretty<'bump, Self>,
        I::IntoIter: ExactSizeIterator,
    {
        let exprs = elems.into_iter();
        let trailing_comma = if exprs.len() == 1 {
            self.text(",")
        } else {
            self.text(",").flat_alt(self.nil())
        };

        let exprs = self.intersperse(exprs, self.text(",").append(self.line()));
        let exprs = docs![self, self.line_(), exprs, trailing_comma, self.line_()].nest(INDENT);
        docs![self, "(", exprs, ")"].group()
    }

    pub fn record(
        &'bump self,
        fields: impl IntoIterator<Item = impl Pretty<'bump, Self>>,
    ) -> DocBuilder<'bump> {
        let trailing_comma = self.text(",").flat_alt(self.nil());
        let exprs = self.intersperse(fields, self.text(",").append(self.line()));
        let exprs = docs![self, self.line(), exprs, trailing_comma, self.line()].nest(INDENT);
        docs![self, "{", exprs, "}"].group()
    }
}

/// Misc
impl<'bump> BumpDocAllocator<'bump> {
    pub fn symbol(&'bump self, symbol: Symbol) -> DocBuilder<'bump> { self.text(symbol.as_str()) }
}
