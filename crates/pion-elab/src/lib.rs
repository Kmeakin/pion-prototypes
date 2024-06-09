#![feature(allocator_api)]

use command::CommandHandler;
use env::{ElabEnv, LocalInfo, MetaSource};
use pion_core::env::{AbsoluteVar, EnvLen};
use pion_core::semantics::{self, EvalOpts, Type, Value};
use pion_core::syntax::{Expr, FunArg, LetBinding, Plicity};
use pion_diagnostic::DiagnosticHandler;
use pion_printer::BumpDocAllocator;
use text_size::TextRange;

use self::unify::UnifyCtx;

mod diagnostics;
mod expr;
mod r#match;
mod pat;
mod stmt;
mod unify;

pub mod command;
pub mod env;

pub struct Elaborator<'handler, 'core, 'text> {
    bump: &'core bumpalo::Bump,
    text: &'text str,
    file_id: usize,
    diagnostic_handler: &'handler mut dyn DiagnosticHandler,
    command_handler: &'handler mut dyn CommandHandler,

    env: ElabEnv<'core>,
}

impl<'handler, 'core, 'text> Elaborator<'handler, 'core, 'text> {
    pub fn new(
        bump: &'core bumpalo::Bump,
        text: &'text str,
        file_id: usize,
        diagnostic_handler: &'handler mut dyn DiagnosticHandler,
        command_handler: &'handler mut dyn CommandHandler,
    ) -> Self {
        Self {
            bump,
            text,
            file_id,
            diagnostic_handler,
            command_handler,

            env: ElabEnv::default(),
        }
    }

    pub fn report_unsolved_metas(&mut self) {
        let meta_env = std::mem::take(&mut self.env.metas);
        for (id, (source, _, value)) in meta_env.iter().enumerate() {
            if value.is_none() {
                diagnostics::unsolved_meta_var(self, id, source, self.file_id);
            }
        }
        self.env.metas = meta_env;
    }

    fn push_unsolved_expr(&mut self, source: MetaSource, r#type: Type<'core>) -> Expr<'core> {
        let var = self.env.metas.len().to_absolute();
        self.env.metas.push(source, r#type);

        let mut expr = Expr::MetaVar(var);
        for (var, info) in AbsoluteVar::iter().zip(self.env.locals.infos.iter()) {
            match info {
                LocalInfo::Let => {}
                LocalInfo::Param => {
                    let var = self.env.locals.len().absolute_to_relative(var).unwrap();
                    let arg = Expr::LocalVar(var);
                    let (fun, arg) = self.bump.alloc((expr, arg));
                    let arg = FunArg::new(Plicity::Explicit, &*arg);
                    expr = Expr::FunApp { fun, arg };
                }
            }
        }
        expr
    }

    fn push_unsolved_type(&mut self, source: MetaSource) -> Value<'core> {
        let expr = self.push_unsolved_expr(source, Value::TYPE);
        self.eval_env().eval(&expr)
    }

    pub fn elim_env(&self) -> semantics::ElimEnv<'core, '_> {
        semantics::ElimEnv::new(self.bump, EvalOpts::default(), &self.env.metas.values)
    }

    pub fn eval_env(&mut self) -> semantics::EvalEnv<'core, '_> {
        semantics::EvalEnv::new(
            self.bump,
            EvalOpts::default(),
            &mut self.env.locals.values,
            &self.env.metas.values,
        )
    }

    pub fn quote_env(&self) -> semantics::QuoteEnv<'core, '_> {
        semantics::QuoteEnv::new(self.bump, self.env.locals.len(), &self.env.metas.values)
    }

    pub fn zonk_env(&mut self) -> semantics::ZonkEnv<'core, '_> {
        semantics::ZonkEnv::new(
            self.bump,
            &mut self.env.locals.values,
            &self.env.metas.values,
        )
    }

    pub fn unify_env(&mut self) -> UnifyCtx<'core, '_> {
        UnifyCtx::new(
            self.bump,
            &mut self.env.renaming,
            self.env.locals.len(),
            &mut self.env.metas.values,
        )
    }

    pub fn pretty(&mut self, expr: &Expr<'core>) -> String {
        let expr = self.zonk_env().zonk(expr);
        let alloc = BumpDocAllocator::new(self.bump);
        let unelaborator =
            pion_core::unelab::Unelaborator::new(alloc, pion_core::unelab::Config::default());
        let doc = unelaborator
            .expr(&mut self.env.locals.names, &expr)
            .into_doc();
        doc.pretty(usize::MAX).to_string()
    }

    fn push_let_bindings(&mut self, bindings: &[LetBinding<Expr<'core>, Expr<'core>>]) {
        for LetBinding { name, r#type, rhs } in bindings {
            let value = self.eval_env().eval(rhs);
            let r#type = self.eval_env().eval(r#type);
            self.env.locals.push_let(*name, *rhs, r#type, value);
        }
    }
}
