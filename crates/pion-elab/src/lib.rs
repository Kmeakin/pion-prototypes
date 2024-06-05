#![feature(allocator_api)]

use command::CommandHandler;
use pion_core::env::{AbsoluteVar, EnvLen, RelativeVar, SharedEnv, UniqueEnv};
use pion_core::semantics::{self, EvalOpts, Type, Value};
use pion_core::{Expr, FunArg, LetBinding, Plicity};
use pion_diagnostic::{Diagnostic, DiagnosticHandler, Label};
use pion_symbol::Symbol;
use text_size::TextRange;

use self::unify::{PartialRenaming, UnifyCtx};

mod expr;
mod r#match;
mod pat;
mod stmt;
mod unify;

pub mod command;

pub struct Elaborator<'handler, 'core, 'text> {
    bump: &'core bumpalo::Bump,
    text: &'text str,
    file_id: usize,
    diagnostic_handler: &'handler mut dyn DiagnosticHandler,
    command_handler: &'handler mut dyn CommandHandler,

    env: ElabEnv<'core>,
}

#[derive(Default)]
pub struct ElabEnv<'core> {
    locals: LocalEnv<'core>,
    metas: MetaEnv<'core>,
    renaming: PartialRenaming,
}

#[derive(Default)]
struct LocalEnv<'core> {
    names: UniqueEnv<Option<Symbol>>,
    infos: UniqueEnv<LocalInfo>,
    types: UniqueEnv<Type<'core>>,
    values: SharedEnv<Value<'core>>,
}

#[derive(Debug, Copy, Clone)]
pub enum LocalInfo {
    Let,
    Param,
}

impl<'core> LocalEnv<'core> {
    pub fn len(&self) -> EnvLen { self.names.len() }

    pub fn push(
        &mut self,
        name: Option<Symbol>,
        info: LocalInfo,
        r#type: Type<'core>,
        value: Value<'core>,
    ) {
        self.names.push(name);
        self.infos.push(info);
        self.types.push(r#type);
        self.values.push(value);
    }

    pub fn push_let(&mut self, name: Option<Symbol>, r#type: Type<'core>, value: Value<'core>) {
        self.push(name, LocalInfo::Let, r#type, value);
    }

    pub fn push_param(&mut self, name: Option<Symbol>, r#type: Type<'core>) {
        let var = Value::local_var(self.values.len().to_absolute());
        self.push(name, LocalInfo::Param, r#type, var);
    }

    pub fn pop(&mut self) {
        self.names.pop();
        self.infos.pop();
        self.types.pop();
        self.values.pop();
    }

    pub fn truncate(&mut self, len: EnvLen) {
        self.names.truncate(len);
        self.infos.truncate(len);
        self.types.truncate(len);
        self.values.truncate(len);
    }

    pub fn lookup(&self, sym: Symbol) -> Option<RelativeVar> {
        self.names
            .iter()
            .rev()
            .enumerate()
            .find(|(_, name)| **name == Some(sym))
            .map(|(idx, _)| RelativeVar::from(idx))
    }

    fn next_var(&self) -> Value<'core> { Value::local_var(self.len().to_absolute()) }
}

#[derive(Default)]
struct MetaEnv<'core> {
    sources: UniqueEnv<MetaSource>,
    types: UniqueEnv<Type<'core>>,
    values: UniqueEnv<Option<Value<'core>>>,
}

impl<'core> MetaEnv<'core> {
    fn len(&self) -> EnvLen { self.sources.len() }

    fn push(&mut self, source: MetaSource, r#type: Type<'core>) {
        self.sources.push(source);
        self.types.push(r#type);
        self.values.push(None);
    }

    fn iter(&self) -> impl Iterator<Item = (MetaSource, &Type<'core>, &Option<Value<'core>>)> + '_ {
        self.sources
            .iter()
            .zip(self.types.iter())
            .zip(self.values.iter())
            .map(|((source, r#type), value)| (*source, r#type, value))
    }
}

#[derive(Debug, Copy, Clone)]
pub enum MetaSource {
    PatType {
        range: TextRange,
        name: Option<Symbol>,
    },
    HoleType {
        range: TextRange,
    },
    HoleExpr {
        range: TextRange,
    },
    ImplicitArg {
        range: TextRange,
        name: Option<Symbol>,
    },
    ListElemType {
        range: TextRange,
    },
    MatchResultType {
        range: TextRange,
    },
}

impl MetaSource {
    pub const fn range(&self) -> TextRange {
        match self {
            Self::PatType { range, .. }
            | Self::HoleType { range, .. }
            | Self::HoleExpr { range, .. }
            | Self::ImplicitArg { range, .. }
            | Self::ListElemType { range, .. }
            | Self::MatchResultType { range, .. } => *range,
        }
    }
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

    fn report_diagnostic(&mut self, diagnostic: Diagnostic<usize>) {
        self.diagnostic_handler.handle_diagnostic(diagnostic);
    }

    pub fn report_unsolved_metas(&mut self) {
        let meta_env = std::mem::take(&mut self.env.metas);
        for (id, (source, _, value)) in meta_env.iter().enumerate() {
            if value.is_none() {
                let message = match source {
                    MetaSource::PatType {
                        name: Some(name), ..
                    } => format!("type of variable `{name}`"),
                    MetaSource::PatType { name: None, .. } => {
                        "type of placeholder pattern".to_string()
                    }
                    MetaSource::HoleType { .. } => "type of hole".to_string(),
                    MetaSource::HoleExpr { .. } => "expression to solve hole".to_string(),
                    MetaSource::ImplicitArg {
                        name: Some(name), ..
                    } => format!("implicit argument `{name}`"),
                    MetaSource::ImplicitArg { name: None, .. } => "implicit argument".to_string(),
                    MetaSource::ListElemType { .. } => "element type of empty list".to_string(),
                    MetaSource::MatchResultType { .. } => {
                        "result type of match expression".to_string()
                    }
                };

                self.report_diagnostic(
                    Diagnostic::error()
                        .with_message(format!("Unsolved metavariable: ?{id}"))
                        .with_labels(vec![Label::primary(self.file_id, source.range())
                            .with_message(format!("could not infer {message}"))]),
                );
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
        let printer = pion_printer::Printer::new(self.bump, pion_printer::Config::default());
        let unelaborator =
            pion_core::unelab::Unelaborator::new(printer, pion_core::unelab::Config::default());
        let doc = unelaborator
            .expr(&mut self.env.locals.names, &expr)
            .into_doc();
        doc.pretty(usize::MAX).to_string()
    }

    fn push_let_bindings(&mut self, bindings: &[LetBinding<Expr<'core>, Expr<'core>>]) {
        for LetBinding { name, r#type, expr } in bindings {
            let value = self.eval_env().eval(expr);
            let r#type = self.eval_env().eval(r#type);
            self.env.locals.push_let(*name, r#type, value);
        }
    }
}
