use std::fmt;

use pion_utils::interner::Symbol;
use pion_utils::location::ByteSpan;

use self::unify::UnifyCtx;
use crate::env::{EnvLen, Index, SharedEnv, UniqueEnv};
use crate::semantics::{ElimEnv, EvalEnv, QuoteEnv};
use crate::syntax::{BinderInfo, Expr, Type, Value};

mod diagnostics;
mod expr;
mod pat;
mod unify;

#[cfg(test)]
mod tests;

pub struct ElabCtx<'surface, 'hir, 'core> {
    bump: &'core bumpalo::Bump,
    syntax_map: &'hir pion_hir::syntax::LocalSyntaxMap<'surface, 'hir>,
    local_env: LocalEnv<'core>,
    meta_env: MetaEnv<'core>,
    renaming: unify::PartialRenaming,
    diagnostics: Vec<diagnostics::ElabDiagnostic>,
}

impl<'surface, 'hir, 'core> ElabCtx<'surface, 'hir, 'core> {
    pub fn new(
        bump: &'core bumpalo::Bump,
        syntax_map: &'hir pion_hir::syntax::LocalSyntaxMap<'surface, 'hir>,
    ) -> Self {
        Self {
            bump,
            syntax_map,
            local_env: LocalEnv::default(),
            meta_env: MetaEnv::default(),
            renaming: unify::PartialRenaming::new(),
            diagnostics: Vec::default(),
        }
    }

    pub fn finish(self) -> Vec<diagnostics::ElabDiagnostic> { self.diagnostics }

    fn push_unsolved_expr(&mut self, source: MetaSource, r#type: Type<'core>) -> Expr<'core> {
        let level = self.meta_env.len().to_level();
        self.meta_env.push(source, r#type);
        Expr::InsertedMeta(
            level,
            self.bump.alloc_slice_copy(self.local_env.infos.as_slice()),
        )
    }

    fn push_unsolved_type(&mut self, source: MetaSource) -> Value<'core> {
        let expr = self.push_unsolved_expr(source, Value::TYPE);
        self.eval_env().eval(&expr)
    }

    fn emit_diagnostic(&mut self, diagnostic: diagnostics::ElabDiagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn elim_env(&self) -> ElimEnv<'core, '_> { ElimEnv::new(self.bump, &self.meta_env.values) }

    pub fn eval_env(&mut self) -> EvalEnv<'core, '_> {
        let elim_env = ElimEnv::new(self.bump, &self.meta_env.values);
        elim_env.eval_env(&mut self.local_env.values)
    }

    pub fn quote_env(&self) -> QuoteEnv<'core, 'core, '_> {
        QuoteEnv::new(self.bump, self.elim_env(), self.local_env.values.len())
    }

    pub fn unifiy_ctx(&mut self) -> UnifyCtx<'core, '_> {
        UnifyCtx::new(
            self.bump,
            &mut self.renaming,
            self.local_env.len(),
            &mut self.meta_env.values,
        )
    }

    /// Run `f`, potentially modifying the local environment, then restore the
    /// local environment to its previous state.
    fn with_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let initial_len = self.local_env.len();
        let result = f(self);
        self.local_env.truncate(initial_len);
        result
    }

    fn with_param<T>(
        &mut self,
        name: Option<Symbol>,
        r#type: Type<'core>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.local_env.push_param(name, r#type);
        let result = f(self);
        self.local_env.pop();
        result
    }
}

#[derive(Default)]
struct LocalEnv<'core> {
    names: UniqueEnv<Option<Symbol>>,
    infos: UniqueEnv<BinderInfo>,
    types: UniqueEnv<Type<'core>>,
    values: SharedEnv<Value<'core>>,
}

#[derive(Debug)]
struct LocalEntry<'env, 'core> {
    name: Option<Symbol>,
    info: BinderInfo,
    r#type: &'env Type<'core>,
    value: &'env Value<'core>,
}

impl<'core> LocalEnv<'core> {
    fn new() -> Self { Self::default() }

    fn len(&self) -> EnvLen { self.names.len() }

    fn reserve(&mut self, amount: usize) {
        self.names.reserve(amount);
        self.infos.reserve(amount);
        self.types.reserve(amount);
        self.values.reserve(amount);
    }

    fn push(
        &mut self,
        name: Option<Symbol>,
        info: BinderInfo,
        r#type: Type<'core>,
        value: Value<'core>,
    ) {
        self.names.push(name);
        self.infos.push(info);
        self.types.push(r#type);
        self.values.push(value);
    }

    fn push_def(&mut self, name: Option<Symbol>, r#type: Type<'core>, value: Value<'core>) {
        self.push(name, BinderInfo::Def, r#type, value);
    }

    fn push_param(&mut self, name: Option<Symbol>, r#type: Type<'core>) {
        let value = self.next_var();
        self.push(name, BinderInfo::Param, r#type, value);
    }

    fn next_var(&self) -> Value<'core> { Value::local(self.values.len().to_level()) }

    fn pop(&mut self) {
        self.names.pop();
        self.infos.pop();
        self.types.pop();
        self.values.pop();
    }

    fn truncate(&mut self, len: EnvLen) {
        self.names.truncate(len);
        self.infos.truncate(len);
        self.types.truncate(len);
        self.values.truncate(len);
    }

    fn lookup(&self, name: Symbol) -> Option<(Index, LocalEntry<'_, 'core>)> {
        let index = self.names.index_of_elem(&Some(name))?;

        let name = self.names.get_index(index).copied()?;
        let info = self.infos.get_index(index).copied()?;
        let r#type = self.types.get_index(index)?;
        let value = self.values.get_index(index)?;
        Some((
            index,
            LocalEntry {
                name,
                info,
                r#type,
                value,
            },
        ))
    }

    fn iter(&self) -> impl Iterator<Item = LocalEntry<'_, 'core>> {
        let names = self.names.iter();
        let infos = self.infos.iter();
        let types = self.types.iter();
        let values = self.values.iter();
        names
            .zip(infos)
            .zip(types)
            .zip(values)
            .map(|(((name, info), r#type), value)| LocalEntry {
                name: *name,
                info: *info,
                r#type,
                value,
            })
    }
}

impl<'core> fmt::Debug for LocalEnv<'core> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter().enumerate()).finish()
    }
}

#[derive(Default)]
struct MetaEnv<'core> {
    sources: UniqueEnv<MetaSource>,
    types: UniqueEnv<Type<'core>>,
    values: UniqueEnv<Option<Value<'core>>>,
}

#[derive(Debug)]
struct MetaEntry<'env, 'core> {
    source: MetaSource,
    r#type: &'env Type<'core>,
    value: &'env Option<Value<'core>>,
}

impl<'core> MetaEnv<'core> {
    fn new() -> Self { Self::default() }

    fn len(&self) -> EnvLen { self.sources.len() }

    fn reserve(&mut self, amount: usize) {
        self.sources.reserve(amount);
        self.types.reserve(amount);
        self.values.reserve(amount);
    }

    fn push(&mut self, source: MetaSource, r#type: Type<'core>) {
        self.sources.push(source);
        self.types.push(r#type);
        self.values.push(None);
    }

    fn pop(&mut self) {
        self.sources.pop();
        self.types.pop();
        self.values.pop();
    }

    fn truncate(&mut self, len: EnvLen) {
        self.sources.truncate(len);
        self.types.truncate(len);
        self.values.truncate(len);
    }

    fn iter(&self) -> impl Iterator<Item = MetaEntry<'_, 'core>> {
        let sources = self.sources.iter();
        let types = self.types.iter();
        let values = self.values.iter();
        sources
            .zip(types)
            .zip(values)
            .map(|((source, r#type), value)| MetaEntry {
                source: *source,
                r#type,
                value,
            })
    }
}

impl<'core> fmt::Debug for MetaEnv<'core> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter().enumerate()).finish()
    }
}

#[derive(Debug, Copy, Clone)]
pub enum MetaSource {
    UnderscoreType { span: ByteSpan },
    UnderscoreExpr { span: ByteSpan },
    EmptyArrayElemType { span: ByteSpan },
    PatType { span: ByteSpan },
}

#[derive(Debug, Clone)]
pub struct Synth<'core, T>(pub T, pub Type<'core>);

impl<'core, T> Synth<'core, T> {
    pub const fn new(core: T, r#type: Type<'core>) -> Self { Self(core, r#type) }
}

#[derive(Debug, Clone)]
pub struct Check<T>(pub T);

impl<T> Check<T> {
    pub const fn new(core: T) -> Self { Self(core) }
}
