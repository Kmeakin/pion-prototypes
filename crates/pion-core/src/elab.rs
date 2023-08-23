use std::fmt;
use std::hash::BuildHasherDefault;

use nohash::IntMap;
use pion_utils::identity::Identity;
use pion_utils::interner::Symbol;
use pion_utils::location::ByteSpan;
use pion_utils::slice_vec::SliceVec;

use self::unify::UnifyCtx;
use crate::env::{EnvLen, Index, SharedEnv, UniqueEnv};
use crate::pretty;
use crate::semantics::{ElimEnv, EvalEnv, QuoteEnv, ZonkEnv};
use crate::syntax::*;

mod diagnostics;
mod expr;
mod pat;
mod unify;

pub struct ElabCtx<'surface, 'hir, 'core> {
    bump: &'core bumpalo::Bump,
    syntax_map: &'hir pion_hir::syntax::LocalSyntaxMap<'surface, 'hir>,
    local_env: LocalEnv<'core>,
    meta_env: MetaEnv<'core>,
    renaming: unify::PartialRenaming,
    type_map: TypeMap<'hir, 'core>,
    diagnostics: Vec<diagnostics::ElabDiagnostic>,
}

impl<'surface, 'hir, 'core> ElabCtx<'surface, 'hir, 'core> {
    pub fn new(
        bump: &'core bumpalo::Bump,
        syntax_map: &'hir pion_hir::syntax::LocalSyntaxMap<'surface, 'hir>,
    ) -> Self {
        Self {
            bump,
            type_map: TypeMap::with_capacity(syntax_map.exprs.len(), syntax_map.pats.len()),
            syntax_map,
            local_env: LocalEnv::default(),
            meta_env: MetaEnv::default(),
            renaming: unify::PartialRenaming::new(),
            diagnostics: Vec::default(),
        }
    }

    pub fn finish_with<T>(mut self, value: T) -> ElabResult<'hir, 'core, T> {
        let mut quote_env =
            QuoteEnv::new(self.bump, &self.meta_env.values, &mut self.local_env.names);
        let metavars = self.bump.alloc_slice_fill_iter(
            self.meta_env
                .values
                .iter()
                .map(|value| value.as_ref().map(|value| quote_env.quote(value))),
        );

        let meta_env = std::mem::take(&mut self.meta_env);
        for entry in meta_env.iter() {
            match (entry.value, entry.source) {
                // Ignore solved metas
                (Some(_), _) => {}

                // These should produce an unsolved UnderscoreExpr, so avoid reporting
                // unsolved meta twice
                (None, MetaSource::UnderscoreType { .. }) => {}

                (None, source) => {
                    self.emit_diagnostic(diagnostics::ElabDiagnostic::UnsolvedMeta { source });
                }
            }
        }

        ElabResult {
            value,
            metavars,
            type_map: self.type_map,
            diagnostics: self.diagnostics,
        }
    }

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
        EvalEnv::new(self.bump, &self.meta_env.values, &mut self.local_env.values)
    }

    pub fn quote_env(&mut self) -> QuoteEnv<'core, '_> {
        QuoteEnv::new(self.bump, &self.meta_env.values, &mut self.local_env.names)
    }

    pub fn zonk_env<'out>(&mut self, out_bump: &'out bumpalo::Bump) -> ZonkEnv<'core, '_, 'out> {
        ZonkEnv::new(
            out_bump,
            self.bump,
            &self.meta_env.values,
            &mut self.local_env.values,
            &mut self.local_env.names,
        )
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

    fn with_def<T>(
        &mut self,
        name: Option<Symbol>,
        r#type: Type<'core>,
        value: Value<'core>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.local_env.push_def(name, r#type, value);
        let result = f(self);
        self.local_env.pop();
        result
    }

    fn pretty_value(&mut self, value: &Value<'_>) -> Box<str> {
        let expr = self.quote_env().quote(value);
        let pretty_ctx = pretty::PrettyCtx::new(self.bump);
        let doc = pretty_ctx.expr(&expr, pretty::Prec::MAX);
        doc.pretty(80).to_string().into()
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

    fn is_empty(&self) -> bool { self.sources.is_empty() }

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
    UnderscoreType {
        span: ByteSpan,
    },
    UnderscoreExpr {
        span: ByteSpan,
    },
    EmptyArrayElemType {
        span: ByteSpan,
    },
    ImplicitArg {
        span: ByteSpan,
        name: Option<Symbol>,
    },
    PatType {
        span: ByteSpan,
    },
}

#[derive(Debug, Clone, Default)]
pub struct TypeMap<'hir, 'core> {
    pub exprs: IntMap<Identity<&'hir pion_hir::syntax::Expr<'hir>>, Expr<'core>>,
    pub pats: IntMap<Identity<&'hir pion_hir::syntax::Pat<'hir>>, Expr<'core>>,
}

impl<'hir, 'core> TypeMap<'hir, 'core> {
    pub fn new() -> Self { Self::default() }

    pub fn with_capacity(exprs: usize, pats: usize) -> Self {
        Self {
            exprs: IntMap::with_capacity_and_hasher(exprs, BuildHasherDefault::default()),
            pats: IntMap::with_capacity_and_hasher(pats, BuildHasherDefault::default()),
        }
    }

    pub fn shrink_to_fit(&mut self) {
        self.exprs.shrink_to_fit();
        self.pats.shrink_to_fit();
    }

    pub fn insert_expr(&mut self, expr: &'hir pion_hir::syntax::Expr<'hir>, core: Expr<'core>) {
        self.exprs.insert(Identity(expr), core);
    }

    pub fn insert_pat(&mut self, pat: &'hir pion_hir::syntax::Pat<'hir>, core: Expr<'core>) {
        self.pats.insert(Identity(pat), core);
    }
}

impl<'hir, 'core> std::ops::Index<&'hir pion_hir::syntax::Expr<'hir>> for TypeMap<'hir, 'core> {
    type Output = Expr<'core>;

    fn index(&self, expr: &'hir pion_hir::syntax::Expr<'hir>) -> &Self::Output {
        &self.exprs[&Identity(expr)]
    }
}

impl<'hir, 'core> std::ops::Index<&'hir pion_hir::syntax::Pat<'hir>> for TypeMap<'hir, 'core> {
    type Output = Expr<'core>;

    fn index(&self, pat: &'hir pion_hir::syntax::Pat<'hir>) -> &Self::Output {
        &self.pats[&Identity(pat)]
    }
}

#[derive(Debug, Clone)]
pub struct ElabResult<'hir, 'core, T> {
    pub value: T,
    pub metavars: &'core [Option<Expr<'core>>],
    pub type_map: TypeMap<'hir, 'core>,
    pub diagnostics: Vec<diagnostics::ElabDiagnostic>,
}

impl<'hir, 'core, T> ElabResult<'hir, 'core, T> {
    pub fn map<V>(self, f: impl Fn(T) -> V) -> ElabResult<'hir, 'core, V> {
        ElabResult {
            value: f(self.value),
            metavars: self.metavars,
            type_map: self.type_map,
            diagnostics: self.diagnostics,
        }
    }
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

pub type ItemMap<'hir, 'core> = Vec<ItemData<'hir, 'core>>;
pub type ItemData<'hir, 'core> = ElabResult<'hir, 'core, ()>;

pub fn elab_def<'surface, 'hir, 'core>(
    bump: &'core bumpalo::Bump,
    syntax_map: &'hir pion_hir::syntax::LocalSyntaxMap<'surface, 'hir>,
    def: &'hir pion_hir::syntax::Def<'hir>,
) -> ElabResult<'hir, 'core, Def<'core>> {
    let mut ctx = ElabCtx::new(bump, syntax_map);

    let (expr, r#type) = match &def.r#type {
        None => {
            let Synth(expr, r#type) = ctx.synth_expr(&def.expr);
            let r#type = ctx.quote_env().quote(&r#type);

            (expr, r#type)
        }
        Some(r#type) => {
            let Check(r#type) = ctx.check_expr(r#type, &Type::TYPE);
            let type_value = ctx.eval_env().eval(&r#type);
            let Check(expr) = ctx.check_expr(&def.expr, &type_value);

            (expr, r#type)
        }
    };
    let expr = ctx.zonk_env(bump).zonk(&expr);
    let r#type = ctx.zonk_env(bump).zonk(&r#type);

    let def = Def {
        name: def.name,
        r#type,
        expr,
    };

    ctx.finish_with(def)
}

pub fn elab_expr<'surface, 'hir, 'core>(
    bump: &'core bumpalo::Bump,
    syntax_map: &'hir pion_hir::syntax::LocalSyntaxMap<'surface, 'hir>,
    expr: &'hir pion_hir::syntax::Expr<'hir>,
) -> ElabResult<'hir, 'core, (Expr<'core>, Expr<'core>)> {
    let mut ctx = ElabCtx::new(bump, syntax_map);

    let Synth(expr, r#type) = ctx.synth_expr(expr);
    let r#type = ctx.quote_env().quote(&r#type);

    let expr = ctx.zonk_env(bump).zonk(&expr);
    let r#type = ctx.zonk_env(bump).zonk(&r#type);
    ctx.finish_with((expr, r#type))
}
