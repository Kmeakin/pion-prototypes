use std::fmt;
use std::hash::BuildHasherDefault;

use nohash::IntMap;
use pion_utils::identity::{Identity, PtrMap};
use pion_utils::location::ByteSpan;
use pion_utils::slice_vec::SliceVec;
use pion_utils::symbol::Symbol;

use self::unify::UnifyCtx;
use crate::env::{EnvLen, Index, Level, SharedEnv, UniqueEnv};
use crate::name::BinderName;
use crate::pretty;
use crate::semantics::{ElimEnv, EvalEnv, QuoteEnv, ZonkEnv};
use crate::syntax::*;

mod diagnostics;
mod expr;
mod r#match;
mod pat;
mod unify;

pub mod dependencies;

pub struct ElabCtx<'hir, 'core> {
    bump: &'core bumpalo::Bump,
    item_env: ItemEnv<'core>,
    local_env: LocalEnv<'core>,
    meta_env: MetaEnv<'core>,
    renaming: unify::PartialRenaming,
    type_map: TypeMap<'hir, 'core>,
    diagnostics: Vec<diagnostics::ElabDiagnostic>,
}

impl<'hir, 'core> ElabCtx<'hir, 'core> {
    pub fn new(bump: &'core bumpalo::Bump) -> Self {
        Self {
            bump,
            type_map: TypeMap::with_capacity(0, 0),
            item_env: ItemEnv::default(),
            local_env: LocalEnv::default(),
            meta_env: MetaEnv::default(),
            renaming: unify::PartialRenaming::new(),
            diagnostics: Vec::default(),
        }
    }

    pub fn finish_with<T>(mut self, value: T) -> ElabResult<'hir, 'core, T> {
        let mut zonk_env = ZonkEnv::new(
            self.bump,
            self.bump,
            &self.meta_env.values,
            &mut self.local_env.values,
            &self.item_env.values,
            &mut self.local_env.names,
            &self.item_env.names,
        );
        let metavars = self.bump.alloc_slice_fill_iter(
            self.meta_env
                .values
                .iter()
                .map(|value| value.as_ref().map(|value| zonk_env.zonk_value(value))),
        );

        let meta_env = std::mem::take(&mut self.meta_env);
        for entry in meta_env.iter() {
            if entry.value.is_none() {
                // `UnderscoreType` and `UnderscoreExpr` are produced in pairs, so dont report
                // unsolved metas twice
                if let MetaSource::UnderscoreType { .. } = entry.source {
                    continue;
                }

                self.emit_diagnostic(diagnostics::ElabDiagnostic::UnsolvedMeta {
                    source: entry.source,
                });
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

        let mut expr = Expr::Meta(level);
        for (level, info) in Level::iter().zip(self.local_env.infos.iter()) {
            match info {
                BinderInfo::Def => {}
                BinderInfo::Param => {
                    let index = self.local_env.len().level_to_index(level).unwrap();
                    expr =
                        Expr::fun_app(self.bump, Plicity::Explicit, expr, Expr::Local((), index));
                }
            }
        }
        expr
    }

    fn push_unsolved_type(&mut self, source: MetaSource) -> Value<'core> {
        let expr = self.push_unsolved_expr(source, Value::TYPE);
        self.eval_env().eval(&expr)
    }

    fn emit_diagnostic(&mut self, diagnostic: diagnostics::ElabDiagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn elim_env(&self) -> ElimEnv<'core, '_> {
        ElimEnv::new(self.bump, &self.meta_env.values, &self.item_env.values)
    }

    pub fn eval_env(&mut self) -> EvalEnv<'core, '_> {
        EvalEnv::new(
            self.bump,
            &self.meta_env.values,
            &mut self.local_env.values,
            &self.item_env.values,
        )
    }

    pub fn quote_env(&mut self) -> QuoteEnv<'core, '_> {
        QuoteEnv::new(
            self.bump,
            &self.meta_env.values,
            &self.item_env.values,
            self.local_env.len(),
        )
    }

    pub fn zonk_env<'out>(&mut self, out_bump: &'out bumpalo::Bump) -> ZonkEnv<'core, '_, 'out> {
        ZonkEnv::new(
            out_bump,
            self.bump,
            &self.meta_env.values,
            &mut self.local_env.values,
            &self.item_env.values,
            &mut self.local_env.names,
            &self.item_env.names,
        )
    }

    pub fn unifiy_ctx(&mut self) -> UnifyCtx<'core, '_> {
        UnifyCtx::new(
            self.bump,
            &mut self.renaming,
            self.local_env.len(),
            &mut self.meta_env.values,
            &self.item_env.values,
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
        name: BinderName,
        r#type: Type<'core>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.local_env.push_param(name, r#type);
        let result = f(self);
        self.local_env.pop();
        result
    }

    fn pretty_value(&mut self, value: &Value<'core>) -> Box<str> {
        let expr = self.zonk_env(self.bump).zonk_value(value);
        let pretty_ctx = pretty::PrettyCtx::new(self.bump);
        let doc = pretty_ctx.expr(&expr, pretty::Prec::MAX);
        doc.pretty(80).to_string().into()
    }
}

#[derive(Default)]
struct ItemEnv<'core> {
    names: UniqueEnv<Symbol>,
    types: UniqueEnv<Type<'core>>,
    values: SharedEnv<Value<'core>>,
}

impl<'core> ItemEnv<'core> {
    fn push(&mut self, name: Symbol, r#type: Type<'core>, value: Value<'core>) {
        self.names.push(name);
        self.types.push(r#type);
        self.values.push(value);
    }

    fn lookup(&self, name: Symbol) -> Option<(Level, ItemEntry<'_, 'core>)> {
        let level = self.names.level_of_elem(&name)?;
        let r#type = self.types.get_level(level)?;
        let value = self.values.get_level(level)?;
        Some((
            level,
            ItemEntry {
                name,
                r#type,
                value,
            },
        ))
    }

    fn len(&self) -> EnvLen { self.names.len() }

    fn truncate(&mut self, len: EnvLen) {
        self.names.truncate(len);
        self.types.truncate(len);
        self.values.truncate(len);
    }
}

#[derive(Debug)]
#[allow(dead_code)]
struct ItemEntry<'env, 'core> {
    name: Symbol,
    r#type: &'env Type<'core>,
    value: &'env Value<'core>,
}

#[derive(Default)]
struct LocalEnv<'core> {
    names: UniqueEnv<BinderName>,
    infos: UniqueEnv<BinderInfo>,
    types: UniqueEnv<Type<'core>>,
    values: SharedEnv<Value<'core>>,
}

#[derive(Debug)]
#[allow(dead_code)]
struct LocalEntry<'env, 'core> {
    name: BinderName,
    info: BinderInfo,
    r#type: &'env Type<'core>,
    value: &'env Value<'core>,
}

#[allow(dead_code)]
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
        name: BinderName,
        info: BinderInfo,
        r#type: Type<'core>,
        value: Value<'core>,
    ) {
        self.names.push(name);
        self.infos.push(info);
        self.types.push(r#type);
        self.values.push(value);
    }

    fn push_def(&mut self, name: BinderName, r#type: Type<'core>, value: Value<'core>) {
        self.push(name, BinderInfo::Def, r#type, value);
    }

    fn push_param(&mut self, name: BinderName, r#type: Type<'core>) {
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

    fn lookup(&self, symbol: Symbol) -> Option<(Index, LocalEntry<'_, 'core>)> {
        let name = BinderName::User(symbol);
        let index = self.names.index_of_elem(&name)?;
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
    types: UniqueEnv<Type<'core>>, // TODO: do we actually need to track types of metavars?
    values: UniqueEnv<Option<Value<'core>>>,
}

#[derive(Debug)]
#[allow(dead_code)]
struct MetaEntry<'env, 'core> {
    source: MetaSource,
    r#type: &'env Type<'core>,
    value: &'env Option<Value<'core>>,
}

#[allow(dead_code)]
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
    UnderscoreType { span: ByteSpan },
    UnderscoreExpr { span: ByteSpan },
    EmptyArrayElemType { span: ByteSpan },
    ImplicitArg { span: ByteSpan, name: BinderName },
    PatType { span: ByteSpan },
    MatchType { span: ByteSpan },
    ItemType { span: ByteSpan },
}

#[derive(Debug, Clone, Default)]
pub struct TypeMap<'hir, 'core> {
    pub exprs: PtrMap<&'hir pion_hir::syntax::Expr<'hir>, ZonkedExpr<'core>>,
    pub pats: PtrMap<&'hir pion_hir::syntax::Pat<'hir>, ZonkedExpr<'core>>,
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

    pub fn insert_expr(
        &mut self,
        expr: &'hir pion_hir::syntax::Expr<'hir>,
        core: ZonkedExpr<'core>,
    ) {
        self.exprs.insert(Identity(expr), core);
    }

    pub fn insert_pat(&mut self, pat: &'hir pion_hir::syntax::Pat<'hir>, core: ZonkedExpr<'core>) {
        self.pats.insert(Identity(pat), core);
    }
}

impl<'hir, 'core> std::ops::Index<&'hir pion_hir::syntax::Expr<'hir>> for TypeMap<'hir, 'core> {
    type Output = ZonkedExpr<'core>;

    fn index(&self, expr: &'hir pion_hir::syntax::Expr<'hir>) -> &Self::Output {
        &self.exprs[&Identity(expr)]
    }
}

impl<'hir, 'core> std::ops::Index<&'hir pion_hir::syntax::Pat<'hir>> for TypeMap<'hir, 'core> {
    type Output = ZonkedExpr<'core>;

    fn index(&self, pat: &'hir pion_hir::syntax::Pat<'hir>) -> &Self::Output {
        &self.pats[&Identity(pat)]
    }
}

#[derive(Debug, Clone)]
pub struct ElabResult<'hir, 'core, T> {
    pub value: T,
    pub metavars: &'core [Option<ZonkedExpr<'core>>],
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

pub fn elab_module<'hir, 'core>(
    bump: &'core bumpalo::Bump,
    module: &pion_hir::syntax::Module<'hir>,
) -> ElabResult<'hir, 'core, Module<'core>> {
    let sccs = dependencies::module_sccs(module);
    let mut ctx = ElabCtx::new(bump);
    let mut module_items = SliceVec::new(bump, module.items.len());

    for scc in sccs {
        let initial_len = ctx.item_env.len();

        // check item type annotations
        let item_types: &[_] = bump.alloc_slice_fill_iter(scc.iter().map(|item| match item {
            pion_hir::syntax::Item::Def(def) => match def.r#type {
                Some(r#type) => {
                    let Check(r#type) = ctx.check_expr_is_type(r#type);
                    ctx.eval_env().eval(&r#type)
                }
                None => ctx.push_unsolved_type(MetaSource::ItemType {
                    span: def.name.span,
                }),
            },
        }));

        // initialize items
        for (item, r#type) in scc.iter().zip(item_types) {
            match item {
                pion_hir::syntax::Item::Def(def) => {
                    let name = def.name.symbol;
                    let value = Value::item(ctx.item_env.len().to_level());
                    ctx.item_env.push(name, r#type.clone(), value);
                }
            }
        }

        // check items
        let item_exprs = bump.alloc_slice_fill_iter(scc.iter().zip(item_types).map(
            |(item, expected)| match item {
                pion_hir::syntax::Item::Def(def) => {
                    let Check(expr) = ctx.check_ann_expr(def.expr, def.r#type, expected);
                    expr
                }
            },
        ));

        // evaluate items
        let item_values =
            bump.alloc_slice_fill_iter(item_exprs.iter().map(|expr| ctx.eval_env().eval(expr)));

        // update item env
        ctx.item_env.truncate(initial_len);
        for ((item, r#type), value) in scc.iter().zip(item_types).zip(item_values) {
            match item {
                pion_hir::syntax::Item::Def(def) => {
                    ctx.item_env
                        .push(def.name.symbol, r#type.clone(), value.clone());
                }
            }
        }

        // write back
        for ((item, r#type), expr) in scc.iter().zip(item_types).zip(item_exprs) {
            match item {
                pion_hir::syntax::Item::Def(def) => {
                    module_items.push(Item::Def(Def {
                        name: def.name,
                        r#type: ctx.zonk_env(bump).zonk_value(r#type),
                        expr: ctx.zonk_env(bump).zonk(expr),
                    }));
                }
            }
        }
    }

    let module = Module {
        items: module_items.into(),
    };
    ctx.finish_with(module)
}

// REASON: keep all lifetimes explicit for clarity
#[allow(clippy::needless_lifetimes)]
pub fn elab_def<'hir, 'core>(
    bump: &'core bumpalo::Bump,
    def: &'hir pion_hir::syntax::Def<'hir>,
) -> ElabResult<'hir, 'core, Def<'core>> {
    let mut ctx = ElabCtx::new(bump);

    let (expr, r#type) = match &def.r#type {
        None => {
            let Synth(expr, r#type) = ctx.synth_expr(def.expr);
            let r#type = ctx.quote_env().quote(&r#type);

            (expr, r#type)
        }
        Some(r#type) => {
            let Check(r#type) = ctx.check_expr(r#type, &Type::TYPE);
            let type_value = ctx.eval_env().eval(&r#type);
            let Check(expr) = ctx.check_expr(def.expr, &type_value);

            (expr, r#type)
        }
    };
    let mut zonk_env = ctx.zonk_env(bump);
    let expr = zonk_env.zonk(&expr);
    let r#type = zonk_env.zonk(&r#type);

    let def = Def {
        name: def.name,
        r#type,
        expr,
    };

    ctx.finish_with(def)
}

pub fn elab_expr<'hir, 'core>(
    bump: &'core bumpalo::Bump,
    expr: &'hir pion_hir::syntax::Expr<'hir>,
) -> ElabResult<'hir, 'core, (ZonkedExpr<'core>, ZonkedExpr<'core>)> {
    let mut ctx = ElabCtx::new(bump);

    let Synth(expr, r#type) = ctx.synth_expr(expr);

    let expr = ctx.zonk_env(bump).zonk(&expr);
    let r#type = ctx.zonk_env(bump).zonk_value(&r#type);
    ctx.finish_with((expr, r#type))
}
