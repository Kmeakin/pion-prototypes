use pion_core::env::{EnvLen, RelativeVar, SharedEnv, UniqueEnv};
use pion_core::semantics::{Type, Value};
use pion_symbol::Symbol;
use text_size::TextRange;

use crate::unify::PartialRenaming;

#[derive(Default)]
pub struct ElabEnv<'core> {
    pub locals: LocalEnv<'core>,
    pub metas: MetaEnv<'core>,
    pub renaming: PartialRenaming,
}

#[derive(Default)]
pub struct LocalEnv<'core> {
    pub names: UniqueEnv<Option<Symbol>>,
    pub infos: UniqueEnv<LocalInfo>,
    pub types: UniqueEnv<Type<'core>>,
    pub values: SharedEnv<Value<'core>>,
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

    pub fn next_var(&self) -> Value<'core> { Value::local_var(self.len().to_absolute()) }
}

#[derive(Default)]
pub struct MetaEnv<'core> {
    pub sources: UniqueEnv<MetaSource>,
    pub types: UniqueEnv<Type<'core>>,
    pub values: UniqueEnv<Option<Value<'core>>>,
}

impl<'core> MetaEnv<'core> {
    pub fn len(&self) -> EnvLen { self.sources.len() }

    pub fn push(&mut self, source: MetaSource, r#type: Type<'core>) {
        self.sources.push(source);
        self.types.push(r#type);
        self.values.push(None);
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (MetaSource, &Type<'core>, &Option<Value<'core>>)> + '_ {
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
