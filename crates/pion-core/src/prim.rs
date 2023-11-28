use pion_utils::symbol::Symbol;

use crate::env::SharedEnv;
use crate::name::BinderName;
use crate::syntax::*;

macro_rules! define_prims {
    ($($name:ident),*,) => {
        define_prims!($($name),*);
    };

    ($($name:ident),*) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        #[allow(non_camel_case_types)]
        pub enum Prim {
            $($name),*
        }

        impl Prim {
            pub const ALL: &'static [Self] = &[$(Self::$name,)*];

            pub fn name(self) -> &'static str {
                match self {
                    $(Self::$name => stringify!($name)),*
                }
            }

            pub fn symbol(self) -> Symbol {
                match self {
                    $(Self::$name => Symbol::$name,)*
                }
            }

            pub fn from_symbol(sym: Symbol) -> Option<Self> {
                match sym {
                    $(Symbol::$name => Some(Self::$name),)*
                    _ => None,
                }
            }
        }
    };
}

define_prims! {
    Type, Bool, Int, Array,
    add, sub, mul,
    eq, ne, lt, gt, lte, gte,
}

impl Prim {
    pub const fn r#type(self) -> Type<'static> {
        const TYPE: &Type = &Type::TYPE;
        const INT: &Type = &Type::INT;

        match self {
            Self::Type | Self::Bool | Self::Int => Type::TYPE,
            Self::Array => Type::FunType(
                Plicity::Explicit,
                BinderName::User(Symbol::A),
                TYPE,
                Closure::new(
                    SharedEnv::new(),
                    &Expr::FunType(
                        Plicity::Explicit,
                        BinderName::User(Symbol::N),
                        &(Expr::INT, Expr::TYPE),
                    ),
                ),
            ),
            Self::add | Self::sub | Self::mul => Type::FunType(
                Plicity::Explicit,
                BinderName::User(Symbol::x),
                INT,
                Closure::new(
                    SharedEnv::new(),
                    &Expr::FunType(
                        Plicity::Explicit,
                        BinderName::User(Symbol::y),
                        &(Expr::INT, Expr::INT),
                    ),
                ),
            ),
            Self::eq | Self::ne | Self::lt | Self::gt | Self::lte | Self::gte => Type::FunType(
                Plicity::Explicit,
                BinderName::User(Symbol::x),
                INT,
                Closure::new(
                    SharedEnv::new(),
                    &Expr::FunType(
                        Plicity::Explicit,
                        BinderName::User(Symbol::y),
                        &(Expr::INT, Expr::BOOL),
                    ),
                ),
            ),
        }
    }
}
