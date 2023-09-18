use pion_utils::symbol::Symbol;

use crate::env::SharedEnv;
use crate::name::BinderName;
use crate::syntax::*;

macro_rules! define_prims {
    ($($name:ident => $str:expr),*,) => {
        define_prims!($($name => $str),*);
    };

    ($($name:ident => $str:expr),*) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub enum Prim {
            $($name),*
        }

        impl Prim {
            pub const ALL: &'static [Self] = &[$(Self::$name,)*];

            pub fn name(self) -> &'static str {
                match self {
                    $(Self::$name => $str),*
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
    Type => "Type",
    Bool => "Bool",
    Int => "Int",
    Array => "Array",
}

impl Prim {
    pub const fn r#type(self) -> Type<'static> {
        const TYPE: &Type = &Type::TYPE;

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
        }
    }
}
