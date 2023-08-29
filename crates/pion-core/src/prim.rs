use crate::env::SharedEnv;
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
            pub fn name(&self)-> &'static str {
                match self {
                    $(Self::$name => $str),*
                }
            }
        }

        impl Prim {
            pub const ALL: &'static [Self] = &[$(Self::$name,)*];
        }

        impl std::str::FromStr for Prim {
                type Err = ();

                fn from_str(s: &str)-> Result<Self, ()> {
                    match s {
                        $($str => Ok(Self::$name),)*
                        _ => Err(()),
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
    pub fn r#type(self) -> Type<'static> {
        const TYPE: &Type = &Type::TYPE;

        match self {
            Self::Type | Self::Bool | Self::Int => Type::TYPE,
            Self::Array => Type::FunType(
                Plicity::Explicit,
                None,
                TYPE,
                Closure::new(
                    SharedEnv::new(),
                    &Expr::FunType(Plicity::Explicit, None, &(Expr::INT, Expr::TYPE)),
                ),
            ),
        }
    }
}
