use super::semantics::Type;
use crate::core::semantics::Closure;
use crate::core::syntax::{Expr, FunParam};
use crate::env::RelativeVar;
use crate::plicity::Plicity;
use crate::symbol::Symbol;

macro_rules! prims {
    ($($prim:ident),*) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        #[allow(non_camel_case_types)]
        pub enum Prim {
            $($prim,)*
        }

        impl Prim {
            pub const fn name(self) -> &'static str {
                match self {
                    $(Self::$prim => stringify!($prim),)*
                }
            }

            pub const fn from_symbol(sym: Symbol) -> Option<Self> {
                match sym {
                    $(Symbol::$prim => Some(Self::$prim),)*
                    _ => None,
                }
            }
        }
    };
}

prims! {
    Type, Int, Bool,
    add, sub, mul,
    eq, ne, gt, lt, gte, lte,
    fix
}

impl Prim {
    pub const fn r#type(self) -> Type<'static> {
        use Plicity::{Explicit, Implicit};

        const TYPE: &Type<'static> = &Type::TYPE;
        const INT: &Type<'static> = &Type::INT;

        const VAR1: Expr = Expr::LocalVar(RelativeVar::new(1));
        const VAR2: Expr = Expr::LocalVar(RelativeVar::new(2));

        #[allow(clippy::match_same_arms)]
        match self {
            // `Type : Type`
            // `Int : Type`
            // `Bool : Type`
            Self::Type | Self::Int | Self::Bool => Type::TYPE,

            // `add : Int -> Int -> Int`
            // `sub : Int -> Int -> Int`
            // `mul : Int -> Int -> Int`
            Self::add | Self::sub | Self::mul => Type::FunType {
                param: FunParam {
                    plicity: Explicit,
                    name: None,
                    r#type: INT,
                },
                body: Closure::empty(&Expr::FunType {
                    param: FunParam {
                        plicity: Explicit,
                        name: None,
                        r#type: &Expr::INT,
                    },
                    body: &Expr::INT,
                }),
            },

            // `eq : Int -> Int -> Bool`
            // `ne : Int -> Int -> Bool`
            // `lt : Int -> Int -> Bool`
            // `gt : Int -> Int -> Bool`
            // `lte : Int -> Int -> Bool`
            // `gte : Int -> Int -> Bool`
            Self::eq | Self::ne | Self::lt | Self::gt | Self::lte | Self::gte => Type::FunType {
                param: FunParam {
                    plicity: Explicit,
                    name: None,
                    r#type: INT,
                },
                body: Closure::empty(&Expr::FunType {
                    param: FunParam {
                        plicity: Explicit,
                        name: None,
                        r#type: &Expr::INT,
                    },
                    body: &Expr::BOOL,
                }),
            },

            // `fix: forall (@A : Type) (@B : Type) -> ((A -> B) -> A -> B) -> A -> B`
            Self::fix => Type::FunType {
                param: FunParam {
                    plicity: Implicit,
                    name: Some(Symbol::A),
                    r#type: TYPE,
                },
                body: Closure::empty(&Expr::FunType {
                    param: FunParam {
                        plicity: Implicit,
                        name: Some(Symbol::B),
                        r#type: &Expr::TYPE,
                    },
                    body: &Expr::FunType {
                        param: FunParam {
                            plicity: Explicit,
                            name: None,
                            // ((A -> B) -> A -> B)
                            r#type: &Expr::FunType {
                                param: FunParam {
                                    plicity: Explicit,
                                    name: None,
                                    // (A -> B)
                                    r#type: &Expr::FunType {
                                        param: FunParam {
                                            plicity: Explicit,
                                            name: None,
                                            r#type: &VAR1,
                                        },
                                        body: &VAR1,
                                    },
                                },

                                // (A -> B)
                                body: &Expr::FunType {
                                    param: FunParam {
                                        plicity: Explicit,
                                        name: None,
                                        r#type: &VAR2,
                                    },
                                    body: &VAR2,
                                },
                            },
                        },
                        // (A -> B)
                        body: &Expr::FunType {
                            param: FunParam {
                                plicity: Explicit,
                                name: None,
                                r#type: &VAR2,
                            },
                            body: &VAR2,
                        },
                    },
                }),
            },
        }
    }
}
