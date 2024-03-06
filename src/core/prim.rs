use std::str::FromStr;

use super::semantics::Type;
use crate::core::semantics::Closure;
use crate::core::syntax::{Expr, FunArg, FunParam};
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
        }

        impl FromStr for Prim {
            type Err = ();
            fn from_str(text: &str) -> Result<Self, Self::Err> {
                match text {
                    $(stringify!($prim) => Ok(Self::$prim),)*
                    _ => Err(()),
                }
            }
        }
    };
}

prims! {
    Type, Int, Bool,
    Unit, MkUnit,
    DPair, MkDPair, dhead, dtail,
    add, sub, mul,
    eq, ne, gt, lt, gte, lte,
    fix
}

impl Prim {
    pub const fn r#type(self) -> Type<'static> {
        use Plicity::{Explicit, Implicit};

        const TYPE: &Type<'static> = &Type::TYPE;
        const INT: &Type<'static> = &Type::INT;

        const VAR0: Expr = Expr::LocalVar(RelativeVar::new(0));
        const VAR1: Expr = Expr::LocalVar(RelativeVar::new(1));
        const VAR2: Expr = Expr::LocalVar(RelativeVar::new(2));
        const VAR3: Expr = Expr::LocalVar(RelativeVar::new(3));

        #[allow(clippy::match_same_arms)]
        match self {
            // `Type : Type`
            // `Int : Type`
            // `Bool : Type`
            Self::Type | Self::Int | Self::Bool => Type::TYPE,

            // `Unit : Type`
            // `MkUnit : Unit`
            Self::Unit => Type::TYPE,
            Self::MkUnit => Type::UNIT,

            // DPair : forall (A : Type) -> (A -> Type) -> Type
            Self::DPair => Type::FunType {
                param: FunParam {
                    plicity: Explicit,
                    name: Some(Symbol::A),
                    r#type: TYPE,
                },
                body: Closure::empty(&Expr::FunType {
                    param: FunParam {
                        plicity: Explicit,
                        name: None,
                        r#type: &Expr::FunType {
                            param: FunParam {
                                plicity: Explicit,
                                name: None,
                                r#type: &VAR0,
                            },
                            body: &Expr::TYPE,
                        },
                    },
                    body: &Expr::TYPE,
                }),
            },
            // MkDPair : forall (@A : Type) (@B : A -> Type) (a : A) -> B a -> DPair A B
            Self::MkDPair => Type::FunType {
                param: FunParam {
                    plicity: Implicit,
                    name: Some(Symbol::A),
                    r#type: TYPE,
                },
                body: Closure::empty(&Expr::FunType {
                    param: FunParam {
                        plicity: Implicit,
                        name: Some(Symbol::B),
                        r#type: &Expr::FunType {
                            param: FunParam {
                                plicity: Explicit,
                                name: None,
                                r#type: &VAR0,
                            },
                            body: &Expr::TYPE,
                        },
                    },
                    body: &Expr::FunType {
                        param: FunParam {
                            plicity: Explicit,
                            name: Some(Symbol::a),
                            r#type: &VAR1,
                        },
                        body: &Expr::FunType {
                            param: FunParam {
                                plicity: Explicit,
                                name: None,
                                r#type: &Expr::FunApp {
                                    fun: &VAR1,
                                    arg: FunArg {
                                        plicity: Explicit,
                                        expr: &VAR0,
                                    },
                                },
                            },
                            body: &Expr::FunApp {
                                fun: &Expr::FunApp {
                                    fun: &Expr::Prim(Self::DPair),
                                    arg: FunArg {
                                        plicity: Explicit,
                                        expr: &VAR3,
                                    },
                                },
                                arg: FunArg {
                                    plicity: Explicit,
                                    expr: &VAR2,
                                },
                            },
                        },
                    },
                }),
            },
            // dhead : forall (@A: Type) (@B: A -> Type) -> DPair A B -> A
            Self::dhead => Type::FunType {
                param: FunParam {
                    plicity: Implicit,
                    name: Some(Symbol::A),
                    r#type: TYPE,
                },
                body: Closure::empty(&Expr::FunType {
                    param: FunParam {
                        plicity: Implicit,
                        name: Some(Symbol::B),
                        r#type: &Expr::FunType {
                            param: FunParam {
                                plicity: Explicit,
                                name: None,
                                r#type: &VAR0,
                            },
                            body: &Expr::TYPE,
                        },
                    },
                    body: &Expr::FunType {
                        param: FunParam {
                            plicity: Explicit,
                            name: None,
                            r#type: &Expr::FunApp {
                                fun: &Expr::FunApp {
                                    fun: &Expr::Prim(Self::DPair),
                                    arg: FunArg {
                                        plicity: Explicit,
                                        expr: &VAR1,
                                    },
                                },
                                arg: FunArg {
                                    plicity: Explicit,
                                    expr: &VAR0,
                                },
                            },
                        },
                        body: &VAR2,
                    },
                }),
            },
            // dtail : forall (@A: Type) (@B: A -> Type) -> (p : DPair A B) -> B (dhead @A @B p)
            Self::dtail => Type::FunType {
                param: FunParam {
                    plicity: Implicit,
                    name: Some(Symbol::A),
                    r#type: TYPE,
                },
                body: Closure::empty(&Expr::FunType {
                    param: FunParam {
                        plicity: Implicit,
                        name: Some(Symbol::B),
                        r#type: &Expr::FunType {
                            param: FunParam {
                                plicity: Explicit,
                                name: None,
                                r#type: &VAR0,
                            },
                            body: &Expr::TYPE,
                        },
                    },
                    body: &Expr::FunType {
                        param: FunParam {
                            plicity: Explicit,
                            name: Some(Symbol::p),
                            r#type: &Expr::FunApp {
                                fun: &Expr::FunApp {
                                    fun: &Expr::Prim(Self::DPair),
                                    arg: FunArg {
                                        plicity: Explicit,
                                        expr: &VAR1,
                                    },
                                },
                                arg: FunArg {
                                    plicity: Explicit,
                                    expr: &VAR0,
                                },
                            },
                        },
                        body: &Expr::FunApp {
                            fun: &VAR1,
                            arg: FunArg {
                                plicity: Explicit,
                                expr: &Expr::FunApp {
                                    fun: &Expr::FunApp {
                                        fun: &Expr::FunApp {
                                            fun: &Expr::Prim(Self::dhead),
                                            arg: FunArg {
                                                plicity: Implicit,
                                                expr: &VAR2,
                                            },
                                        },
                                        // @B
                                        arg: FunArg {
                                            plicity: Implicit,
                                            expr: &VAR1,
                                        },
                                    },
                                    // p
                                    arg: FunArg {
                                        plicity: Explicit,
                                        expr: &VAR0,
                                    },
                                },
                            },
                        },
                    },
                }),
            },

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
