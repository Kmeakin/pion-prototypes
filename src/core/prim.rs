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
    fix,
    Eq, refl, subst
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
        const VAR4: Expr = Expr::LocalVar(RelativeVar::new(4));

        #[allow(clippy::match_same_arms)]
        #[allow(clippy::use_self)]
        match self {
            // `Type : Type`
            // `Int : Type`
            // `Bool : Type`
            Self::Type | Self::Int | Self::Bool => Type::TYPE,

            // `add : Int -> Int -> Int`
            // `sub : Int -> Int -> Int`
            // `mul : Int -> Int -> Int`
            Self::add | Self::sub | Self::mul => Type::FunType {
                param: FunParam::explicit(None, INT),
                body: Closure::empty(
                    &const {
                        Expr::FunType {
                            param: FunParam::explicit(None, &Expr::INT),
                            body: &Expr::INT,
                        }
                    },
                ),
            },

            // `eq : Int -> Int -> Bool`
            // `ne : Int -> Int -> Bool`
            // `lt : Int -> Int -> Bool`
            // `gt : Int -> Int -> Bool`
            // `lte : Int -> Int -> Bool`
            // `gte : Int -> Int -> Bool`
            Self::eq | Self::ne | Self::lt | Self::gt | Self::lte | Self::gte => Type::FunType {
                param: FunParam::explicit(None, INT),
                body: Closure::empty(
                    &const {
                        Expr::FunType {
                            param: FunParam::explicit(None, &Expr::INT),
                            body: &Expr::BOOL,
                        }
                    },
                ),
            },

            // `fix: forall (@A : Type) (@B : Type) -> ((A -> B) -> A -> B) -> A -> B`
            Self::fix => Type::FunType {
                param: FunParam::implicit(Some(Symbol::A), TYPE),
                body: Closure::empty(
                    &const {
                        Expr::FunType {
                            param: FunParam::implicit(Some(Symbol::B), &Expr::TYPE),
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
                                                param: FunParam::explicit(None, &VAR1),
                                                body: &VAR1,
                                            },
                                        },

                                        // (A -> B)
                                        body: &Expr::FunType {
                                            param: FunParam::explicit(None, &VAR2),
                                            body: &VAR2,
                                        },
                                    },
                                },
                                // (A -> B)
                                body: &Expr::FunType {
                                    param: FunParam::explicit(None, &VAR2),
                                    body: &VAR2,
                                },
                            },
                        }
                    },
                ),
            },

            // `Eq : forall (@A : Type) -> A -> A -> Type`
            Self::Eq => Type::FunType {
                param: FunParam {
                    plicity: Implicit,
                    name: Some(Symbol::A),
                    r#type: TYPE,
                },
                body: Closure::empty(
                    &const {
                        Expr::FunType {
                            param: FunParam::explicit(None, &VAR0),
                            body: &Expr::FunType {
                                param: FunParam::explicit(None, &VAR1),
                                body: &Expr::TYPE,
                            },
                        }
                    },
                ),
            },
            // `refl : forall (@A : Type) (a : A) -> Eq @A a a`
            Self::refl => Type::FunType {
                param: FunParam::implicit(Some(Symbol::A), TYPE),
                body: Closure::empty(
                    &const {
                        Expr::FunType {
                            param: FunParam::explicit(Some(Symbol::a), &VAR0),
                            body: &Expr::FunApp {
                                fun: &Expr::FunApp {
                                    fun: &Expr::FunApp {
                                        fun: &Expr::Prim(Prim::Eq),
                                        arg: FunArg::implicit(&VAR1),
                                    },
                                    arg: FunArg::explicit(&VAR0),
                                },
                                arg: FunArg::explicit(&VAR0),
                            },
                        }
                    },
                ),
            },

            // `subst : forall (@A : Type) (@p : A -> Type) (@a : A) (@b : A) -> Eq @A a b -> p a ->
            // p b`
            Self::subst => Type::FunType {
                param: FunParam {
                    plicity: Implicit,
                    name: Some(Symbol::A),
                    r#type: TYPE,
                },
                body: Closure::empty(
                    &const {
                        Expr::FunType {
                            param: FunParam {
                                plicity: Implicit,
                                name: Some(Symbol::p),
                                r#type: &Expr::FunType {
                                    param: FunParam::explicit(None, &VAR0),
                                    body: &Expr::TYPE,
                                },
                            },
                            body: &Expr::FunType {
                                param: FunParam::explicit(Some(Symbol::a), &VAR1),
                                body: &Expr::FunType {
                                    param: FunParam::explicit(Some(Symbol::b), &VAR2),
                                    body: &Expr::FunType {
                                        param: FunParam {
                                            plicity: Explicit,
                                            name: None,
                                            r#type: &Expr::FunApp {
                                                fun: &Expr::FunApp {
                                                    fun: &Expr::FunApp {
                                                        fun: &Expr::Prim(Prim::Eq),
                                                        arg: FunArg::implicit(&VAR3),
                                                    },
                                                    arg: FunArg::explicit(&VAR1),
                                                },
                                                arg: FunArg::explicit(&VAR0),
                                            },
                                        },
                                        body: &Expr::FunType {
                                            param: FunParam {
                                                plicity: Explicit,
                                                name: None,
                                                r#type: &Expr::FunApp {
                                                    fun: &VAR3,
                                                    arg: FunArg::explicit(&VAR2),
                                                },
                                            },
                                            body: &Expr::FunApp {
                                                fun: &VAR4,
                                                arg: FunArg::explicit(&VAR2),
                                            },
                                        },
                                    },
                                },
                            },
                        }
                    },
                ),
            },
        }
    }
}
