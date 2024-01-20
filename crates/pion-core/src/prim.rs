use pion_utils::symbol::Symbol;

use crate::env::{Index, SharedEnv};
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
    Eq, refl, subst,
    bool_rec,
}

impl Prim {
    #[allow(clippy::use_self)]
    pub const fn r#type(self) -> Type<'static> {
        const TYPE: &Type = &Type::TYPE;
        const INT: &Type = &Type::INT;
        const BOOL: &Type = &Type::BOOL;

        const VAR0: Expr = Expr::Local((), Index::new());
        const VAR1: Expr = Expr::Local((), Index::new().next());
        const VAR2: Expr = Expr::Local((), Index::new().next().next());
        const VAR3: Expr = Expr::Local((), Index::new().next().next().next());

        match self {
            Self::Type | Self::Bool | Self::Int => Type::TYPE,
            Self::Array => Type::FunType(
                Plicity::Explicit,
                BinderName::User(Symbol::A),
                TYPE,
                Closure::empty(&Expr::FunType(
                    Plicity::Explicit,
                    BinderName::User(Symbol::N),
                    &(Expr::INT, Expr::TYPE),
                )),
            ),
            Self::add | Self::sub | Self::mul => Type::FunType(
                Plicity::Explicit,
                BinderName::User(Symbol::x),
                INT,
                Closure::empty(&Expr::FunType(
                    Plicity::Explicit,
                    BinderName::User(Symbol::y),
                    &(Expr::INT, Expr::INT),
                )),
            ),
            Self::eq | Self::ne | Self::lt | Self::gt | Self::lte | Self::gte => Type::FunType(
                Plicity::Explicit,
                BinderName::User(Symbol::x),
                INT,
                Closure::empty(&Expr::FunType(
                    Plicity::Explicit,
                    BinderName::User(Symbol::y),
                    &(Expr::INT, Expr::BOOL),
                )),
            ),
            // Eq: fun(@A: Type, x: A, y: A) -> Type
            Self::Eq => Type::FunType(
                Plicity::Implicit,
                BinderName::User(Symbol::A),
                TYPE,
                Closure::empty(&Expr::FunType(
                    Plicity::Explicit,
                    BinderName::User(Symbol::x),
                    &(
                        VAR0,
                        Expr::FunType(
                            Plicity::Explicit,
                            BinderName::User(Symbol::y),
                            &(VAR1, Expr::TYPE),
                        ),
                    ),
                )),
            ),
            // refl: fun(@A: Type, x: A) -> Eq(@A, x, x)
            Self::refl => Type::FunType(
                Plicity::Implicit,
                BinderName::User(Symbol::A),
                TYPE,
                Closure::empty(&Expr::FunType(
                    Plicity::Explicit,
                    BinderName::User(Symbol::x),
                    &(
                        VAR0,
                        Expr::FunApp(
                            Plicity::Explicit,
                            &(
                                Expr::FunApp(
                                    Plicity::Explicit,
                                    &(
                                        Expr::FunApp(
                                            Plicity::Implicit,
                                            &(Expr::Prim(Prim::Eq), VAR1),
                                        ),
                                        VAR0,
                                    ),
                                ),
                                VAR0,
                            ),
                        ),
                    ),
                )),
            ),
            // subst: fun(@A: Type, @x: A, @y: A, @p: A -> Type, x_eq_y: Eq(@A, x, y), p_x: p(x)) ->
            // p(y)
            Self::subst => Type::FunType(
                Plicity::Implicit,
                BinderName::User(Symbol::A),
                TYPE,
                Closure::empty(&Expr::FunType(
                    Plicity::Implicit,
                    BinderName::User(Symbol::x),
                    &(
                        VAR0,
                        Expr::FunType(
                            Plicity::Implicit,
                            BinderName::User(Symbol::y),
                            &(
                                VAR1,
                                Expr::FunType(
                                    Plicity::Implicit,
                                    BinderName::User(Symbol::p),
                                    &(
                                        Expr::FunType(
                                            Plicity::Explicit,
                                            BinderName::Underscore,
                                            &(VAR2, Expr::TYPE),
                                        ),
                                        Expr::FunType(
                                            Plicity::Explicit,
                                            BinderName::User(Symbol::x_eq_y),
                                            &(
                                                Expr::FunApp(
                                                    Plicity::Explicit,
                                                    &(
                                                        Expr::FunApp(
                                                            Plicity::Explicit,
                                                            &(
                                                                Expr::FunApp(
                                                                    Plicity::Implicit,
                                                                    &(Expr::Prim(Prim::Eq), VAR3),
                                                                ),
                                                                VAR2,
                                                            ),
                                                        ),
                                                        VAR1,
                                                    ),
                                                ),
                                                Expr::FunType(
                                                    Plicity::Explicit,
                                                    BinderName::User(Symbol::p_x),
                                                    &(
                                                        Expr::FunApp(
                                                            Plicity::Explicit,
                                                            &(VAR1, VAR3),
                                                        ),
                                                        Expr::FunApp(
                                                            Plicity::Explicit,
                                                            &(VAR2, VAR3),
                                                        ),
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                )),
            ),
            // bool_rec: fun(b: Bool, p: Bool -> Type) -> p(true) -> p(false) -> p(b)
            Self::bool_rec => Type::FunType(
                Plicity::Explicit,
                BinderName::User(Symbol::b),
                BOOL,
                Closure::empty(&Expr::FunType(
                    Plicity::Explicit,
                    BinderName::User(Symbol::p),
                    &(
                        Expr::FunType(
                            Plicity::Explicit,
                            BinderName::Underscore,
                            &(Expr::BOOL, Expr::TYPE),
                        ),
                        Expr::FunType(
                            Plicity::Explicit,
                            BinderName::Underscore,
                            &(
                                Expr::FunApp(Plicity::Explicit, &(VAR0, Expr::TRUE)),
                                Expr::FunType(
                                    Plicity::Explicit,
                                    BinderName::Underscore,
                                    &(
                                        Expr::FunApp(Plicity::Explicit, &(VAR1, Expr::FALSE)),
                                        Expr::FunApp(Plicity::Explicit, &(VAR2, VAR3)),
                                    ),
                                ),
                            ),
                        ),
                    ),
                )),
            ),
        }
    }
}
