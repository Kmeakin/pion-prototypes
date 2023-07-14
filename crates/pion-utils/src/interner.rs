pub use lasso;

pub type Symbol = lasso::Spur;
pub type Interner = lasso::ThreadedRodeo<Symbol>;
