use std::hash::BuildHasherDefault;

use fxhash::FxHasher;
pub use lasso;

pub type Symbol = lasso::Spur;
pub type Interner = lasso::ThreadedRodeo<Symbol>;
