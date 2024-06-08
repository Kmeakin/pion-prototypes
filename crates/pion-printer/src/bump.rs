use std::ops::Deref;

use bumpalo::Bump;
use pretty::{Doc, DocAllocator, DocPtr, RefDoc};

/// A wrapper around a `Bump` that implements `DocAllocator`.
#[derive(Debug, Copy, Clone)]
pub struct BumpDocAllocator<'bump> {
    bump: &'bump Bump,
}

impl<'bump> From<&'bump Bump> for BumpDocAllocator<'bump> {
    fn from(bump: &'bump Bump) -> Self { Self { bump } }
}

impl<'bump> BumpDocAllocator<'bump> {
    pub const fn new(bump: &'bump Bump) -> Self { Self { bump } }
}

impl<'bump> Deref for BumpDocAllocator<'bump> {
    type Target = Bump;
    fn deref(&self) -> &Self::Target { self.bump }
}

impl<'bump, A: 'bump> DocAllocator<'bump, A> for BumpDocAllocator<'bump> {
    type Doc = RefDoc<'bump, A>;

    fn alloc(&'bump self, doc: Doc<'bump, Self::Doc, A>) -> Self::Doc {
        RefDoc(self.bump.alloc(doc))
    }

    fn alloc_column_fn(
        &'bump self,
        f: impl Fn(usize) -> Self::Doc + 'bump,
    ) -> <Self::Doc as DocPtr<'bump, A>>::ColumnFn {
        self.bump.alloc(f)
    }

    fn alloc_width_fn(
        &'bump self,
        f: impl Fn(isize) -> Self::Doc + 'bump,
    ) -> <Self::Doc as DocPtr<'bump, A>>::WidthFn {
        self.bump.alloc(f)
    }
}
