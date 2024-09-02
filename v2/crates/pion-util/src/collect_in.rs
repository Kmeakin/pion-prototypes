pub trait CollectIn: ExactSizeIterator + Sized {
    fn collect_in(self, bump: &bumpalo::Bump) -> &[Self::Item] { bump.alloc_slice_fill_iter(self) }
}

impl<I: ExactSizeIterator> CollectIn for I {}
