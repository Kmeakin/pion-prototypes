use std::mem::MaybeUninit;
use std::ops::Deref;

// TODO: investigate if this could be replaced with an existing crate. For
// example:
//
// - https://lib.rs/crates/slicevec
// - https://lib.rs/crates/fixed-slice-vec
//
// Our requirements around drop-glue might be an issue.

/// A helpful type for allocating elements to a slice up to a maximum length.
///
/// This can be helpful if we have initialization code that might be difficult
/// to implement using [`bumpallo::Bump::alloc`], for example:
///
/// - when pushing to multiple slices at once
/// - when element initialization code has the possibility of failure
pub struct SliceVec<'bump, Elem> {
    len: usize,
    // SAFETY: The slice `self.elems[..self.len]` should only ever
    //         contain elements initialized with `MaybeUninit::new`.
    elems: &'bump mut [MaybeUninit<Elem>],
}

impl<'bump, Elem> SliceVec<'bump, Elem> {
    /// Allocates a new slice vec to the scope.
    ///
    /// # Panics
    ///
    /// If the type has drop-glue to be executed.
    pub fn new(bump: &'bump bumpalo::Bump, capacity: usize) -> Self {
        // NOTE: Ensure that that the element type does not have any drop glue.
        //       This would be problematic as we have no way of registering the
        //       drop glue of `Elem` with `scoped_arena::Scope`.
        assert!(!std::mem::needs_drop::<Elem>());

        SliceVec {
            len: 0,
            elems: bump.alloc_slice_fill_with(capacity, |_| MaybeUninit::uninit()),
        }
    }

    pub const fn len(&self) -> usize { self.len }

    pub const fn is_empty(&self) -> bool { self.len() == 0 }

    pub const fn capacity(&self) -> usize { self.elems.len() }

    pub const fn is_full(&self) -> bool { self.len() >= self.capacity() }

    /// Push an element to the slice vec.
    ///
    /// # Panics
    ///
    /// If the pushing the element would exceed the capacity.
    pub fn push(&mut self, elem: Elem) {
        assert!(!self.is_full(), "Cannot push onto a full `SliceVec`");
        self.elems[self.len] = MaybeUninit::new(elem);
        self.len += 1;
    }

    pub fn into_slice(self) -> &'bump [Elem] {
        // SAFETY: This is safe because we know that `self.elems[..self.len]`
        // only ever contains elements initialized with `MaybeUninit::new`.
        // We know this because:
        //
        // - `self.len` is always initialized to `0` in `SliceVec::new`
        // - `self.len` is only incremented in `SliceVec::push`, and in that case we
        //   make sure `self.elems[self.len]` has been initialized before hand.
        unsafe { MaybeUninit::slice_assume_init_ref(&self.elems[..self.len]) }
    }

    pub fn as_slice(&self) -> &[Elem] {
        // SAFETY: This is safe because we know that `self.elems[..self.len]`
        // only ever contains elements initialized with `MaybeUninit::new`.
        // We know this because:
        //
        // - `self.len` is always initialized to `0` in `SliceVec::new`
        // - `self.len` is only incremented in `SliceVec::push`, and in that case we
        //   make sure `self.elems[self.len]` has been initialized before hand.
        unsafe { MaybeUninit::slice_assume_init_ref(&self.elems[..self.len]) }
    }
}

impl<'a, Elem> Deref for SliceVec<'a, Elem> {
    type Target = [Elem];
    fn deref(&self) -> &[Elem] { self.as_slice() }
}

impl<'a, Elem> From<SliceVec<'a, Elem>> for &'a [Elem] {
    fn from(vec: SliceVec<'a, Elem>) -> &'a [Elem] { vec.into_slice() }
}
