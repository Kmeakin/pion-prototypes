// TODO: investigate if this could be replaced with an existing crate. For
// example:
//
// - https://lib.rs/crates/slicevec
// - https://lib.rs/crates/fixed-slice-vec
//
// Our requirements around drop-glue might be an issue.

use std::mem::MaybeUninit;
use std::ops::Deref;

/// A helpful type for allocating elements to a slice up to a maximum length.
/// This can be helpful if we have initialization code that might be difficult
/// to implement using [`bumpalo::Bump::alloc_slice_fill_iter`], for example:
///
/// - when pushing to multiple slices at once
/// - when element initialization code has the possibility of failure
pub struct SliceVec<'alloc, T> {
    len: usize,
    // SAFETY: The slice `self.elems[..self.next_index]` should only ever
    //         contain elements initialized with `MaybeUninit::new`.
    elems: &'alloc mut [MaybeUninit<T>],
}

impl<'alloc, Elem> SliceVec<'alloc, Elem> {
    /// Allocates a new slice builder to the arena.
    ///
    /// # Panics
    ///
    /// If the type has drop-glue to be executed.
    pub fn new(bump: &'alloc bumpalo::Bump, capacity: usize) -> SliceVec<'alloc, Elem> {
        // NOTE: Ensure that that the element type does not have any drop glue.
        //       This would be problematic as we have no way of registering the
        //       drop glue of `Elem` with `bumpalo::Bump`.
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

    /// Push an element to the slice builder.
    ///
    /// # Panics
    ///
    /// If the pushing the element would exceed the maximum slice length
    /// supplied in [`SliceVec::new`].
    pub fn push(&mut self, elem: Elem) {
        assert!(
            !self.is_full(),
            "Cannot push onto a full `SliceVec` (capacity is {})",
            self.capacity()
        );
        self.elems[self.len] = MaybeUninit::new(elem);
        self.len += 1;
    }
}

impl<'a, Elem> Deref for SliceVec<'a, Elem> {
    type Target = [Elem];

    fn deref(&self) -> &[Elem] {
        // SAFETY: This is safe because we know that `self.elems[..self.next_index]`
        // only ever contains elements initialized with `MaybeUninit::new`.
        // We know this because:
        //
        // - `self.next_index` is always initialized to `0` in `SliceBuilder::new`
        // - `self.next_index` is only incremented in `SliceBuilder::push`, and in that
        //   case we make sure `self.elems[self.next_index]` has been initialized before
        //   hand.
        unsafe { slice_assume_init_ref(&self.elems[..self.len]) }
    }
}

impl<'a, Elem> From<SliceVec<'a, Elem>> for &'a [Elem] {
    fn from(slice: SliceVec<'a, Elem>) -> &'a [Elem] {
        // SAFETY: This is safe because we know that `self.elems[..self.next_index]`
        // only ever contains elements initialized with `MaybeUninit::new`.
        // We know this because:
        //
        // - `self.next_index` is always initialized to `0` in `SliceBuilder::new`
        // - `self.next_index` is only incremented in `SliceBuilder::push`, and in that
        //   case we make sure `self.elems[self.next_index]` has been initialized before
        //   hand.
        unsafe { slice_assume_init_ref(&slice.elems[..slice.len]) }
    }
}

// NOTE: This is the same implementation as
// `MaybeUninit::slice_assume_init_ref`, which is currently unstable (see https://github.com/rust-lang/rust/issues/63569).
/// Assuming all the elements are initialized, get a slice to them.
///
/// # Safety
///
/// It is up to the caller to guarantee that the `MaybeUninit<T>` elements
/// really are in an initialized state.
/// Calling this when the content is not yet fully initialized causes undefined
/// behavior.
///
/// See [`assume_init_ref`] for more details and examples.
///
/// [`assume_init_ref`]: MaybeUninit::assume_init_ref
pub unsafe fn slice_assume_init_ref<T>(slice: &[MaybeUninit<T>]) -> &[T] {
    // SAFETY: casting slice to a `*const [T]` is safe since the caller guarantees
    // that `slice` is initialized, and`MaybeUninit` is guaranteed to have the
    // same layout as `T`. The pointer obtained is valid since it refers to
    // memory owned by `slice` which is a reference and thus guaranteed to be
    // valid for reads.
    &*(slice as *const [MaybeUninit<T>] as *const [T])
}
