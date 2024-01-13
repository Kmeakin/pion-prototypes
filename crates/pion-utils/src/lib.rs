#![feature(maybe_uninit_slice)]

pub mod identity;
pub mod location;
pub mod numeric_conversions;
pub mod slice_vec;
pub mod source;
pub mod symbol;

pub fn slice_eq_by_key<T, V: PartialEq>(
    xs: &[T],
    ys: &[T],
    key_fn: impl Copy + Fn(&T) -> V,
) -> bool {
    if xs.len() != ys.len() {
        return false;
    }

    Iterator::eq(xs.iter().map(key_fn), ys.iter().map(key_fn))
}
