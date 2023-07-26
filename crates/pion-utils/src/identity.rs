use std::rc::Rc;

/// Wrapper type for comparison and hashing by pointer address rather than by
/// value.
#[derive(Debug, Copy, Clone)]
pub struct Identity<T>(pub T);

impl<T: HasAddress> PartialEq for Identity<T> {
    fn eq(&self, other: &Self) -> bool { self.address() == other.address() }
}

impl<T: HasAddress> Eq for Identity<T> {}

impl<T: HasAddress> PartialOrd for Identity<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> { Some(self.cmp(other)) }
}

impl<T: HasAddress> Ord for Identity<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.address().cmp(&other.address()) }
}

impl<T: HasAddress> std::hash::Hash for Identity<T> {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) { hasher.write_usize(self.address()) }
}

impl<T: HasAddress> nohash::IsEnabled for Identity<T> {}

/// Trait for values that have an address
pub trait HasAddress {
    fn address(&self) -> usize;
}

impl<'a, T> HasAddress for &'a T {
    fn address(&self) -> usize {
        let r#ref: &T = self;
        let ptr: *const T = r#ref as *const T;
        ptr as usize
    }
}

impl<'a, T> HasAddress for &'a mut T {
    fn address(&self) -> usize {
        let r#ref: &T = self;
        let ptr: *const T = r#ref as *const T;
        ptr as usize
    }
}

impl<T> HasAddress for Box<T> {
    fn address(&self) -> usize {
        let r#ref: &T = self;
        let ptr: *const T = r#ref as *const T;
        ptr as usize
    }
}

impl<T> HasAddress for Rc<T> {
    fn address(&self) -> usize {
        let r#ref: &T = self;
        let ptr: *const T = r#ref as *const T;
        ptr as usize
    }
}

impl<T> HasAddress for std::sync::Arc<T> {
    fn address(&self) -> usize {
        let r#ref: &T = self;
        let ptr: *const T = r#ref as *const T;
        ptr as usize
    }
}

impl<T> HasAddress for triomphe::Arc<T> {
    fn address(&self) -> usize {
        let r#ref: &T = self;
        let ptr: *const T = r#ref as *const T;
        ptr as usize
    }
}
