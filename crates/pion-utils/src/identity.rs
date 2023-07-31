use std::fmt::{self, Debug};
use std::ops::Deref;
use std::rc::Rc;

/// Wrapper type for comparison and hashing by pointer address rather than by
/// value.
#[derive(Copy, Clone)]
pub struct Identity<T>(pub T);

impl<T: Deref> Identity<T> {
    pub fn address(&self) -> *const T::Target { &*self.0 }
}

impl<T: Deref + Debug> Debug for Identity<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Identity")
            .field(&(self.address()))
            .field(&self.0)
            .finish()
    }
}

impl<T: Deref> PartialEq for Identity<T> {
    fn eq(&self, other: &Self) -> bool { self.address() == other.address() }
}

impl<T: Deref> Eq for Identity<T> {}

impl<T: Deref> PartialOrd for Identity<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> { Some(self.cmp(other)) }
}

impl<T: Deref> Ord for Identity<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.address().cmp(&other.address()) }
}

impl<T: Deref> std::hash::Hash for Identity<T> {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) { self.address().hash(hasher) }
}

impl<T: Deref> nohash::IsEnabled for Identity<T> {}

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;
    use std::collections::hash_map::DefaultHasher;
    use std::collections::HashMap;
    use std::hash::{Hash, Hasher};

    use super::*;

    #[test]
    fn eq() {
        let one: &u32 = Box::leak(Box::new(1));
        let also_one: &u32 = Box::leak(Box::new(1));

        assert_eq!(Identity(one), Identity(one));
        assert_ne!(Identity(one), Identity(also_one));
    }

    #[test]
    fn ord() {
        let one: &u32 = Box::leak(Box::new(1));
        let also_one: &u32 = Box::leak(Box::new(1));

        assert_eq!(Identity(one).cmp(&Identity(one)), Ordering::Equal);
        assert_ne!(Identity(one).cmp(&Identity(also_one)), Ordering::Equal);
    }

    #[test]
    fn hash() {
        fn hash<T: Hash>(value: T) -> u64 {
            let mut hasher = DefaultHasher::new();
            value.hash(&mut hasher);
            hasher.finish()
        }

        let one: &u32 = Box::leak(Box::new(1));
        let also_one: &u32 = Box::leak(Box::new(1));

        assert_eq!(hash(Identity(one)), hash(Identity(one)));
        assert_ne!(hash(Identity(one)), hash(Identity(also_one)));
    }

    #[test]
    fn hashmap() {
        let one: &u32 = Box::leak(Box::new(1));
        let also_one: &u32 = Box::leak(Box::new(1));

        let two: &u32 = Box::leak(Box::new(2));
        let also_two: &u32 = Box::leak(Box::new(2));

        let mut hashmap = HashMap::new();

        hashmap.insert(Identity(one), "one");
        hashmap.insert(Identity(two), "two");

        assert_eq!(hashmap.get(&Identity(one)), Some("one").as_ref());
        assert_eq!(hashmap.get(&Identity(two)), Some("two").as_ref());

        assert_eq!(hashmap.get(&Identity(also_one)), None);
        assert_eq!(hashmap.get(&Identity(also_two)), None);
    }
}

#[test]
fn fuck() {
    let one = &1;
    let addr = one as *const i32 as usize;
    let id = Identity(one);

    assert_eq!(addr, addr);
    assert_eq!(id, id);
}
