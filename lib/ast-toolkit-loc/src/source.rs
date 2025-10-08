//  SOURCE.rs
//    by Lut99
//
//  Description:
//!   Abstract representation of a source text.
//!
//!   You can think of a source text as \*any\* array of homogenous elements.
//!   Usually, this is bytes, or even better, UTF-8 graphemes.
//

use std::borrow::Cow;
use std::cell::{Ref, RefMut};
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};


/***** HELPER MACROS *****/
macro_rules! source_ptr_impl {
    ('a,Cow < 'a, $type:ty >) => {
        impl<'a, T: Source + ToOwned> Source for Cow<'a, $type> {
            #[inline]
            fn len(&self) -> u64 { <T as Source>::len(self) }
        }
    };
    ('a, $type:ty) => {
        impl<'a, T: Source> Source for $type {
            #[inline]
            fn len(&self) -> u64 { <T as Source>::len(self) }
        }
    };
    ($type:ty) => {
        impl<T: Source> Source for $type {
            #[inline]
            fn len(&self) -> u64 { <T as Source>::len(self) }
        }
    };
}





/***** LIBRARY *****/
/// Abstract representation of a [`Loc`](crate::Loc)atable source text.
pub trait Source {
    /// Returns the number of elements in this Source text.
    ///
    /// # Returns
    /// A [`u64`] encoding the number of elements.
    fn len(&self) -> u64;

    /// Alias for checking if [`Source::len()`] equals `0`.
    ///
    /// # Returns
    /// True if `Source::len() == 0`, false otherwise.
    #[inline]
    fn is_empty(&self) -> bool { Source::len(self) == 0 }
}

// Std impls
impl Source for () {
    #[inline]
    fn len(&self) -> u64 { 0 }
}
impl<T> Source for Option<T> {
    #[inline]
    fn len(&self) -> u64 { if self.is_some() { 1 } else { 0 } }
}
impl<T> Source for [T] {
    #[inline]
    fn len(&self) -> u64 { <[T]>::len(self) as u64 }
}
impl<T> Source for Vec<T> {
    #[inline]
    fn len(&self) -> u64 { <Vec<T>>::len(self) as u64 }
}
impl Source for str {
    #[inline]
    fn len(&self) -> u64 { <str>::len(self) as u64 }
}
impl Source for String {
    #[inline]
    fn len(&self) -> u64 { <String>::len(self) as u64 }
}

// Pointer-like impls
source_ptr_impl!('a, &'a T);
source_ptr_impl!('a, &'a mut T);
source_ptr_impl!('a, Ref<'a, T>);
source_ptr_impl!('a, RefMut<'a, T>);
source_ptr_impl!('a, Cow<'a, T>);
source_ptr_impl!(Box<T>);
source_ptr_impl!(Rc<T>);
source_ptr_impl!(Arc<T>);
source_ptr_impl!('a, MutexGuard<'a, T>);
source_ptr_impl!('a, RwLockReadGuard<'a, T>);
source_ptr_impl!('a, RwLockWriteGuard<'a, T>);
