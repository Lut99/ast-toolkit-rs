//  SPANNABLE 2.rs
//    by Lut99
//
//  Created:
//    17 Mar 2025, 10:14:05
//  Last edited:
//    08 May 2025, 11:54:28
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a trait abstracting over things that are sensible to put
//!   in a [`Span`].
//

use std::borrow::Cow;
use std::cell::{Ref, RefMut};
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};


/***** HELPER MACROS *****/
/// Does pointer-like impls for [`Spannable`].
macro_rules! spannable_ptr_impl {
    ($({ $lt:lifetime $(+ $bound:ident)?},)? $ty:ty) => {
        impl<$($lt,)? 's, T: ?Sized $($(+ $bound)?)? + Spannable<'s>> Spannable<'s> for $ty {
            type SourceId = <T as Spannable<'s>>::SourceId;
            type Elem = <T as Spannable<'s>>::Elem;


            #[inline]
            fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

            #[inline]
            fn as_slice(&self) -> &'s [Self::Elem] { <T as Spannable>::as_slice(self) }

            #[inline]
            fn len(&self) -> usize { <T as Spannable>::len(self) }

            #[inline]
            fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
        }
    };
}





/***** LIBRARY *****/
/// Defines things that can be [`Span`]ned.
///
/// Remember that this always concerns a array of sorts. For more information, see the
/// [`Span`](crate::span::Span).
///
/// Note that the implementations using spans (and therefore [`Spannable`]) will assume that the
/// span as a whole is cheaply [`Clone`]able. Therefore, the trait is designed to hold a reference
/// to the full source text somewhere else instead of owning it.
///
/// # Lifetime
/// The given lifetime `'s` represents the lifetime of the spanned item. This is used to commute
/// the lifetime of the spanned item over the container referencing it (i.e., the span).
///
/// If your object does not have a lifetime (e.g., an owned object like [`String`]), then this
/// lifetime should be `'static`.
pub trait Spannable<'s> {
    /// Describes the ID returned by [`Spannable::source_id()`].
    type SourceId: Debug + Eq + Hash + PartialEq;
    /// Describes the elements in this spannable array.
    type Elem: 's;


    // Mandatory implementation
    /// Returns some identifier of a source that is used to acertain uniqueness.
    ///
    /// # Returns
    /// A formatter of type [`Spannable::SourceId`] that implements [`Display`].
    fn source_id(&self) -> Self::SourceId;

    /// Returns slice version of the underlying array.
    ///
    /// This function imposes quite some requirements on the underlying memory, namely that it is
    /// a) present and b) continuous. However, it does align with the intended usage of a [`Span`],
    /// and offers for great convenience in interoperating with other libraries (i.e., get a byte
    /// slice of [`str`] as value of a [`Span`]).
    ///
    /// # Returns
    /// A slice of internal elements that should be a counterpart of the spanned area.
    fn as_slice(&self) -> &'s [Self::Elem];


    // Derived functions
    /// Returns the total length of the array.
    ///
    /// By default, acts as a convenience function for:
    /// ```ignore
    /// Spannable::as_slice().len()
    /// ```
    ///
    /// # Returns
    /// The total number of elements in the array.
    #[inline]
    fn len(&self) -> usize { self.as_slice().len() }

    /// Checks whether there is anything in this Spannable.
    ///
    /// By default, acts as a convenience function for:
    /// ```ignore
    /// Spannable::len() == 0
    /// ```
    ///
    /// # Returns
    /// True if no elements are in this array.
    #[inline]
    fn is_empty(&self) -> bool { self.len() == 0 }
}

// Default impls
impl<'a> Spannable<'a> for &'a str {
    type SourceId = usize;
    type Elem = u8;


    #[inline]
    fn source_id(&self) -> Self::SourceId { (*self as *const str).addr() }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { <str>::as_bytes(self) }
}
impl<'a, T> Spannable<'a> for &'a [T] {
    type Elem = T;
    type SourceId = usize;


    #[inline]
    fn source_id(&self) -> Self::SourceId { (*self as *const [T]).addr() }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { self }
}
impl<'a, T: Clone + Debug + Eq + Hash + PartialEq, U: Spannable<'a>> Spannable<'a> for (T, U) {
    type Elem = <U as Spannable<'a>>::Elem;
    type SourceId = T;


    #[inline]
    fn source_id(&self) -> Self::SourceId { self.0.clone() }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { <U as Spannable>::as_slice(&self.1) }

    #[inline]
    fn len(&self) -> usize { self.1.len() }

    #[inline]
    fn is_empty(&self) -> bool { self.1.is_empty() }
}
// The `Spannable` impl for Span is over at its own definition

// Pointer-like impls
spannable_ptr_impl!({'a}, &'a T);
spannable_ptr_impl!({'a}, &'a mut T);
spannable_ptr_impl!({'a + ToOwned}, Cow<'a, T>);
spannable_ptr_impl!(Box<T>);
spannable_ptr_impl!(Rc<T>);
spannable_ptr_impl!(Arc<T>);
spannable_ptr_impl!({'a}, MutexGuard<'a, T>);
spannable_ptr_impl!({'a}, RwLockReadGuard<'a, T>);
spannable_ptr_impl!({'a}, RwLockWriteGuard<'a, T>);
spannable_ptr_impl!({'a}, Ref<'a, T>);
spannable_ptr_impl!({'a}, RefMut<'a, T>);



/// Defines an alias for [`Spannable`]s over [bytes](u8).
///
/// See [`Spannable`]'s docs for more information.
pub trait SpannableBytes<'s>: Spannable<'s, Elem = u8> {
    /// Alias for [`Spannable::as_slice()`] that has a more topical name.
    ///
    /// # Returns
    /// A byte slice representing this array.
    fn as_bytes(&self) -> &'s [u8];



    /// Will attempt to read the bytes as a valid UTF-8 string.
    ///
    /// # Returns
    /// A [`str`]-slice of the byte array if it's valid UTF-8.
    ///
    /// # Errors
    /// This function will return the error if it was not a valid UTF-8 string.
    #[inline]
    fn try_as_str(&self) -> Result<&'s str, std::str::Utf8Error> { std::str::from_utf8(self.as_bytes()) }
}

// Default impl to make it an alias
impl<'s, T: Spannable<'s, Elem = u8>> SpannableBytes<'s> for T {
    #[inline]
    fn as_bytes(&self) -> &'s [u8] { self.as_slice() }
}
