//  FMT.rs
//    by Lut99
//
//  Description:
//!   Implements some traits for formatting things in the parsers.
//

use std::borrow::Cow;
use std::cell::{Ref, RefMut};
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};


/***** HELPER MACROS *****/
macro_rules! expects_formatter_ptr_impl {
    ($({$lt:lifetime $(+ $bound:ident)? $(+ T::Owned: $bound2:ident + $bound3:ident)?},)? $ty:ty) => {
        impl<$($lt,)? 's, T: ?Sized + $($($bound +)?)? ExpectsFormatter> ExpectsFormatter for $ty $($(where <T as ToOwned>::Owned: $bound2 + $bound3)?)? {
            #[inline]
            fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult { <T as ExpectsFormatter>::expects_fmt(self, f, indent) }
        }
    };
}

macro_rules! spannable_display_ptr_impl {
    ($({$lt:lifetime $(+ $bound:ident)?},)? $ty:ty) => {
        impl<$($lt,)? 's, T: ?Sized + $($($bound +)?)? ElemDisplay> ElemDisplay for $ty {
            #[inline]
            fn elem_fmt(&self, f: &mut Formatter) -> FResult { <T as ElemDisplay>::elem_fmt(self, f) }
        }
    };
}





/***** AUXILLARY *****/
/// Defines a wrapper for [`ElemDisplay`]-types that will allow one to access
/// [`ElemDisplay::elem_fmt()`] through either [`Debug`] or [`Display`].
pub struct ElemDisplayFormatter<'e, T: ?Sized>(pub &'e T);
impl<'e, T: ?Sized + ElemDisplay> Debug for ElemDisplayFormatter<'e, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { T::elem_fmt(self.0, f) }
}
impl<'e, T: ?Sized + ElemDisplay> Display for ElemDisplayFormatter<'e, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { T::elem_fmt(self.0, f) }
}





/***** LIBRARY *****/
/// A trait implemented by [`Combinator::ExpectsFormatter`](super::Combinator::ExpectsFormatter)s.
///
/// This trait actually produces expect-strings.
pub trait ExpectsFormatter: Debug + Display {
    /// Formats the thing that this Expects expected as input.
    ///
    /// The string written should be something along the lines of filling in `XXX` in:
    /// ```plain
    /// Expected XXX.
    /// ```
    ///
    /// # Arguments
    /// - `f`: Some [`Formatter`] to write to.
    /// - `indent`: If this formatter writes newlines, they should be indented by this amount.
    ///
    /// # Errors
    /// This function should only error if it failed to write to the given `f`ormatter.
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult;
}

// Default impls for string-like types
impl ExpectsFormatter for str {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        // If it begins with `Expected`, cut that off
        if self.starts_with("Expected ") { <str as Display>::fmt(&self[9..], f) } else { <str as Display>::fmt(self, f) }
    }
}
impl ExpectsFormatter for String {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        // If it begins with `Expected`, cut that off
        if self.starts_with("Expected ") { <str as Display>::fmt(&self[9..], f) } else { <str as Display>::fmt(self, f) }
    }
}

// Default impls for pointer-like types
expects_formatter_ptr_impl!({'a}, &'a T);
expects_formatter_ptr_impl!({'a}, &'a mut T);
expects_formatter_ptr_impl!({'a + ToOwned + T::Owned: Debug + Display}, Cow<'a, T>);
expects_formatter_ptr_impl!(Box<T>);
expects_formatter_ptr_impl!(Rc<T>);
expects_formatter_ptr_impl!(Arc<T>);
expects_formatter_ptr_impl!({'a}, Ref<'a, T>);
expects_formatter_ptr_impl!({'a}, RefMut<'a, T>);
expects_formatter_ptr_impl!({'a}, MutexGuard<'a, T>);
expects_formatter_ptr_impl!({'a}, RwLockReadGuard<'a, T>);
expects_formatter_ptr_impl!({'a}, RwLockWriteGuard<'a, T>);



/// A trait for displaying spans of a `S`ource text nicely.
///
/// This is mostly used for element-agnostic combinators that are looking for specific elements
/// (e.g., those in [`scan`](crate::scan)).
pub trait ElemDisplay {
    /// Formats a specific element in a spanned area.
    ///
    /// This should be _like_ [`Display`], in that humans should comfortably understand it, but
    /// also like [`Debug`] in that every possible element type should be supported.
    ///
    /// # Arguments
    /// - `elem`: The [`SliceDisplay::Elem`]ent to display.
    /// - `f`: Some [`Formatter`] to write the serialization of this element to.
    ///
    /// # Errors
    /// This function should only error if it failed to write to `f`.
    fn elem_fmt(&self, f: &mut Formatter) -> FResult;
}

// Default impls for default types
impl ElemDisplay for u8 {
    #[inline]
    fn elem_fmt(&self, f: &mut Formatter) -> FResult { write!(f, "{self:02X}") }
}

// Pointer-like types
spannable_display_ptr_impl!({'a}, &'a T);
spannable_display_ptr_impl!({'a}, &'a mut T);
spannable_display_ptr_impl!({'a + Clone}, Cow<'a, T>);
spannable_display_ptr_impl!(Box<T>);
spannable_display_ptr_impl!(Rc<T>);
spannable_display_ptr_impl!(Arc<T>);
spannable_display_ptr_impl!({'a}, Ref<'a, T>);
spannable_display_ptr_impl!({'a}, RefMut<'a, T>);
spannable_display_ptr_impl!({'a}, MutexGuard<'a, T>);
spannable_display_ptr_impl!({'a}, RwLockReadGuard<'a, T>);
spannable_display_ptr_impl!({'a}, RwLockWriteGuard<'a, T>);
