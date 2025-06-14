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


/***** AUXILLARY *****/
/// Defines a wrapper for [`ElemDisplay`]-types that will allow one to access
/// [`ElemDisplay::fmt()`] through either [`Debug`] or [`Display`].
pub struct ElemDisplayFormatter<'e, T: ?Sized>(pub &'e T);
impl<'e, T: ?Sized + ElemDisplay> Debug for ElemDisplayFormatter<'e, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { self.0.fmt(f) }
}
impl<'e, T: ?Sized + ElemDisplay> Display for ElemDisplayFormatter<'e, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { self.0.fmt(f) }
}



/// Defines a wrapper for [`ElemDisplay`]-types that will allow one to access
/// [`ElemDisplay::fmt_iter()`] through either [`Debug`] or [`Display`].
pub struct ElemDisplayIterFormatter<I>(pub I);
impl<'e, I: Clone + IntoIterator<Item = &'e T>, T: 'e + ?Sized + ElemDisplay> Debug for ElemDisplayIterFormatter<I> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { T::fmt_iter(self.0.clone(), f) }
}
impl<'e, I: Clone + IntoIterator<Item = &'e T>, T: 'e + ?Sized + ElemDisplay> Display for ElemDisplayIterFormatter<I> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { T::fmt_iter(self.0.clone(), f) }
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

// Default impls for pointer-like types
impl<'a, T: ?Sized + ExpectsFormatter> ExpectsFormatter for &'a T {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult { (**self).expects_fmt(f, indent) }
}
impl<T: ?Sized + ExpectsFormatter> ExpectsFormatter for Box<T> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult { (**self).expects_fmt(f, indent) }
}

// Default impls for string-like types
impl ExpectsFormatter for str {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        // If it begins with `Expected`, cut that off
        if self.starts_with("Expected ") { <str as Display>::fmt(&self[9..], f) } else { <str as Display>::fmt(self, f) }
    }
}
impl<'a> ExpectsFormatter for Cow<'a, str> {
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



/// A trait for elements of a `S`ource text to be formatted nicely.
pub trait ElemDisplay {
    /// Formats this element.
    ///
    /// This should be _like_ [`Display`], in that humans should comfortably understand it, but
    /// also like [`Debug`] in that every possible element type should be supported.
    ///
    /// While it has a default implementation, also check [`ElemDisplay::fmt_iter()`] for
    /// formatting multiple elements in sequence.
    ///
    /// # Arguments
    /// - `f`: Some [`Formatter`] to write the serialization of this element to.
    ///
    /// # Errors
    /// This function should only error if it failed to write to `f`.
    fn fmt(&self, f: &mut Formatter) -> FResult;

    /// Formats a sequence of this element.
    ///
    /// By default, this will simply call [`ElemDisplay::fmt()`] for every element in the
    /// sequence rendered as a slice (e.g., `[a, b, c]`). However, depending on what you're
    /// formalizing, this could be much nicer.
    ///
    /// # Arguments
    /// - `f`: Some [`Formatter`] to write the serialization of this element to.
    ///
    /// # Errors
    /// This function should only error if it failed to write to `f`.
    #[inline]
    fn fmt_iter<'s>(elems: impl IntoIterator<Item = &'s Self>, f: &mut Formatter) -> FResult
    where
        Self: 's,
    {
        // Serialize using `Formatter`'s `DebugList`
        let mut fmt = f.debug_list();
        fmt.entries(elems.into_iter().map(ElemDisplayFormatter));
        fmt.finish()
    }
}

// Default impls for default types
impl ElemDisplay for u8 {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { write!(f, "{self:02X}") }

    #[inline]
    fn fmt_iter<'s>(elems: impl IntoIterator<Item = &'s Self>, f: &mut Formatter) -> FResult
    where
        Self: 's,
    {
        for (i, elem) in elems.into_iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            <Self as ElemDisplay>::fmt(elem, f)?;
        }
        Ok(())
    }
}

// Pointer-like types
impl<'a, T: ElemDisplay> ElemDisplay for &'a T {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { <T as ElemDisplay>::fmt(self, f) }

    #[inline]
    fn fmt_iter<'s>(elems: impl IntoIterator<Item = &'s Self>, f: &mut Formatter) -> FResult
    where
        Self: 's,
    {
        <T as ElemDisplay>::fmt_iter(elems.into_iter().map(|elem| *elem), f)
    }
}
impl<'a, T: ElemDisplay> ElemDisplay for &'a mut T {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { <T as ElemDisplay>::fmt(self, f) }

    #[inline]
    fn fmt_iter<'s>(elems: impl IntoIterator<Item = &'s Self>, f: &mut Formatter) -> FResult
    where
        Self: 's,
    {
        <T as ElemDisplay>::fmt_iter(elems.into_iter().map(|elem| &**elem), f)
    }
}
impl<T: ElemDisplay> ElemDisplay for Box<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { <T as ElemDisplay>::fmt(self, f) }

    #[inline]
    fn fmt_iter<'s>(elems: impl IntoIterator<Item = &'s Self>, f: &mut Formatter) -> FResult
    where
        Self: 's,
    {
        <T as ElemDisplay>::fmt_iter(elems.into_iter().map(|elem| &**elem), f)
    }
}
impl<'a, T: Clone + ElemDisplay> ElemDisplay for Cow<'a, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { <T as ElemDisplay>::fmt(self, f) }

    #[inline]
    fn fmt_iter<'s>(elems: impl IntoIterator<Item = &'s Self>, f: &mut Formatter) -> FResult
    where
        Self: 's,
    {
        <T as ElemDisplay>::fmt_iter(elems.into_iter().map(|elem| elem.as_ref()), f)
    }
}
impl<T: ElemDisplay> ElemDisplay for Rc<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { <T as ElemDisplay>::fmt(self, f) }

    #[inline]
    fn fmt_iter<'s>(elems: impl IntoIterator<Item = &'s Self>, f: &mut Formatter) -> FResult
    where
        Self: 's,
    {
        <T as ElemDisplay>::fmt_iter(elems.into_iter().map(|elem| &**elem), f)
    }
}
impl<T: ElemDisplay> ElemDisplay for Arc<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { <T as ElemDisplay>::fmt(self, f) }

    #[inline]
    fn fmt_iter<'s>(elems: impl IntoIterator<Item = &'s Self>, f: &mut Formatter) -> FResult
    where
        Self: 's,
    {
        <T as ElemDisplay>::fmt_iter(elems.into_iter().map(|elem| &**elem), f)
    }
}
impl<'a, T: ElemDisplay> ElemDisplay for Ref<'a, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { <T as ElemDisplay>::fmt(self, f) }

    #[inline]
    fn fmt_iter<'s>(elems: impl IntoIterator<Item = &'s Self>, f: &mut Formatter) -> FResult
    where
        Self: 's,
    {
        <T as ElemDisplay>::fmt_iter(elems.into_iter().map(|elem| &**elem), f)
    }
}
impl<'a, T: ElemDisplay> ElemDisplay for RefMut<'a, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { <T as ElemDisplay>::fmt(self, f) }

    #[inline]
    fn fmt_iter<'s>(elems: impl IntoIterator<Item = &'s Self>, f: &mut Formatter) -> FResult
    where
        Self: 's,
    {
        <T as ElemDisplay>::fmt_iter(elems.into_iter().map(|elem| &**elem), f)
    }
}
impl<'a, T: ElemDisplay> ElemDisplay for MutexGuard<'a, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { <T as ElemDisplay>::fmt(self, f) }

    #[inline]
    fn fmt_iter<'s>(elems: impl IntoIterator<Item = &'s Self>, f: &mut Formatter) -> FResult
    where
        Self: 's,
    {
        <T as ElemDisplay>::fmt_iter(elems.into_iter().map(|elem| &**elem), f)
    }
}
impl<'a, T: ElemDisplay> ElemDisplay for RwLockReadGuard<'a, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { <T as ElemDisplay>::fmt(self, f) }

    #[inline]
    fn fmt_iter<'s>(elems: impl IntoIterator<Item = &'s Self>, f: &mut Formatter) -> FResult
    where
        Self: 's,
    {
        <T as ElemDisplay>::fmt_iter(elems.into_iter().map(|elem| &**elem), f)
    }
}
impl<'a, T: ElemDisplay> ElemDisplay for RwLockWriteGuard<'a, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { <T as ElemDisplay>::fmt(self, f) }

    #[inline]
    fn fmt_iter<'s>(elems: impl IntoIterator<Item = &'s Self>, f: &mut Formatter) -> FResult
    where
        Self: 's,
    {
        <T as ElemDisplay>::fmt_iter(elems.into_iter().map(|elem| &**elem), f)
    }
}
