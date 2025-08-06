//  SPEC.rs
//    by Lut99
//
//  Description:
//!   Defines the main interface of the crate.
//

use std::borrow::Cow;
use std::cell::{Ref, RefMut};
use std::fmt::{Result as FResult, Write};
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};

use crate::display::Display;
use crate::formatter::Formatter;


/***** HELPER MACROS *****/
/// Implements [`Displayable`] for pointer-like types.
macro_rules! display_fmt_ptr_impl {
    ('a,Cow < 'a,T >) => {
        impl<'a, T: ?Sized + DisplayFmt + ToOwned> DisplayFmt for Cow<'a, T> {
            #[inline]
            fn display_fmt<W: Write>(&self, f: &mut Formatter<'_, W>) -> FResult { <T as DisplayFmt>::display_fmt(self, f) }
        }
    };

    ('a, $target:ty) => {
        impl<'a, T: ?Sized + DisplayFmt> DisplayFmt for $target {
            #[inline]
            fn display_fmt<W: Write>(&self, f: &mut Formatter<'_, W>) -> FResult { <T as DisplayFmt>::display_fmt(self, f) }
        }
    };

    ($target:ty) => {
        impl<T: ?Sized + DisplayFmt> DisplayFmt for $target {
            #[inline]
            fn display_fmt<W: Write>(&self, f: &mut Formatter<'_, W>) -> FResult { <T as DisplayFmt>::display_fmt(self, f) }
        }
    };
}





/***** LIBRARY *****/
/// Marks that [`display()`](Displayable::display()) can be called on a type implementing
/// [`DisplayFmt`].
pub trait Displayable {
    /// Returns a formatter for serializing the output representation as something understood by
    /// humans.
    ///
    /// # Arguments
    /// - `color`: Whether to use ANSI-colors while serializing. Use [`None`] to automatically
    ///   decide based on whether stdout is a TTY.
    ///
    /// # Returns
    /// A [`Display`] that implements [`std::fmt::Display`].
    fn display(&self, color: Option<bool>) -> Display<'_, Self>;
}

// Blanket impl for everything [`DisplayFmt`].
impl<T: ?Sized + DisplayFmt> Displayable for T {
    #[inline]
    fn display(&self, color: Option<bool>) -> Display<'_, Self> { Display(self, color) }
}



/// Implements how a type should be [`Display`]ed.
pub trait DisplayFmt {
    /// Serializes this type such that humans can understand it.
    ///
    /// # Arguments
    /// - `f`: Some [`Formatter`] to [`Write`] to.
    ///
    ///   Note, however, that it's a custom struct; it embeds information about coloring and
    ///   indentation.
    ///
    /// # Errors
    /// This function can error if it failed to serialize to the given `f`ormatter.
    fn display_fmt<W: Write>(&self, f: &mut Formatter<'_, W>) -> FResult;
}

// Pointer-like impls
display_fmt_ptr_impl!('a, &'a T);
display_fmt_ptr_impl!('a, &'a mut T);
display_fmt_ptr_impl!('a, Cow<'a, T>);
display_fmt_ptr_impl!(Box<T>);
display_fmt_ptr_impl!(Rc<T>);
display_fmt_ptr_impl!(Arc<T>);
display_fmt_ptr_impl!('a, Ref<'a, T>);
display_fmt_ptr_impl!('a, RefMut<'a, T>);
display_fmt_ptr_impl!('a, MutexGuard<'a, T>);
display_fmt_ptr_impl!('a, RwLockReadGuard<'a, T>);
display_fmt_ptr_impl!('a, RwLockWriteGuard<'a, T>);
