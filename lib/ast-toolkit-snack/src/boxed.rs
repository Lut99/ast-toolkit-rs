//  BOXED.rs
//    by Lut99
//
//  Created:
//    08 May 2025, 16:30:56
//  Last edited:
//    09 May 2025, 10:08:51
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements [`Box`]ed versions of various traits.
//

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{self, Display, Formatter, Result as FResult};

use ast_toolkit_span::{Span, Spannable, Spanning};

use crate::combinator::forget_ty;
use crate::{Combinator, ExpectsFormatter, ParseError};


/***** BOXED TYPES *****/
/// Implements a [`ParseError`] that is [`Box`]ed.
///
/// This is a separate type because [`Box`] does not implement [`Error`] unless the wrapped type is
/// [`Sized`] (see <https://users.rust-lang.org/t/why-box-dyn-error-is-not-sized/61642/4>).
///
/// We fix it by defining this custom type ourselves which _does_ implement [`Error`] and,
/// therefore, [`ParseError`].
pub struct BoxedParseError<'e, S: Clone>(pub Box<dyn 'e + ParseError<S>>);
impl<'e, S: Clone> BoxedParseError<'e, S> {
    #[inline]
    pub fn new(err: impl 'e + ParseError<S>) -> Self { Self(Box::new(err) as Box<dyn 'e + ParseError<S>>) }
}
impl<'e, S: Clone> fmt::Debug for BoxedParseError<'e, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let Self(err) = self;
        let mut fmt = f.debug_tuple("BoxedParseError");
        fmt.field(err);
        fmt.finish()
    }
}
impl<'e, S: Clone> Display for BoxedParseError<'e, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { <Box<dyn ParseError<S>> as Display>::fmt(&self.0, f) }
}
impl<'e, S: Clone> Error for BoxedParseError<'e, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> { self.0.source() }
}
impl<'e, S: Clone> Spanning<S> for BoxedParseError<'e, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { self.0.span() }
    #[inline]
    fn into_span(self) -> Span<S> { self.0.into_span() }
}



/// Alias for a [`Box`]ed [`Combinator`].
pub type BoxedCombinator<'a, 'f, 'e1, 'e2, 'c, 's, O, S> = Box<
    dyn 'a
        + Combinator<
            'c,
            's,
            S,
            Output = O,
            ExpectsFormatter = Box<dyn 'f + ExpectsFormatter>,
            Recoverable = BoxedParseError<'e1, S>,
            Fatal = BoxedParseError<'e2, S>,
        >,
>;





/***** BOXING INTERFACES *****/
/// Provides [`BoxableParseError::boxed()`] on [`ParseError`]s.
pub trait BoxableParseError<S: Clone>: ParseError<S> {
    /// Turns any [`ParseError`] into a [`BoxedParseError`].
    ///
    /// # Returns
    /// A [`BoxedParseError`] that can be used in e.g. recursion.
    fn boxed<'e>(self) -> BoxedParseError<'e, S>
    where
        Self: 'e;
}

// Blanket impl for [`BoxableParseError`]
impl<S: Clone, T: ParseError<S>> BoxableParseError<S> for T {
    #[inline]
    fn boxed<'e>(self) -> BoxedParseError<'e, S>
    where
        T: 'e,
    {
        BoxedParseError::new(self)
    }
}



/// Provides [`BoxableCombinator::boxed()`] on [`Combinator`]s.
pub trait BoxableCombinator<'c, 's, S>: Combinator<'c, 's, S>
where
    S: Clone + Spannable<'s>,
{
    /// Turns any [`Combinator`] into a [`Box`]ed one.
    ///
    /// # Returns
    /// A [`Box<dyn Combinator>`] that can be used in e.g. recursion.
    fn boxed<'a, 'f, 'e1, 'e2>(self) -> BoxedCombinator<'a, 'f, 'e1, 'e2, 'c, 's, Self::Output, S>
    where
        'f: 'a,
        'e1: 'a,
        'e2: 'a,
        Self: 'a,
        Self::ExpectsFormatter: 'f,
        Self::Recoverable: 'e1,
        Self::Fatal: 'e2,
        S: 'a;
}

// Blanket impl for [`BoxableCombinator`]
impl<'c, 's, C, S> BoxableCombinator<'c, 's, S> for C
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    #[inline]
    fn boxed<'a, 'f, 'e1, 'e2>(self) -> BoxedCombinator<'a, 'f, 'e1, 'e2, 'c, 's, Self::Output, S>
    where
        'f: 'a,
        'e1: 'a,
        'e2: 'a,
        C: 'a,
        C::ExpectsFormatter: 'f,
        C::Recoverable: 'e1,
        C::Fatal: 'e2,
        S: 'a,
    {
        Box::new(forget_ty(self))
    }
}
