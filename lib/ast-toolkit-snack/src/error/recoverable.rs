//  RECOVERABLE.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:01:07
//  Last edited:
//    08 May 2025, 11:20:52
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`recoverable()`]-combinator.
//

use std::borrow::Cow;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, Spanning, SpanningInf, SpanningMut, SpanningRef};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ExpectsFormatter as _, ParseError};


/***** ERRORS *****/
/// Defines the recoverable error thrown by the [`Recoverable`]-combinator.
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'s, S>, bound = (S: Spannable<'s>))]
pub struct Recoverable<S> {
    /// The place where the recoverable error was thrown.
    pub span: Span<S>,
}
impl<S> Display for Recoverable<S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "A recoverable error has occurred while parsing") }
}
impl<'s, S: Spannable<'s>> Error for Recoverable<S> {}
impl<S: Clone> Spanning<S> for Recoverable<S> {
    #[inline]
    fn get_span(&self) -> Option<Cow<'_, Span<S>>> { Some(Cow::Borrowed(&self.span)) }

    #[inline]
    fn take_span(self) -> Option<Span<S>> { Some(self.span) }
}
impl<S: Clone> SpanningInf<S> for Recoverable<S> {
    #[inline]
    fn span(&self) -> Cow<'_, Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}
impl<S: Clone> SpanningRef<S> for Recoverable<S> {
    #[inline]
    fn span_ref(&self) -> &Span<S> { &self.span }
}
impl<S: Clone> SpanningMut<S> for Recoverable<S> {
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> { &mut self.span }
}
impl<'s, S: Clone + Spannable<'s>> ParseError<S> for Recoverable<S> {
    #[inline]
    fn more_might_fix(&self) -> bool { false }

    #[inline]
    fn needed_to_fix(&self) -> Option<usize> { None }
}





/***** FORMATTERS *****/
/// Expectsformatter for the [`Recoverable`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter;
impl Display for ExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl crate::ExpectsFormatter for ExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "something impossible") }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`recoverable()`]-combinator.
pub struct RecoverableComb<S> {
    _s: PhantomData<S>,
}
impl<'s, S> Combinator<'static, 's, S> for RecoverableComb<S>
where
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = Infallible;
    type Recoverable = Recoverable<S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        Err(SnackError::Recoverable(Recoverable { span: input }))
    }
}





/***** LIBRARY *****/
/// Implements a cobminator that always throws a recoverable error.
///
/// This may be useful when you're explicitly detecting incorrect inputs (e.g., for linting).
///
/// # Returns
/// A combinator [`Recoverable`] that will never succeed.
///
/// # Fails
/// The returned combinator fails always, with a recoverable [`Recoverable`] error.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::error::recoverable;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb = recoverable();
/// assert_eq!(
///     comb.parse(span1),
///     Err(SnackError::Recoverable(recoverable::Recoverable { span: span1 }))
/// );
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(recoverable::Recoverable { span: span2 }))
/// );
/// ```
#[inline]
pub const fn recoverable<'s, S>() -> RecoverableComb<S>
where
    S: Clone + Spannable<'s>,
{
    RecoverableComb { _s: PhantomData }
}
