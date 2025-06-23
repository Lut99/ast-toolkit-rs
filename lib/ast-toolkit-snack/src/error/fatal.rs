//  FATAL.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 21:50:58
//  Last edited:
//    08 May 2025, 11:20:52
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`fatal()`]-combinator.
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
/// Defines the fatal error thrown by the [`Fatal`]-combinator.
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'s, S>, bound = (S: Spannable<'s>))]
pub struct Fatal<S> {
    /// The place where the fatal error was thrown.
    pub span: Span<S>,
}
impl<S> Display for Fatal<S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "A fatal error has occurred while parsing") }
}
impl<'s, S: Spannable<'s>> Error for Fatal<S> {}
impl<S: Clone> Spanning<S> for Fatal<S> {
    #[inline]
    fn get_span(&self) -> Option<Cow<Span<S>>> { Some(Cow::Borrowed(&self.span)) }

    #[inline]
    fn take_span(self) -> Option<Span<S>> { Some(self.span) }
}
impl<S: Clone> SpanningInf<S> for Fatal<S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}
impl<S: Clone> SpanningRef<S> for Fatal<S> {
    #[inline]
    fn span_ref(&self) -> &Span<S> { &self.span }
}
impl<S: Clone> SpanningMut<S> for Fatal<S> {
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> { &mut self.span }
}
impl<'s, S: Clone + Spannable<'s>> ParseError<S> for Fatal<S> {
    #[inline]
    fn more_might_fix(&self) -> bool { false }

    #[inline]
    fn needed_to_fix(&self) -> Option<usize> { None }
}





/***** FORMATTERS *****/
/// Expectsformatter for the [`Fatal`]-combinator.
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
/// Actual implementation of the [`fatal()`]-combinator.
pub struct FatalComb<S> {
    _s: PhantomData<S>,
}
impl<'s, S> Combinator<'static, 's, S> for FatalComb<S>
where
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = Infallible;
    type Recoverable = Infallible;
    type Fatal = Fatal<S>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> { Err(SnackError::Fatal(Fatal { span: input })) }
}





/***** LIBRARY *****/
/// Implements a cobminator that always throws a fatal error.
///
/// This may be useful when you're explicitly detecting incorrect inputs (e.g., for linting).
///
/// # Returns
/// A combinator [`Fatal`] that will never succeed.
///
/// # Fails
/// The returned combinator fails always, with a fatal [`Fatal`] error.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::error::fatal;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb = fatal();
/// assert_eq!(comb.parse(span1), Err(SnackError::Fatal(fatal::Fatal { span: span1 })));
/// assert_eq!(comb.parse(span2), Err(SnackError::Fatal(fatal::Fatal { span: span2 })));
/// ```
#[inline]
pub const fn fatal<'s, S>() -> FatalComb<S>
where
    S: Clone + Spannable<'s>,
{
    FatalComb { _s: PhantomData }
}
