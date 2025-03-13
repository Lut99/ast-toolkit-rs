//  RECOVERABLE.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:01:07
//  Last edited:
//    18 Jan 2025, 17:55:30
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`recoverable()`]-combinator.
//

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning};
use better_derive::{Debug, Eq, PartialEq};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ExpectsFormatter as _};


/***** ERRORS *****/
/// Defines the recoverable error thrown by the [`Recoverable`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct Recoverable<F, S> {
    /// The place where the recoverable error was thrown.
    pub span: Span<F, S>,
}
impl<F, S> Display for Recoverable<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "A recoverable error has occurred while parsing") }
}
impl<F, S> Error for Recoverable<F, S> {}
impl<F: Clone, S: Clone> Spanning<F, S> for Recoverable<F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.span.clone() }

    #[inline]
    fn into_span(self) -> Span<F, S>
    where
        Self: Sized,
    {
        self.span
    }
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
pub struct RecoverableComb<F, S> {
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<F, S> Combinator<'static, F, S> for RecoverableComb<F, S> {
    type ExpectsFormatter = ExpectsFormatter;
    type Output = Infallible;
    type Recoverable = Recoverable<F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
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
/// use ast_toolkit_snack::error::recoverable::Recoverable;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = recoverable();
/// assert_eq!(comb.parse(span1), Err(SnackError::Recoverable(Recoverable { span: span1 })));
/// assert_eq!(comb.parse(span2), Err(SnackError::Recoverable(Recoverable { span: span2 })));
/// ```
#[inline]
pub const fn recoverable<F, S>() -> RecoverableComb<F, S> { RecoverableComb { _f: PhantomData, _s: PhantomData } }
