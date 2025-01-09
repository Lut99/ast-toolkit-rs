//  RECOVERABLE.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:01:07
//  Last edited:
//    09 Jan 2025, 20:34:39
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
use crate::{Combinator2, ExpectsFormatter};


/***** ERRORS *****/
/// Defines the recoverable error thrown by the [`Recoverable`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct RecoverableRecoverable<F, S> {
    /// The place where the recoverable error was thrown.
    pub span: Span<F, S>,
}
impl<F, S> Display for RecoverableRecoverable<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "A recoverable error has occurred while parsing") }
}
impl<F, S> Error for RecoverableRecoverable<F, S> {}
impl<F: Clone, S: Clone> Spanning<F, S> for RecoverableRecoverable<F, S> {
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
pub struct RecoverableExpectsFormatter;
impl Display for RecoverableExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for RecoverableExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "something impossible") }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`recoverable()`]-combinator.
pub struct Recoverable<F, S> {
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<F, S> Combinator2<'static, F, S> for Recoverable<F, S> {
    type ExpectsFormatter = RecoverableExpectsFormatter;
    type Output = Infallible;
    type Recoverable = RecoverableRecoverable<F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { RecoverableExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        Err(SnackError::Recoverable(RecoverableRecoverable { span: input }))
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
/// The returned combinator fails always, with a recoverable [`RecoverableRecoverable`] error.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::error2::recoverable;
/// use ast_toolkit_snack::error2::recoverable::RecoverableRecoverable;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = recoverable();
/// assert_eq!(
///     comb.parse(span1),
///     Err(SnackError::Recoverable(RecoverableRecoverable { span: span1 }))
/// );
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(RecoverableRecoverable { span: span2 }))
/// );
/// ```
#[inline]
pub const fn recoverable<F, S>() -> Recoverable<F, S> { Recoverable { _f: PhantomData, _s: PhantomData } }
