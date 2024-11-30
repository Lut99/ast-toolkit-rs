//  FATAL.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 21:50:58
//  Last edited:
//    30 Nov 2024, 22:00:47
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`fatal()`]-combinator.
//

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpannableEq, Spanning};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator2, ExpectsFormatter};


/***** ERRORS *****/
/// Defines the fatal error thrown by the [`Fatal`]-combinator.
pub struct FatalFatal<F, S> {
    /// The place where the fatal error was thrown.
    pub span: Span<F, S>,
}
impl<F, S> Debug for FatalFatal<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut fmt = f.debug_struct("FatalFatal");
        fmt.field("span", &self.span);
        fmt.finish()
    }
}
impl<F, S> Display for FatalFatal<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "A fatal error has occurred while parsing") }
}
impl<F, S> Error for FatalFatal<F, S> {}
impl<F: Clone, S: Clone> Spanning<F, S> for FatalFatal<F, S> {
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
impl<F, S: SpannableEq> Eq for FatalFatal<F, S> {}
impl<F, S: SpannableEq> PartialEq for FatalFatal<F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.span.eq(&other.span) }
}





/***** FORMATTERS *****/
/// Expectsformatter for the [`Fatal`]-combinator.
#[derive(Debug)]
pub struct FatalExpectsFormatter;
impl Display for FatalExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for FatalExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "something impossible") }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`fatal()`]-combinator.
pub struct Fatal<F, S> {
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<F, S> Combinator2<'static, F, S> for Fatal<F, S> {
    type ExpectsFormatter = FatalExpectsFormatter;
    type Output = Infallible;
    type Recoverable = Infallible;
    type Fatal = FatalFatal<F, S>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { FatalExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        Err(SnackError::Fatal(FatalFatal { span: input }))
    }
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
/// The returned combinator fails always, with a fatal [`FatalFatal`] error.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::error2::fatal;
/// use ast_toolkit_snack::error2::fatal::FatalFatal;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = fatal();
/// assert_eq!(comb.parse(span1), Err(SnackError::Fatal(FatalFatal { span: span1 })));
/// assert_eq!(comb.parse(span2), Err(SnackError::Fatal(FatalFatal { span: span2 })));
/// ```
#[inline]
pub const fn fatal<F, S>() -> Fatal<F, S> { Fatal { _f: PhantomData, _s: PhantomData } }
