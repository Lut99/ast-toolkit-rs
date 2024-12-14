//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    14 Dec 2024, 19:26:36
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`whitespace1()`]-combinator.
//

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpannableEq, Spanning};

use super::one_of1;
use crate::result::{Result as SResult, SnackError};
use crate::span::OneOfUtf8;
use crate::{Combinator2, ExpectsFormatter};


/***** ERRORS *****/
/// Error thrown by the [`Whitespace1`]-combinator that encodes that not even one whitespace was
/// parsed.
pub struct Whitespace1Recoverable<F, S> {
    /// The location where no whitespaces were found.
    pub span: Span<F, S>,
}
// NOTE: We manually implement `Debug` to avoid an unnecessary `Debug`-bound on `F` and `S`
impl<F, S> Debug for Whitespace1Recoverable<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut fmt = f.debug_struct("Whitespace1Recoverable");
        fmt.field("span", &self.span);
        fmt.finish()
    }
}
impl<F, S> Display for Whitespace1Recoverable<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", Whitespace1ExpectsFormatter) }
}
impl<F, S> Error for Whitespace1Recoverable<F, S> {}
impl<F: Clone, S: Clone> Spanning<F, S> for Whitespace1Recoverable<F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.span.clone() }

    #[inline]
    fn into_span(self) -> Span<F, S> { self.span }
}
impl<F, S: SpannableEq> Eq for Whitespace1Recoverable<F, S> {}
impl<F, S: SpannableEq> PartialEq for Whitespace1Recoverable<F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.span == other.span }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Whitespace1`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct Whitespace1ExpectsFormatter;
impl Display for Whitespace1ExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for Whitespace1ExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one space, tab, carriage return or newline") }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`whitespace1()`].
#[derive(Debug)]
pub struct Whitespace1<F, S> {
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<F, S> Combinator2<'static, F, S> for Whitespace1<F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type ExpectsFormatter = Whitespace1ExpectsFormatter;
    type Output = Span<F, S>;
    type Recoverable = Whitespace1Recoverable<F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { Whitespace1ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        match one_of1(&[" ", "\t", "\n", "\r", "\r\n"]).parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(Whitespace1Recoverable { span: err.into_span() })),
            Err(SnackError::Fatal(_)) => unreachable!(),
            Err(SnackError::NotEnough { .. }) => unreachable!(),
        }
    }
}





/***** LIBRARY *****/
/// Matches as many whitespace characters as possible.
///
/// Specifically, will match as many as possible from the following set of whitespaces:
/// - A space (` `);
/// - A tab (`\t`);
/// - A carriage return (`\r`); or
/// - A newline (`\n`).
///
/// This version does NOT accept matching none of them. See
/// [`whitespace0()`](super::super::whitespace0) for a version that does.
///
/// # Returns
/// A combinator [`Whitespace1`] that matches only whitespace characters (see above).
///
/// # Fails
/// The returned combinator fails if it did not match at least one whitespace.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::whitespace1;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "   \t\n  awesome");
/// let span2 = Span::new("<example>", "cool \n dope");
/// let span3 = Span::new("<example>", "");
///
/// let mut comb = whitespace1();
/// assert_eq!(comb.parse(span1), Ok((span1.slice(7..), span1.slice(..7))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(whitespace1::Whitespace1Recoverable { span: span2 }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(whitespace1::Whitespace1Recoverable { span: span3 }))
/// );
/// ```
#[inline]
pub const fn whitespace1<F, S>() -> Whitespace1<F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    Whitespace1 { _f: PhantomData, _s: PhantomData }
}
