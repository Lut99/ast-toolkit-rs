//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    03 Nov 2024, 19:22:26
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`digit1()`]-combinator.
//

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpannableEq, Spanning};

use super::while1;
use crate::result::{Result as SResult, SnackError};
use crate::span::WhileUtf8;
use crate::{Combinator2, ExpectsFormatter};


/***** ERRORS *****/
/// Error thrown by the [`Digit1`]-combinator that encodes that not even one digit was parsed.
pub struct Digit1Recoverable<F, S> {
    /// The location where no digits were found.
    pub span: Span<F, S>,
}
// NOTE: We manually implement `Debug` to avoid an unnecessary `Debug`-bound on `F` and `S`
impl<F, S> Debug for Digit1Recoverable<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut fmt = f.debug_struct("Digit1Recoverable");
        fmt.field("span", &self.span);
        fmt.finish()
    }
}
impl<F, S> Display for Digit1Recoverable<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", Digit1ExpectsFormatter) }
}
impl<F, S> Error for Digit1Recoverable<F, S> {}
impl<F: Clone, S: Clone> Spanning<F, S> for Digit1Recoverable<F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.span.clone() }

    #[inline]
    fn into_span(self) -> Span<F, S> { self.span }
}
impl<F, S: SpannableEq> Eq for Digit1Recoverable<F, S> {}
impl<F, S: SpannableEq> PartialEq for Digit1Recoverable<F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.span == other.span }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`digit1()`]-combinator.
#[derive(Debug)]
pub struct Digit1ExpectsFormatter;
impl Display for Digit1ExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for Digit1ExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one digit") }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`digit1()`].
#[derive(Debug)]
pub struct Digit1<F, S> {
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<F, S> Combinator2<'static, F, S> for Digit1<F, S>
where
    F: Clone,
    S: Clone + WhileUtf8,
{
    type ExpectsFormatter = Digit1ExpectsFormatter;
    type Output = Span<F, S>;
    type Recoverable = Digit1Recoverable<F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { Digit1ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        match while1(|c: &str| -> bool {
            c.len() == 1 && {
                let c: char = c.chars().next().unwrap();
                c >= '0' && c <= '9'
            }
        })
        .parse(input)
        {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(Digit1Recoverable { span: err.into_span() })),
            Err(SnackError::Fatal(_)) => unreachable!(),
            Err(SnackError::NotEnough { .. }) => unreachable!(),
        }
    }
}





/***** LIBRARY *****/
/// Matches as many digits as possible.
///
/// This version does _not_ accept matching none of them. See [`digit0()`](super::super::digit0())
/// to also allow finding none.
///
/// # Returns
/// A combinator [`Digit1`] that matches only digits 0-9.
///
/// # Fails
/// The returned combinator fails if it did not match at least one digit.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::digit1;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "12345six");
/// let span2 = Span::new("<example>", "one23456");
/// let span3 = Span::new("<example>", "");
///
/// let mut comb = digit1();
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(digit1::Digit1Recoverable { span: span2 }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(digit1::Digit1Recoverable { span: span3 }))
/// );
/// ```
#[inline]
pub const fn digit1<F, S>() -> Digit1<F, S>
where
    F: Clone,
    S: Clone + WhileUtf8,
{
    Digit1 { _f: PhantomData, _s: PhantomData }
}
