//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    07 Mar 2025, 14:33:16
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`whitespace0()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use super::one_of0;
use crate::result::{Result as SResult, SnackError};
use crate::span::OneOfUtf8;
use crate::{Combinator, ExpectsFormatter as _};


/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Whitespace0`]-combinator.
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
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "spaces, tabs, carriage returns or newlines") }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`whitespace0()`].
#[derive(Debug)]
pub struct Whitespace0<F, S> {
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<F, S> Combinator<'static, F, S> for Whitespace0<F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = Span<F, S>;
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        match one_of0(&[" ", "\t", "\n", "\r", "\r\n"]).parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(_)) => unreachable!(),
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
/// This version accepts matching none of them. See
/// [`whitespace1()`](super::complete::whitespace1()) (or its streaming version,
/// [`whitespace1()`](super::streaming::whitespace1())) to assert at least something must be
/// matched.
///
/// # Returns
/// A combinator [`Whitespace0`] that matches only whitespace characters (see above).
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::whitespace0;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "   \t\n  awesome");
/// let span2 = Span::new("<example>", "cool \n dope");
/// let span3 = Span::new("<example>", "");
///
/// let mut comb = whitespace0();
/// assert_eq!(comb.parse(span1), Ok((span1.slice(7..), span1.slice(..7))));
/// assert_eq!(comb.parse(span2), Ok((span2, span2.slice(..0))));
/// assert_eq!(comb.parse(span3), Ok((span3, span3.slice(..0))));
/// ```
#[inline]
pub const fn whitespace0<F, S>() -> Whitespace0<F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    Whitespace0 { _f: PhantomData, _s: PhantomData }
}
