//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    03 Nov 2024, 19:20:38
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`digit0()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use super::while0;
use crate::result::{Result as SResult, SnackError};
use crate::span::WhileUtf8;
use crate::{Combinator2, ExpectsFormatter};


/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Digit0`]-combinator.
#[derive(Debug)]
pub struct Digit0ExpectsFormatter;
impl Display for Digit0ExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for Digit0ExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "digits") }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`digit0()`].
#[derive(Debug)]
pub struct Digit0<F, S> {
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<F, S> Combinator2<'static, F, S> for Digit0<F, S>
where
    F: Clone,
    S: Clone + WhileUtf8,
{
    type ExpectsFormatter = Digit0ExpectsFormatter;
    type Output = Span<F, S>;
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { Digit0ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        match while0(|c: &str| -> bool {
            c.len() == 1 && {
                let c: char = c.chars().next().unwrap();
                c >= '0' && c <= '9'
            }
        })
        .parse(input)
        {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(_)) => unreachable!(),
            Err(SnackError::Fatal(_)) => unreachable!(),
            Err(SnackError::NotEnough { .. }) => unreachable!(),
        }
    }
}





/***** LIBRARY *****/
/// Matches as many digits as possible.
///
/// This version accepts matching none of them. See [`digit1()`](super::complete::digit1()) (or its
/// streaming version, [`digit1()`](super::streaming::digit1())) to assert at least something must
/// be matched.
///
/// # Returns
/// A combinator [`Digit0`] that matches only digits 0-9.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::digit0;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "12345six");
/// let span2 = Span::new("<example>", "one23456");
/// let span3 = Span::new("<example>", "");
///
/// let mut comb = digit0();
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(comb.parse(span2), Ok((span2, span2.slice(..0))));
/// assert_eq!(comb.parse(span3), Ok((span3, span3.slice(..0))));
/// ```
#[inline]
pub const fn digit0<F, S>() -> Digit0<F, S>
where
    F: Clone,
    S: Clone + WhileUtf8,
{
    Digit0 { _f: PhantomData, _s: PhantomData }
}