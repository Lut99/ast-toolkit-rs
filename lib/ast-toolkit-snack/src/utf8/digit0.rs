//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    07 Mar 2025, 14:42:37
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
use crate::{Combinator, ExpectsFormatter as _};


/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Digit0`]-combinator.
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
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "digits") }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`digit0()`].
#[derive(Debug)]
pub struct Digit0<F, S> {
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<F, S> Combinator<'static, F, S> for Digit0<F, S>
where
    F: Clone,
    S: Clone + WhileUtf8,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = Span<F, S>;
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        match while0("", |c: &str| -> bool {
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
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::digit0;
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
