//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    08 May 2025, 11:56:23
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`digit0()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::Debug;
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpannableBytes};

use crate::Combinator;
use crate::result::Result as SResult;
use crate::scan::while0;


/***** TYPE ALIASES *****/
/// ExpectsFormatter for the [`Digit0`]-combinator.
pub type ExpectsFormatter = while0::ExpectsFormatter<'static>;





/***** COMBINATORS *****/
/// Actual combinator implementing [`digit0()`].
#[derive(Debug)]
pub struct Digit0<S> {
    _s: PhantomData<S>,
}
impl<'s, S> Combinator<'static, 's, S> for Digit0<S>
where
    S: Clone + SpannableBytes<'s>,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = Span<S>;
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: "digits" } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        while0("digits", |b| -> bool { *b >= b'0' && *b <= b'9' }).parse(input)
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
/// use ast_toolkit_snack::ascii::digit0;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("12345six");
/// let span2 = Span::new("one23456");
/// let span3 = Span::new("");
///
/// let mut comb = digit0();
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(0..), span2.slice(..0))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(0..), span3.slice(..0))));
/// ```
#[inline]
pub const fn digit0<'s, S>() -> Digit0<S>
where
    S: Clone + SpannableBytes<'s>,
{
    Digit0 { _s: PhantomData }
}
