//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    18 Jan 2025, 18:11:02
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`digit1()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning as _};

pub use super::super::complete::digit1::{ExpectsFormatter, Recoverable};
use super::while1;
use crate::Combinator;
use crate::result::{Result as SResult, SnackError};
use crate::span::{LenBytes, WhileUtf8};


/***** COMBINATORS *****/
/// Actual combinator implementing [`digit1()`].
#[derive(Debug)]
pub struct Digit1<F, S> {
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<F, S> Combinator<'static, F, S> for Digit1<F, S>
where
    F: Clone,
    S: Clone + LenBytes + WhileUtf8,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = Span<F, S>;
    type Recoverable = Recoverable<F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        match while1("", |c: &str| -> bool {
            c.len() == 1 && {
                let c: char = c.chars().next().unwrap();
                c >= '0' && c <= '9'
            }
        })
        .parse(input)
        {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(Recoverable { fmt: self.expects(), span: err.into_span() })),
            Err(SnackError::Fatal(_)) => unreachable!(),
            Err(SnackError::NotEnough { needed, span }) => Err(SnackError::NotEnough { needed, span }),
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
/// The returned combinator fails if it did not match at least one digit. If this match failed
/// because end-of-input was reached, then this fails with a [`SnackError::NotEnough`] instead.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::streaming::digit1;
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
///     Err(SnackError::Recoverable(digit1::Recoverable {
///         fmt:  digit1::ExpectsFormatter,
///         span: span2,
///     }))
/// );
/// assert_eq!(comb.parse(span3), Err(SnackError::NotEnough { needed: Some(1), span: span3 }));
/// ```
#[inline]
pub const fn digit1<F, S>() -> Digit1<F, S>
where
    F: Clone,
    S: Clone + LenBytes + WhileUtf8,
{
    Digit1 { _f: PhantomData, _s: PhantomData }
}
