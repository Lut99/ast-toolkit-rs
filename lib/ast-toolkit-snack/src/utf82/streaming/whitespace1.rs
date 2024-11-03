//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    03 Nov 2024, 19:25:44
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`whitespace1()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning};

pub use super::super::complete::whitespace1::{Whitespace1ExpectsFormatter, Whitespace1Recoverable};
use super::one_of1;
use crate::Combinator2;
use crate::result::{Result as SResult, SnackError};
use crate::span::{LenBytes, OneOfUtf8};


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
    S: Clone + LenBytes + OneOfUtf8,
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
            Err(SnackError::NotEnough { needed, span }) => Err(SnackError::NotEnough { needed, span }),
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
/// The returned combinator fails if it did not match at least one whitespace. If this match failed
/// because end-of-input was reached, then this fails with a [`SnackError::NotEnough`] instead.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::streaming::whitespace1;
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
/// assert_eq!(comb.parse(span3), Err(SnackError::NotEnough { needed: Some(1), span: span3 }));
/// ```
#[inline]
pub const fn whitespace1<F, S>() -> Whitespace1<F, S>
where
    F: Clone,
    S: Clone + LenBytes + OneOfUtf8,
{
    Whitespace1 { _f: PhantomData, _s: PhantomData }
}
