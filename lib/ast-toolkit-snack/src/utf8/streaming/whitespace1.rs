//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    22 Apr 2025, 11:37:23
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`whitespace1()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, Spanning};

pub use super::super::complete::whitespace1::{ExpectsFormatter, Recoverable};
use super::one_of1;
use crate::Combinator;
use crate::result::{Result as SResult, SnackError};
use crate::span::Utf8Parsable;


/***** COMBINATORS *****/
/// Actual combinator implementing [`whitespace1()`].
#[derive(Debug)]
pub struct Whitespace1<S> {
    _s: PhantomData<S>,
}
impl<'s, S> Combinator<'static, 's, S> for Whitespace1<S>
where
    S: Clone + Spannable<'s>,
    S::Slice: Utf8Parsable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = Span<S>;
    type Recoverable = Recoverable<S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match one_of1(&[" ", "\t", "\n", "\r", "\r\n"]).parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(Recoverable { fmt: self.expects(), span: err.into_span() })),
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
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::streaming::whitespace1;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("   \t\n  awesome");
/// let span2 = Span::new("cool \n dope");
/// let span3 = Span::new("");
///
/// let mut comb = whitespace1();
/// assert_eq!(comb.parse(span1), Ok((span1.slice(7..), span1.slice(..7))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(whitespace1::Recoverable {
///         fmt:  whitespace1::ExpectsFormatter,
///         span: span2,
///     }))
/// );
/// assert_eq!(comb.parse(span3), Err(SnackError::NotEnough { needed: Some(1), span: span3 }));
/// ```
#[inline]
pub const fn whitespace1<'s, S>() -> Whitespace1<S>
where
    S: Clone + Spannable<'s>,
    S::Slice: Utf8Parsable<'s>,
{
    Whitespace1 { _s: PhantomData }
}
