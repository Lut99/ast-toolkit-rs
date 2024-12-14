//  WHILE 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:40:18
//  Last edited:
//    14 Dec 2024, 19:39:18
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`while1()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use super::super::complete::while1 as while1_complete;
pub use super::super::complete::while1::{While1ExpectsFormatter, While1Recoverable};
use crate::Combinator2;
use crate::result::{Result as SResult, SnackError};
use crate::span::{LenBytes, WhileUtf8};


/***** COMBINATORS *****/
/// Actual combinator implementing [`While1()`].
#[derive(Debug)]
pub struct While1<'t, P, F, S> {
    predicate: P,
    what: &'t str,
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<'t, P, F, S> Combinator2<'static, F, S> for While1<'t, P, F, S>
where
    P: for<'a> FnMut(&'a str) -> bool,
    F: Clone,
    S: Clone + LenBytes + WhileUtf8,
{
    type ExpectsFormatter = While1ExpectsFormatter<'t>;
    type Output = Span<F, S>;
    type Recoverable = While1Recoverable<'t, F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { While1ExpectsFormatter { what: self.what } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        // Check first if there's *any* input to parse.
        if input.len() == 0 {
            return Err(SnackError::NotEnough { needed: Some(1), span: input });
        }

        // Otherwise, continue as usual
        while1_complete(self.what, &mut self.predicate).parse(input)
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many characters from the start of a span as possible, as long as those
/// characters match a given predicate.
///
/// This version does _not_ accept matching none of them. See [`while0()`](super::super::while0())
/// to also allow finding none.
///
/// # Arguments
/// - `what`: A short string describing what byte is being matched. Should finish the sentence
///   "Expected at least one ...".
/// - `predicate`: A closure that returns true for matching characters, and false for non-matching
///   characters. All characters that are matched are returned up to the first for which
///   `predicate` returns false (if any).
///
/// # Returns
/// A combinator [`While1`] that will match the prefix of input as long as those characters match
/// the given `predicate`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one character. If this match failed
/// because end-of-input was reached, then this fails with a [`SnackError::NotEnough`] instead.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::streaming::while1;
/// use ast_toolkit_snack::{Combinator2 as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "abcdefg");
/// let span2 = Span::new("<example>", "cdefghi");
/// let span3 = Span::new("<example>", "abÿcdef");
/// let span4 = Span::new("<example>", "hijklmn");
/// let span5 = Span::new("<example>", "");
///
/// let mut comb = while1("'a', 'b', 'c' or 'ÿ'", |c: &str| -> bool {
///     c == "a" || c == "b" || c == "c" || c == "ÿ"
/// });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(while1::While1Recoverable {
///         what: "'a', 'b', 'c' or 'ÿ'",
///         span: span4,
///     }))
/// );
/// assert_eq!(comb.parse(span5), Err(SnackError::NotEnough { needed: Some(1), span: span5 }));
/// ```
#[inline]
pub const fn while1<'t, P, F, S>(what: &'t str, predicate: P) -> While1<'t, P, F, S>
where
    P: for<'a> FnMut(&'a str) -> bool,
    F: Clone,
    S: Clone + LenBytes + WhileUtf8,
{
    While1 { predicate, what, _f: PhantomData, _s: PhantomData }
}
