//  WHILE 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:40:18
//  Last edited:
//    08 May 2025, 11:55:36
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`while1()`]-combinator.
//

use std::borrow::Cow;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, SpannableBytes, Spanning};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ParseError};


/***** ERRORS *****/
/// Error thrown by the [`While1`]-combinator that encodes that not even one of the expected
/// characters was parsed.
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'c, 's, S>, bound = (S: Spannable<'s>))]
pub struct Recoverable<'c, S> {
    /// Some string describing what we were matching.
    pub what: &'c str,
    /// The location where no characters were found.
    pub span: Span<S>,
}
impl<'c, S> Display for Recoverable<'c, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", ExpectsFormatter { what: self.what }) }
}
impl<'c, 's, S: Spannable<'s>> Error for Recoverable<'c, S> {}
impl<'c, S: Clone> Spanning<S> for Recoverable<'c, S> {
    #[inline]
    fn get_span(&self) -> Cow<'_, Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn take_span(self) -> Span<S> { self.span }
}
impl<'c, 's, S: Clone + Spannable<'s>> ParseError<S> for Recoverable<'c, S> {
    #[inline]
    fn more_might_fix(&self) -> bool { self.span.is_empty() }

    #[inline]
    fn needed_to_fix(&self) -> Option<usize> { if self.more_might_fix() { Some(1) } else { None } }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`While1`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'c> {
    /// Some string describing what we were matching.
    pub what: &'c str,
}
impl<'c> Display for ExpectsFormatter<'c> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl<'c> crate::ExpectsFormatter for ExpectsFormatter<'c> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one {}", self.what) }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`While1()`].
#[derive(Debug)]
pub struct While1<'c, P, S> {
    predicate: P,
    what: &'c str,
    _s: PhantomData<S>,
}
impl<'c, 's, 'a, P, S> Combinator<'a, 's, S> for While1<'c, P, S>
where
    'c: 'a,
    P: for<'b> FnMut(&'b str) -> bool,
    S: Clone + SpannableBytes<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<'c>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'c, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: self.what } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Check first if there's *any* input to parse.
        if input.is_empty() {
            return Err(SnackError::Recoverable(Recoverable { what: self.what, span: input }));
        }

        // Otherwise, continue as usual
        let split: usize = input.match_utf8_while(&mut self.predicate);
        if split > 0 {
            Ok((input.slice(split..), input.slice(..split)))
        } else {
            Err(SnackError::Recoverable(Recoverable { what: self.what, span: input }))
        }
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
/// use ast_toolkit_snack::utf8::while1;
/// use ast_toolkit_snack::{Combinator as _, ParseError as _};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("abcdefg");
/// let span2 = Span::new("cdefghi");
/// let span3 = Span::new("abÿcdef");
/// let span4 = Span::new("hijklmn");
/// let span5 = Span::new("");
///
/// let mut comb = while1("'a', 'b', 'c' or 'ÿ'", |c: &str| -> bool {
///     c == "a" || c == "b" || c == "c" || c == "ÿ"
/// });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
///
/// let err = comb.parse(span4);
/// assert_eq!(
///     err,
///     Err(SnackError::Recoverable(while1::Recoverable {
///         what: "'a', 'b', 'c' or 'ÿ'",
///         span: span4,
///     }))
/// );
/// assert!(!err.as_ref().unwrap_err().more_might_fix());
///
/// let err = comb.parse(span5);
/// assert_eq!(
///     err,
///     Err(SnackError::Recoverable(while1::Recoverable {
///         what: "'a', 'b', 'c' or 'ÿ'",
///         span: span5,
///     }))
/// );
/// assert!(err.as_ref().unwrap_err().more_might_fix());
/// assert_eq!(err.as_ref().unwrap_err().needed_to_fix(), Some(1));
/// ```
#[inline]
pub const fn while1<'c, 's, P, S>(what: &'c str, predicate: P) -> While1<'c, P, S>
where
    P: for<'a> FnMut(&'a str) -> bool,
    S: Clone + SpannableBytes<'s>,
{
    While1 { predicate, what, _s: PhantomData }
}
