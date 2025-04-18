//  WHILE 1.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:42:41
//  Last edited:
//    18 Mar 2025, 10:24:20
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

use ast_toolkit_span::{Span, Spannable, Spanning};
use better_derive::{Debug, Eq, PartialEq};

use crate::result::{Result as SResult, SnackError};
use crate::span::BytesParsable;
use crate::{Combinator, ExpectsFormatter as _};


/***** ERRORS *****/
/// Error thrown by the [`While1`]-combinator that encodes that not even one of the expected
/// bytes was parsed.
#[derive(Debug, Eq, PartialEq)]
pub struct Recoverable<'t, S> {
    /// Some description of what was expected.
    pub what: &'t str,
    /// The location where no bytes were found.
    pub span: Span<S>,
}
impl<'t, S> Display for Recoverable<'t, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", ExpectsFormatter { what: self.what }) }
}
impl<'t, S: Spannable> Error for Recoverable<'t, S> {}
impl<'t, S: Clone> Spanning<S> for Recoverable<'t, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`While1`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'t> {
    /// Some description of what was expected.
    pub what: &'t str,
}
impl<'t> Display for ExpectsFormatter<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> crate::ExpectsFormatter for ExpectsFormatter<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one {}", self.what) }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`while1()`]-combinator.
pub struct While1<'t, S, P> {
    /// The predicate used for matching.
    predicate: P,
    /// A helper provided by the user to describe what is expected.
    what: &'t str,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<'t, S, P> Combinator<'t, S> for While1<'t, S, P>
where
    S: Clone + BytesParsable,
    P: FnMut(u8) -> bool,
{
    type ExpectsFormatter = ExpectsFormatter<'t>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'t, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: self.what } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Try to iterate over the head to find the match
        let mut i: usize = 0;
        for byte in input.bytes() {
            // Check if it's in the set
            if (self.predicate)(*byte) {
                i += 1;
                continue;
            } else {
                break;
            }
        }

        // Return if there's at least one
        if i > 0 { Ok((input.slice(i..), input.slice(..i))) } else { Err(SnackError::Recoverable(Recoverable { what: self.what, span: input })) }
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many bytes from the start of a span as possible, as long as those
/// bytes match a given predicate.
///
/// This version does _not_ accept matching none of them. See [`while0()`](super::super::while0())
/// to also allow finding none.
///
/// # Arguments
/// - `what`: A short string describing what byte is being matched. Should finish the sentence
///   "Expected at least one ...".
/// - `predicate`: A closure that returns true for matching bytes, and false for non-matching
///   bytes. All bytes that are matched are returned up to the first for which
///   `predicate` returns false (if any).
///
/// # Returns
/// A combinator [`While1`] that will match the prefix of input as long as those bytes match
/// the given `predicate`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one byte.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::bytes::complete::while1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new(b"abcdefg".as_slice());
/// let span2 = Span::new(b"cdefghi".as_slice());
/// let span3 = Span::new("abÿcdef".as_bytes());
/// let span4 = Span::new(b"hijklmn".as_slice());
/// let span5 = Span::new(b"".as_slice());
///
/// let mut comb = while1("'a', 'b', 'c' or 'ÿ'", |b: u8| -> bool {
///     b == b'a' || b == b'b' || b == b'c' || b == 191 || b == 195
/// });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(while1::Recoverable {
///         what: "'a', 'b', 'c' or 'ÿ'",
///         span: span4,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Recoverable(while1::Recoverable {
///         what: "'a', 'b', 'c' or 'ÿ'",
///         span: span5,
///     }))
/// );
/// ```
#[inline]
pub const fn while1<'t, S, P>(what: &'t str, predicate: P) -> While1<'t, S, P>
where
    S: Clone + BytesParsable,
    P: FnMut(u8) -> bool,
{
    While1 { predicate, what, _s: PhantomData }
}
