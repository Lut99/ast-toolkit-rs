//  TAG.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:25:00
//  Last edited:
//    09 Jan 2025, 19:01:54
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the (binary) [`tag()`]-combinator.
//

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::{Span, Spanning};
use better_derive::{Debug, Eq, PartialEq};

use crate::result::{Result as SResult, SnackError};
use crate::span::MatchBytes;
use crate::{Combinator2, ExpectsFormatter};


/***** ERRORS *****/
// Recoverable error for the [`Tag`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct TagRecoverable<'t, F, S> {
    /// What we expected
    pub tag:  &'t [u8],
    /// Where we expected it
    pub span: Span<F, S>,
}
impl<'t, F, S> Display for TagRecoverable<'t, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { write!(f, "{}", TagExpectsFormatter { tag: self.tag }) }
}
impl<'t, F, S> Error for TagRecoverable<'t, F, S> {}
impl<'t, F: Clone, S: Clone> Spanning<F, S> for TagRecoverable<'t, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.span.clone() }

    #[inline]
    fn into_span(self) -> Span<F, S> { self.span }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Tag`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct TagExpectsFormatter<'t> {
    /// The tag of bytes we expect one of.
    pub tag: &'t [u8],
}
impl<'t> Display for TagExpectsFormatter<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for TagExpectsFormatter<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "{:#04X?}", self.tag) }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`tag()`].
pub struct Tag<'t, F, S> {
    /// The actual tag that is being matched for.
    tag: &'t [u8],
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:  PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:  PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'t`.
impl<'c, 't, F, S> Combinator2<'c, F, S> for Tag<'t, F, S>
where
    't: 'c,
    F: Clone,
    S: Clone + MatchBytes,
{
    type ExpectsFormatter = TagExpectsFormatter<'t>;
    type Output = Span<F, S>;
    type Recoverable = TagRecoverable<'t, F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { TagExpectsFormatter { tag: self.tag } }

    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        // See if we can parse the input
        let match_point: usize = input.match_bytes(SpanRange::Open, self.tag);
        if match_point >= self.tag.len() {
            // Matched the entire tag
            #[cfg(debug_assertions)]
            assert!(match_point == self.tag.len());
            Ok((input.slice(match_point..), input.slice(..match_point)))
        } else {
            // Didn't match the entire tag
            Err(SnackError::Recoverable(TagRecoverable { tag: self.tag, span: input.start_onwards() }))
        }
    }
}





/***** LIBRARY *****/
/// Matches a specific "tag", i.e., a sequence of bytes.
///
/// Useful for matching (binary) keywords.
///
/// # Arguments
/// - `tag`: The tag to match for.
///
/// # Returns
/// A combinator [`Tag`] that will match the prefix of input if it matches `tag`.
///
/// # Fails
/// The returned combinator fails if the prefix of the input was not `tag`.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::bytes2::complete::tag;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::<&str, &[u8]>::new("<example>", b"Hello, world!");
/// let span2 = Span::<&str, &[u8]>::new("<example>", b"Goodbye, world!");
/// let span3 = Span::<&str, &[u8]>::new("<example>", b"Hell");
///
/// let mut comb = tag(b"Hello");
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::TagRecoverable {
///         tag:  b"Hello",
///         span: span2.slice(0..),
///     }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(tag::TagRecoverable {
///         tag:  b"Hello",
///         span: span3.slice(0..),
///     }))
/// );
/// ```
pub const fn tag<'t, F, S>(tag: &'t [u8]) -> Tag<'t, F, S>
where
    F: Clone,
    S: Clone + MatchBytes,
{
    Tag { tag, _f: PhantomData, _s: PhantomData }
}
