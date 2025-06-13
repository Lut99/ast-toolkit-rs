//  TAG.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:25:00
//  Last edited:
//    08 May 2025, 14:39:51
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the (binary) [`tag()`]-combinator.
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
// Recoverable error for the [`Tag`]-combinator.
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'t, 's, S>, bound = (S: Spannable<'s>))]
pub struct Recoverable<'t, S> {
    /// What we expected
    pub tag: &'t [u8],
    /// Whether more might fix the error.
    ///
    /// If true, this means that `tag` matches whatever is still sliced by `span` - there just
    /// isn't enough of it.
    pub is_fixable: bool,
    /// Where we expected it
    pub span: Span<S>,
}
impl<'t, S> Display for Recoverable<'t, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { write!(f, "{}", ExpectsFormatter { tag: self.tag }) }
}
impl<'t, 'a, S: Spannable<'a>> Error for Recoverable<'t, S> {}
impl<'t, S: Clone> Spanning<S> for Recoverable<'t, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}
impl<'s, 't, S: Clone + Spannable<'s>> ParseError<S> for Recoverable<'t, S> {
    #[inline]
    fn more_might_fix(&self) -> bool { self.is_fixable }

    #[inline]
    fn needed_to_fix(&self) -> Option<usize> {
        if self.is_fixable {
            // SAFETY: This will never underflow because `self.is_fixable` only occurs when there isn't
            // enough in `self.span` to match `self.tag` (i.e., it is shorter)
            Some(self.tag.len() - self.span.len())
        } else {
            None
        }
    }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Tag`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'t> {
    /// The tag of bytes we expect one of.
    pub tag: &'t [u8],
}
impl<'t> Display for ExpectsFormatter<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl<'t> crate::ExpectsFormatter for ExpectsFormatter<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "{:#04X?}", self.tag) }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`tag()`].
pub struct Tag<'c, S> {
    /// The actual tag that is being matched for.
    tag: &'c [u8],
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:  PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'t`.
impl<'c, 's, 'a, S> Combinator<'a, 's, S> for Tag<'c, S>
where
    'c: 'a,
    S: Clone + SpannableBytes<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<'c>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'c, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { tag: self.tag } }

    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Match the tag
        let tag_len: usize = self.tag.len();
        let mut i: usize = 0;
        let split: usize = input.match_bytes_while(|b| {
            if i < tag_len && b == self.tag[i] {
                i += 1;
                true
            } else {
                false
            }
        });

        // Assert there is at least one
        if split == tag_len {
            Ok((input.slice(i..), input.slice(..i)))
        } else if split > tag_len {
            unreachable!()
        } else {
            Err(SnackError::Recoverable(Recoverable { tag: self.tag, is_fixable: split >= input.len(), span: input }))
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
/// The returned combinator fails if the prefix of the input was not `tag`. If this was because of
/// an unexpected end-of-file, [`SnackError::NotEnough`] is returned instead.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::bytes::tag;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::{Combinator as _, ParseError as _};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new(b"Hello, world!".as_slice());
/// let span2 = Span::new(b"Goodbye, world!".as_slice());
/// let span3 = Span::new(b"Hell".as_slice());
/// let span4 = Span::new(b"abc".as_slice());
///
/// let mut comb = tag(b"Hello");
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
///
/// let err = comb.parse(span2);
/// assert_eq!(err, Err(SnackError::Recoverable(tag::Recoverable { tag: b"Hello", span: span2 })));
/// assert!(!err.more_might_fix());
///
/// let err = comb.parse(span3);
/// assert_eq!(err, Err(SnackError::Recoverable(tag::Recoverable { tag: b"Hello", span: span4 })));
/// assert!(err.more_might_fix());
/// assert_eq!(err.needed_to_fix(), Some(1));
///
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: b"Hello", span: span4 }))
/// );
/// ```
pub const fn tag<'c, 's, S>(tag: &'c [u8]) -> Tag<'c, S>
where
    S: Clone + SpannableBytes<'s>,
{
    Tag { tag, _s: PhantomData }
}
