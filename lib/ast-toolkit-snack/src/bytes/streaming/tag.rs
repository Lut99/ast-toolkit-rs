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

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpannableBytes};

pub use super::super::complete::tag::{ExpectsFormatter, Recoverable, tag as tag_complete};
use crate::Combinator;
use crate::result::{Result as SResult, SnackError};


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
        } else if split < input.len() {
            Err(SnackError::Recoverable(Recoverable { tag: self.tag, span: input }))
        } else {
            Err(SnackError::NotEnough { needed: Some(tag_len - split), span: input.slice(split..) })
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
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::bytes::streaming::tag;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new(b"Hello, world!".as_slice());
/// let span2 = Span::new(b"Goodbye, world!".as_slice());
/// let span3 = Span::new(b"Hell".as_slice());
/// let span4 = Span::new(b"abc".as_slice());
///
/// let mut comb = tag(b"Hello");
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: b"Hello", span: span2 }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::NotEnough { needed: Some(1), span: span3.slice(4..) })
/// );
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
