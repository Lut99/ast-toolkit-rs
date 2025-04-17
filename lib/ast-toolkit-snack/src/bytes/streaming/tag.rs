//  TAG.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:25:00
//  Last edited:
//    18 Mar 2025, 10:24:33
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the (binary) [`tag()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::Span;

pub use super::super::complete::tag::{ExpectsFormatter, Recoverable};
use crate::Combinator;
use crate::result::{Result as SResult, SnackError};
use crate::span::BytesParsable;


/***** COMBINATORS *****/
/// Actual combinator implementing [`tag()`].
pub struct Tag<'t, S> {
    /// The actual tag that is being matched for.
    tag: &'t [u8],
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:  PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'t`.
impl<'c, 't, S> Combinator<'c, S> for Tag<'t, S>
where
    't: 'c,
    S: Clone + BytesParsable,
{
    type ExpectsFormatter = ExpectsFormatter<'t>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'t, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { tag: self.tag } }

    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Try to iterate over the head to find the match
        let mut i: usize = 0;
        let mut bytes = input.bytes();
        for byte in self.tag {
            // Attempt to get the next byte
            match bytes.next() {
                Some(head) if byte == head => {
                    i += 1;
                    continue;
                },
                Some(_) => {
                    // Note: required, or else Rust will think the iterator will be destructed at
                    // the end of the loop
                    drop(bytes);
                    return Err(SnackError::Recoverable(Recoverable { tag: self.tag, span: input }));
                },
                None => {
                    // We crash with notenough instead
                    drop(bytes);
                    return Err(SnackError::NotEnough { needed: Some(self.tag.len() - i), span: input.slice(i..) });
                },
            }
        }
        #[cfg(debug_assertions)]
        assert_eq!(i, self.tag.len());

        // We parsed it!
        Ok((input.slice(i..), input.slice(..i)))
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
///
/// let mut comb = tag(b"Hello");
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: b"Hello", span: span2.slice(..) }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::NotEnough { needed: Some(1), span: span3.slice(4..) })
/// );
/// ```
pub const fn tag<'t, S>(tag: &'t [u8]) -> Tag<'t, S>
where
    S: Clone + BytesParsable,
{
    Tag { tag, _s: PhantomData }
}
