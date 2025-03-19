//  TAG.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 17:16:33
//  Last edited:
//    19 Mar 2025, 09:46:35
//  Auto updated?
//    Yes
//
//  Description:
//!   Implemens the [`tag()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::Span;
use unicode_segmentation::UnicodeSegmentation as _;

pub use super::super::complete::tag::{ExpectsFormatter, Recoverable};
use crate::Combinator;
use crate::result::{Result as SResult, SnackError};
use crate::span::Utf8Parsable;


/***** COMBINATORS *****/
/// Actual combinator implementing [`tag()`].
#[derive(Debug)]
pub struct Tag<'t, S> {
    tag: &'t str,
    _s:  PhantomData<S>,
}
impl<'t, S> Combinator<'t, S> for Tag<'t, S>
where
    S: Clone + Utf8Parsable,
{
    type ExpectsFormatter = ExpectsFormatter<'t>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'t, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { tag: self.tag } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Try to iterate over the head to find the match
        let mut i: usize = 0;
        let mut head = input.graphs();
        for c in self.tag.graphemes(true) {
            // Attempt to get the next byte
            match head.next() {
                Some(head) if c == head => {
                    i += head.len();
                    continue;
                },
                Some(_) => {
                    // Note: required, or else Rust will think the iterator will be destructed at
                    // the end of the loop
                    drop(head);
                    return Err(SnackError::Recoverable(Recoverable { tag: self.tag, span: input }));
                },
                None => {
                    // We crash with notenough instead
                    drop(head);
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
/// Matches a specific "tag", i.e., a sequence of UTF-8 characters.
///
/// Useful for matching keywords.
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
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::streaming::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
/// let span3 = Span::new("Hell");
///
/// let mut comb = tag("Hello");
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: "Hello", span: span2.slice(..) }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::NotEnough { needed: Some(1), span: span3.slice(4..) })
/// );
/// ```
#[inline]
pub const fn tag<'t, S>(tag: &'t str) -> Tag<'t, S>
where
    S: Clone + Utf8Parsable,
{
    Tag { tag, _s: PhantomData }
}
