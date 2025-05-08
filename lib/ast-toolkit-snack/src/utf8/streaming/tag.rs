//  TAG.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 17:16:33
//  Last edited:
//    08 May 2025, 14:40:39
//  Auto updated?
//    Yes
//
//  Description:
//!   Implemens the [`tag()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpannableBytes as _, SpannableUtf8};

pub use super::super::complete::tag::{ExpectsFormatter, Recoverable, tag as tag_complete};
use crate::Combinator;
use crate::result::{Result as SResult, SnackError};


/***** COMBINATORS *****/
/// Actual combinator implementing [`tag()`].
#[derive(Debug)]
pub struct Tag<'c, S> {
    tag: &'c str,
    _s:  PhantomData<S>,
}
impl<'c, 's, 'a, S> Combinator<'a, 's, S> for Tag<'c, S>
where
    'c: 'a,
    S: Clone + SpannableUtf8<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<'c>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'c, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { tag: self.tag } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Match the tag
        let tag: &[u8] = self.tag.as_bytes();
        let tag_len: usize = tag.len();
        let mut i: usize = 0;
        let split: usize = input.match_bytes_while(|b| {
            if i < tag_len && b == tag[i] {
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
/// let span4 = Span::new("abc");
/// let span5 = Span::new("");
///
/// let mut comb = tag("Hello");
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: "Hello", span: span2 }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::NotEnough { needed: Some(1), span: span3.slice(4..) })
/// );
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: "Hello", span: span4 }))
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::NotEnough { needed: Some(5), span: span5.slice(0..) })
/// );
/// ```
#[inline]
pub const fn tag<'c, 's, S>(tag: &'c str) -> Tag<'c, S>
where
    S: Clone + SpannableUtf8<'s>,
{
    Tag { tag, _s: PhantomData }
}





/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_utf8_streaming_tag() {
        let span = Span::new("hellohello");
        let (rem, _) = tag("hello").parse(span).unwrap_or_else(|err| panic!("Unexpected err: {err}"));
        let (rem, _) = tag("hello").parse(rem).unwrap_or_else(|err| panic!("Unexpected err: {err}"));
        assert_eq!(tag("hello").parse(rem), Err(SnackError::NotEnough { needed: Some(5), span: rem }));
    }
}
