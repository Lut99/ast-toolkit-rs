//  TAG STRING.rs
//    by Lut99
//
//  Description:
//!   Defines the [`tag_s()`]-combinator.
//

use std::borrow::Cow;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, SpannableBytes, Spanning, SpanningInf, SpanningMut, SpanningRef};

use crate::result::{Result as SResult, SnackError};
use crate::scan::tag;
use crate::{Combinator, ParseError};


/***** ERRORS *****/
// Recoverable error for the [`Tag`]-combinator.
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'t, 's, S>, bound = (S: Spannable<'s>))]
pub struct Recoverable<'t, S> {
    /// What we expected
    pub tag: &'t str,
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
impl<'t, 's, S: Spannable<'s>> Error for Recoverable<'t, S> {}
impl<'t, S: Clone> Spanning<S> for Recoverable<'t, S> {
    #[inline]
    fn get_span(&self) -> Option<Cow<'_, Span<S>>> { Some(Cow::Borrowed(&self.span)) }

    #[inline]
    fn take_span(self) -> Option<Span<S>> { Some(self.span) }
}
impl<'t, S: Clone> SpanningInf<S> for Recoverable<'t, S> {
    #[inline]
    fn span(&self) -> Cow<'_, Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}
impl<'t, S: Clone> SpanningRef<S> for Recoverable<'t, S> {
    #[inline]
    fn span_ref(&self) -> &Span<S> { &self.span }
}
impl<'t, S: Clone> SpanningMut<S> for Recoverable<'t, S> {
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> { &mut self.span }
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
/// ExpectsFormatter for the [`TagS`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'t> {
    /// The tag of bytes we expect one of.
    pub tag: &'t str,
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
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "{:?}", self.tag) }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`tag_s()`]-combinator.
pub struct TagS<'t, S> {
    /// The tag we're matching.
    tag: &'t str,
    _s:  PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'t`.
impl<'t, 's, 'a, S> Combinator<'a, 's, S> for TagS<'t, S>
where
    't: 'a,
    S: Clone + SpannableBytes<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<'t>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'t, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { tag: self.tag } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match tag(self.tag.as_bytes()).parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => {
                Err(SnackError::Recoverable(Recoverable { tag: self.tag, is_fixable: err.is_fixable, span: err.into_span() }))
            },
            Err(SnackError::Fatal(_)) => unreachable!(),
        }
    }
}





/***** LIBRARY *****/
/// Exactly the same as a [`tag()`](super::tag()), but hints that the tag can be formatted as
/// UTF-8.
///
/// As such, this only works over [byte spans](SpannableBytes).
///
/// # Arguments
/// - `tag`: A string that encodes bytes we expect at the start of the input.
///
/// # Returns
/// A combinator [`TagS`] that will match the prefix of input if it matches `tag`.
///
/// # Fails
/// The returned combinator fails recoverably if the prefix of the input was not `tag`. It never
/// fails fatally.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_snack::utf8::tag_s;
///
/// let mut comb: tag::Tag<_, &'static str> = tag(b"Hello");
/// let mut comb_s: tag_s::TagS<&'static str> = tag_s("Hello");
/// assert_eq!(comb.expects().to_string(), "Expected 48 65 6C 6C 6F");
/// assert_eq!(comb_s.expects().to_string(), "Expected \"Hello\"");
/// ```
pub const fn tag_s<'t, 's, S>(tag: &'t str) -> TagS<'t, S>
where
    S: Clone + SpannableBytes<'s>,
{
    TagS { tag, _s: PhantomData }
}





/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;


    macro_rules! assert_comb_eq {
        ($lhs:expr, $rhs:expr) => {{
            let lhs = $lhs;
            let rhs = $rhs;
            match (lhs, rhs) {
                (Ok(lhs), Ok(rhs)) if lhs == rhs => (),
                (
                    Err(SnackError::Recoverable(tag::Recoverable { tag: _, is_fixable: lhs_is_fixable, span: lhs_span })),
                    Err(SnackError::Recoverable(Recoverable { tag: _, is_fixable: rhs_is_fixable, span: rhs_span })),
                ) if lhs_is_fixable == rhs_is_fixable && lhs_span == rhs_span => (),
                (lhs, rhs) => panic!("Combinators do not agree on answer:\n    lhs: {lhs:?}\n    rhs: {rhs:?}"),
            }
        }};
    }


    #[test]
    fn test_tag_s() {
        let span1 = Span::new("Hello, world!");
        let span2 = Span::new("Goodbye, world!");
        let span3 = Span::new("Hell");
        let span4 = Span::new("abc");

        let mut gold = tag(b"Hello");
        let mut comb = tag_s("Hello");

        assert_comb_eq!(gold.parse(span1), comb.parse(span1));
        assert_comb_eq!(gold.parse(span2), comb.parse(span2));
        assert_comb_eq!(gold.parse(span3), comb.parse(span3));
        assert_comb_eq!(gold.parse(span4), comb.parse(span4));
    }
}
