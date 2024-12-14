//  TAG.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 17:16:33
//  Last edited:
//    14 Dec 2024, 19:37:00
//  Auto updated?
//    Yes
//
//  Description:
//!   Implemens the [`tag()`]-combinator.
//

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::{Span, SpannableEq, Spanning};

use crate::result::SnackError;
use crate::span::MatchBytes;
use crate::{Combinator2, ExpectsFormatter};


/***** ERRORS *****/
// Recoverable error for the [`Tag`]-combinator.
pub struct TagRecoverable<'t, F, S> {
    /// What we expected
    pub tag:  &'t str,
    /// Where we expected it
    pub span: Span<F, S>,
}
// NOTE: We manually implement `Debug` to avoid an unnecessary `Debug`-bound on `F` and `S`
impl<'t, F, S> Debug for TagRecoverable<'t, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut fmt = f.debug_struct("TagRecoverable");
        fmt.field("tag", &self.tag);
        fmt.field("span", &self.span);
        fmt.finish()
    }
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
impl<'t, F, S: SpannableEq> Eq for TagRecoverable<'t, F, S> {}
impl<'t, F, S: SpannableEq> PartialEq for TagRecoverable<'t, F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.tag == other.tag && self.span == other.span }
}





/***** FORMATTERS *****/
/// Expects formatter for the [`Tag`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct TagExpectsFormatter<'t> {
    pub tag: &'t str,
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
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "{:?}", self.tag) }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`tag()`].
#[derive(Debug)]
pub struct Tag<'t, F, S> {
    tag: &'t str,
    _f:  PhantomData<F>,
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

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<(Span<F, S>, Self::Output), SnackError<F, S, Self::Recoverable, Self::Fatal>> {
        // See if we can parse the input
        let tag: &'t [u8] = self.tag.as_bytes();
        let match_point: usize = input.match_bytes(SpanRange::Open, tag);
        if match_point >= tag.len() {
            // Matched the entire tag
            #[cfg(debug_assertions)]
            assert!(match_point == tag.len());
            Ok((input.slice(match_point..), input.slice(..match_point)))
        } else {
            // Didn't match the entire tag
            Err(SnackError::Recoverable(TagRecoverable { tag: self.tag, span: input.start_onwards() }))
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
/// The returned combinator fails if the prefix of the input was not `tag`.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
/// let span3 = Span::new("<example>", "Hell");
///
/// let mut comb = tag("Hello");
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::TagRecoverable { tag: "Hello", span: span2.slice(0..) }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(tag::TagRecoverable { tag: "Hello", span: span3.slice(0..) }))
/// );
/// ```
#[inline]
pub const fn tag<'t, F, S>(tag: &'t str) -> Tag<'t, F, S>
where
    F: Clone,
    S: Clone + MatchBytes,
{
    Tag { tag, _f: PhantomData, _s: PhantomData }
}
