//  TAG.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 17:16:33
//  Last edited:
//    20 Mar 2025, 15:55:15
//  Auto updated?
//    Yes
//
//  Description:
//!   Implemens the [`tag()`]-combinator.
//

use std::borrow::Cow;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, Spanning};
use better_derive::{Debug, Eq, PartialEq};
use unicode_segmentation::UnicodeSegmentation;

use crate::result::SnackError;
use crate::span::Utf8Parsable;
use crate::{Combinator, ExpectsFormatter as _};


/***** ERRORS *****/
// Recoverable error for the [`Tag`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct Recoverable<'t, S> {
    /// What we expected
    pub tag:  &'t str,
    /// Where we expected it
    pub span: Span<S>,
}
impl<'t, S> Display for Recoverable<'t, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { write!(f, "{}", ExpectsFormatter { tag: self.tag }) }
}
impl<'t, S: Spannable> Error for Recoverable<'t, S> {}
impl<'t, S: Clone> Spanning<S> for Recoverable<'t, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}





/***** FORMATTERS *****/
/// Expects formatter for the [`Tag`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'t> {
    pub tag: &'t str,
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
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "{:?}", self.tag) }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`tag()`].
#[derive(Debug)]
pub struct Tag<'t, S> {
    tag: &'t str,
    _s:  PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'t`.
impl<'c, 't, S> Combinator<'c, S> for Tag<'t, S>
where
    't: 'c,
    S: Clone + Utf8Parsable,
{
    type ExpectsFormatter = ExpectsFormatter<'t>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'t, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { tag: self.tag } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> Result<(Span<S>, Self::Output), SnackError<Self::Recoverable, Self::Fatal, S>> {
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
                Some(_) | None => {
                    // Note: required, or else Rust will think the iterator will be destructed at
                    // the end of the loop
                    drop(head);
                    return Err(SnackError::Recoverable(Recoverable { tag: self.tag, span: input }));
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
/// The returned combinator fails if the prefix of the input was not `tag`.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
/// let span3 = Span::new("Hell");
/// let span4 = Span::new("");
///
/// let mut comb = tag("Hello");
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: "Hello", span: span2.slice(..) }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: "Hello", span: span3.slice(..) }))
/// );
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: "Hello", span: span4.slice(..) }))
/// );
/// ```
#[inline]
pub const fn tag<'t, S>(tag: &'t str) -> Tag<'t, S>
where
    S: Clone + Utf8Parsable,
{
    Tag { tag, _s: PhantomData }
}
