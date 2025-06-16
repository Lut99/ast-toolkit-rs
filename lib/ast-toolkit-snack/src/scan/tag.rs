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
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, Spanning};

use crate::fmt::ElemDisplay;
use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ParseError};


/***** ERRORS *****/
// Recoverable error for the [`Tag`]-combinator.
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'t, 's, T, S>, bound = (T: r#trait, S: Spannable<'s>))]
pub struct Recoverable<'t, T, S> {
    /// What we expected
    pub tag: &'t [T],
    /// Whether more might fix the error.
    ///
    /// If true, this means that `tag` matches whatever is still sliced by `span` - there just
    /// isn't enough of it.
    pub is_fixable: bool,
    /// Where we expected it
    pub span: Span<S>,
}
impl<'t, T: Debug + ElemDisplay, S> Display for Recoverable<'t, T, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult { write!(f, "{}", ExpectsFormatter { tag: self.tag }) }
}
impl<'t, 'a, T: Debug + ElemDisplay, S: Spannable<'a>> Error for Recoverable<'t, T, S> {}
impl<'t, T, S: Clone> Spanning<S> for Recoverable<'t, T, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}
impl<'s, 't, T: Debug + ElemDisplay, S: Clone + Spannable<'s>> ParseError<S> for Recoverable<'t, T, S> {
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
pub struct ExpectsFormatter<'t, T> {
    /// The tag of bytes we expect one of.
    pub tag: &'t [T],
}
impl<'t, T: Debug + ElemDisplay> Display for ExpectsFormatter<'t, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl<'t, T: Debug + ElemDisplay> crate::ExpectsFormatter for ExpectsFormatter<'t, T> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        for (i, elem) in self.tag.into_iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            elem.elem_fmt(f)?;
        }
        Ok(())
    }
}





/***** INTERFACES *****/
/// Abstracts over formatters for displaying `Tag`s.
pub trait TagFormatter {
    type Elem;

    /// Displays a tag of [`TagFormatter::Elem`]ents.
    ///
    /// # Arguments
    /// - `tag`: The tag to display.
    /// - `f`: A [`Formatter`] to write the display to.
    ///
    /// # Errors
    /// This function should only error when it failed to write to the given `f`ormatter.
    fn tag_fmt(&self, tag: &[Self::Elem], f: &mut Formatter) -> FResult;
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`tagf()`].
pub struct Tag<'c, T, S> {
    /// The actual tag that is being matched for.
    tag: &'c [T],
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:  PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'t`.
impl<'c, 's, 'a, S> Combinator<'a, 's, S> for Tag<'c, S::Elem, S>
where
    'c: 'a,
    S: Clone + Spannable<'s>,
    S::Elem: Debug + ElemDisplay + PartialEq,
{
    type ExpectsFormatter = ExpectsFormatter<'c, S::Elem>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'c, S::Elem, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { tag: self.tag } }

    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Match the tag
        let slice: &[S::Elem] = input.as_slice();
        let slice_len: usize = slice.len();
        let tag_len: usize = self.tag.len();
        let mut i: usize = 0;
        while i < std::cmp::min(slice_len, tag_len) && slice[i] == self.tag[i] {
            i += 1;
        }
        if i == tag_len {
            Ok((input.slice(i..), input.slice(..i)))
        } else {
            Err(SnackError::Recoverable(Recoverable { tag: self.tag, is_fixable: i == slice_len && slice_len < tag_len, span: input }))
        }
    }
}





/***** LIBRARY *****/
/// Matches a specific "tag", i.e., a specific sequence of elements.
///
/// Useful for matching keywords or punctuation in the input.
///
/// # Arguments
/// - `tag`: A sequence of elements that we expect at the head of the input.
///
/// # Returns
/// A combinator [`Tag`] that will match the prefix of input if it matches `tag`.
///
/// # Fails
/// The returned combinator fails if the prefix of the input was not `tag`.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_snack::{Combinator as _, ParseError as _};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
/// let span3 = Span::new("Hell");
/// let span4 = Span::new("abc");
///
/// let mut comb = tag(b"Hello");
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable {
///         tag: b"Hello",
///         is_fixable: false,
///         span: span2,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(tag::Recoverable {
///         tag: b"Hello",
///         is_fixable: true,
///         span: span3,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(tag::Recoverable {
///         tag: b"Hello",
///         is_fixable: false,
///         span: span4,
///     }))
/// );
/// ```
pub const fn tag<'t, 's, S>(tag: &'t [S::Elem]) -> Tag<'t, S::Elem, S>
where
    S: Clone + Spannable<'s>,
    S::Elem: Debug + ElemDisplay + PartialEq,
{
    Tag { tag, _s: PhantomData }
}
