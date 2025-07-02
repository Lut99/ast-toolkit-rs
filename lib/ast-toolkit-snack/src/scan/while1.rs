//  WHILE 1.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:42:41
//  Last edited:
//    08 May 2025, 11:31:58
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`while1()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::Combinator;
use crate::result::{Expected, Result as SResult, SnackError};


/***** ERRORS *****/
/// Error thrown by the [`While1`]-combinator that encodes that not even one of the expected
/// bytes was parsed.
pub type Recoverable<'c, S> = Expected<ExpectsFormatter<'c>, S>;





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`While1`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'c> {
    /// Some description of what was expected.
    pub what: &'c str,
}
impl<'c> Display for ExpectsFormatter<'c> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl<'c> crate::ExpectsFormatter for ExpectsFormatter<'c> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one {}", self.what) }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`while1()`]-combinator.
pub struct While1<'c, P, S> {
    /// The predicate used for matching.
    predicate: P,
    /// A helper provided by the user to describe what is expected.
    what: &'c str,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'c`.
impl<'c, 's, 'a, P, S> Combinator<'a, 's, S> for While1<'c, P, S>
where
    'c: 'a,
    P: for<'b> FnMut(&'b S::Elem) -> bool,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<'c>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'c, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: self.what } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Simply iterate as long as we can
        let slice: &[S::Elem] = input.as_slice();
        let slice_len: usize = slice.len();
        let mut i: usize = 0;
        while i < slice_len && (self.predicate)(&slice[i]) {
            i += 1;
        }
        if i > 0 {
            Ok((input.slice(i..), input.slice(..i)))
        } else {
            Err(SnackError::Recoverable(Recoverable {
                fmt:     self.expects(),
                fixable: if input.is_empty() { Some(Some(1)) } else { None },
                span:    input,
            }))
        }
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many bytes from the start of a span as possible, as long as those
/// bytes match a given predicate.
///
/// This version does _not_ accept matching none of them. See [`while0()`](super::super::while0())
/// to also allow finding none.
///
/// # Arguments
/// - `what`: A short string describing what byte is being matched. Should finish the sentence
///   "Expected at least one ...".
/// - `predicate`: A closure that returns true for matching bytes, and false for non-matching
///   bytes. All bytes that are matched are returned up to the first for which
///   `predicate` returns false (if any).
///
/// # Returns
/// A combinator [`While1`] that will match the prefix of input as long as those bytes match
/// the given `predicate`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one byte. If this match failed
/// because end-of-input was reached, then this fails with a [`SnackError::NotEnough`] instead.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::while1;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("abcdefg");
/// let span2 = Span::new("cdefghi");
/// let span3 = Span::new("abÿcdef");
/// let span4 = Span::new("hijklmn");
/// let span5 = Span::new("");
///
/// let mut comb = while1("'a', 'b', 'c' or 'ÿ'", |b: &u8| -> bool {
///     *b == b'a' || *b == b'b' || *b == b'c' || *b == 191 || *b == 195
/// });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(while1::Recoverable {
///         fmt:     while1::ExpectsFormatter { what: "'a', 'b', 'c' or 'ÿ'" },
///         fixable: None,
///         span:    span4,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Recoverable(while1::Recoverable {
///         fmt:     while1::ExpectsFormatter { what: "'a', 'b', 'c' or 'ÿ'" },
///         fixable: Some(Some(1)),
///         span:    span5,
///     }))
/// );
/// ```
#[inline]
pub const fn while1<'c, 's, P, S>(what: &'c str, predicate: P) -> While1<'c, P, S>
where
    P: for<'a> FnMut(&'a S::Elem) -> bool,
    S: Clone + Spannable<'s>,
{
    While1 { predicate, what, _s: PhantomData }
}
