//  ONE OF 1.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:34:02
//  Last edited:
//    18 Mar 2025, 10:23:58
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`one_of1()`]-combinator.
//

use std::borrow::Cow;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, Spanning};
use better_derive::{Debug, Eq, PartialEq};

use crate::result::{Result as SResult, SnackError};
use crate::span::BytesParsable;
use crate::{Combinator, ExpectsFormatter as _};


/***** ERRORS *****/
/// Error thrown by the [`OneOf1`]-combinator that encodes that not even one of the expected
/// bytes was parsed.
#[derive(Debug, Eq, PartialEq)]
pub struct Recoverable<'b, S> {
    /// The set of bytes to one of.
    pub byteset: &'b [u8],
    /// The location where no characters were found.
    pub span:    Span<S>,
}
impl<'b, S> Display for Recoverable<'b, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", ExpectsFormatter { byteset: self.byteset }) }
}
impl<'b, S: Spannable> Error for Recoverable<'b, S> {}
impl<'b, S: Clone> Spanning<S> for Recoverable<'b, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`OneOf1`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'b> {
    /// The set of bytes we expect one of.
    pub byteset: &'b [u8],
}
impl<'b> Display for ExpectsFormatter<'b> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'b> crate::ExpectsFormatter for ExpectsFormatter<'b> {
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        write!(f, "at least one of ")?;
        for i in 0..self.byteset.len() {
            if i == 0 {
                // SAFETY: Loops prevents us from going outside of byteset's length
                write!(f, "{:#04x?}", unsafe { self.byteset.get_unchecked(i) })?;
            } else if i < self.byteset.len() - 1 {
                // SAFETY: Loops prevents us from going outside of byteset's length
                write!(f, ", {:#04x?}", unsafe { self.byteset.get_unchecked(i) })?;
            } else {
                // SAFETY: Loops prevents us from going outside of byteset's length
                write!(f, " or {:#04x?}", unsafe { self.byteset.get_unchecked(i) })?;
            }
        }
        Ok(())
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`one_of1()`]-combinator.
pub struct OneOf1<'b, S> {
    /// The set of bytes to one of.
    byteset: &'b [u8],
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:      PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'b`.
impl<'c, 'b, S> Combinator<'c, S> for OneOf1<'b, S>
where
    'b: 'c,
    S: Clone + BytesParsable,
{
    type ExpectsFormatter = ExpectsFormatter<'b>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'b, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { byteset: self.byteset } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Try to iterate over the head to find the match
        let mut i: usize = 0;
        for byte in input.bytes() {
            // Check if it's in the set
            if self.byteset.contains(byte) {
                i += 1;
                continue;
            } else {
                break;
            }
        }

        // Return if there's at least one
        if i > 0 {
            Ok((input.slice(i..), input.slice(..i)))
        } else {
            Err(SnackError::Recoverable(Recoverable { byteset: self.byteset, span: input }))
        }
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many bytes from the start of a span as possible, as long as those
/// bytes are in the set of to-be-searched-for bytes.
///
/// This version does _not_ accept matching none of them. See [`one_of0()`](super::super::one_of0())
/// to also allow finding none.
///
/// # Arguments
/// - `byteset`: A byte array(-like) that defines the set of bytes we are looking for.
///
/// # Returns
/// A combinator [`OneOf1`] that will match the prefix of input as long as those bytes are in
/// `byteset`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one byte.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::bytes::complete::one_of1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new(b"abcdefg".as_slice());
/// let span2 = Span::new(b"cdefghi".as_slice());
/// let span3 = Span::new("abÿcdef".as_bytes());
/// let span4 = Span::new(b"hijklmn".as_slice());
/// let span5 = Span::new(b"".as_slice());
///
/// // Note: the magic numbers below are the two bytes made up by "ÿ"
/// let mut comb = one_of1(&[b'a', b'b', b'c', 191, 195]);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(one_of1::Recoverable {
///         byteset: &[b'a', b'b', b'c', 191, 195],
///         span:    span4,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Recoverable(one_of1::Recoverable {
///         byteset: &[b'a', b'b', b'c', 191, 195],
///         span:    span5,
///     }))
/// );
/// ```
#[inline]
pub const fn one_of1<'b, S>(byteset: &'b [u8]) -> OneOf1<'b, S>
where
    S: Clone + BytesParsable,
{
    OneOf1 { byteset, _s: PhantomData }
}
