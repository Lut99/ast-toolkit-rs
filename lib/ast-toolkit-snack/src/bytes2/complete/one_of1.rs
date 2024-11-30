//  ONE OF 1.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:34:02
//  Last edited:
//    30 Nov 2024, 22:54:48
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`one_of1()`]-combinator.
//

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::{Span, SpannableEq, Spanning};

use crate::result::{Result as SResult, SnackError};
use crate::span::OneOfBytes;
use crate::{Combinator2, ExpectsFormatter};


/***** ERRORS *****/
/// Error thrown by the [`OneOf1`]-combinator that encodes that not even one of the expected
/// bytes was parsed.
pub struct OneOf1Recoverable<'b, F, S> {
    /// The set of bytes to one of.
    pub byteset: &'b [u8],
    /// The location where no characters were found.
    pub span:    Span<F, S>,
}
// NOTE: We manually implement `Debug` to avoid an unnecessary `Debug`-bound on `F` and `S`
impl<'b, F, S> Debug for OneOf1Recoverable<'b, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut fmt = f.debug_struct("OneOf1Recoverable");
        fmt.field("byteset", &self.byteset);
        fmt.field("span", &self.span);
        fmt.finish()
    }
}
impl<'b, F, S> Display for OneOf1Recoverable<'b, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", OneOf1ExpectsFormatter { byteset: self.byteset }) }
}
impl<'b, F, S> Error for OneOf1Recoverable<'b, F, S> {}
impl<'b, F: Clone, S: Clone> Spanning<F, S> for OneOf1Recoverable<'b, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.span.clone() }

    #[inline]
    fn into_span(self) -> Span<F, S> { self.span }
}
impl<'b, F, S: SpannableEq> Eq for OneOf1Recoverable<'b, F, S> {}
impl<'b, F, S: SpannableEq> PartialEq for OneOf1Recoverable<'b, F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.byteset == other.byteset && self.span == other.span }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`OneOf1`] combinator.
#[derive(Debug)]
pub struct OneOf1ExpectsFormatter<'b> {
    /// The set of bytes we expect one of.
    pub(crate) byteset: &'b [u8],
}
impl<'b> Display for OneOf1ExpectsFormatter<'b> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'b> ExpectsFormatter for OneOf1ExpectsFormatter<'b> {
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
pub struct OneOf1<'b, F, S> {
    /// The set of bytes to one of.
    byteset: &'b [u8],
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:      PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:      PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'b`.
impl<'c, 'b, F, S> Combinator2<'c, F, S> for OneOf1<'b, F, S>
where
    'b: 'c,
    F: Clone,
    S: Clone + OneOfBytes,
{
    type ExpectsFormatter = OneOf1ExpectsFormatter<'b>;
    type Output = Span<F, S>;
    type Recoverable = OneOf1Recoverable<'b, F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { OneOf1ExpectsFormatter { byteset: self.byteset } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        let match_point: usize = input.one_of_bytes(SpanRange::Open, self.byteset);
        if match_point > 0 {
            Ok((input.slice(match_point..), input.slice(..match_point)))
        } else {
            Err(SnackError::Recoverable(OneOf1Recoverable { byteset: self.byteset, span: input.start_onwards() }))
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
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::bytes2::complete::one_of1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::<&str, &[u8]>::new("<example>", b"abcdefg");
/// let span2 = Span::<&str, &[u8]>::new("<example>", b"cdefghi");
/// let span3 = Span::<&str, &[u8]>::new("<example>", "abÿcdef".as_bytes());
/// let span4 = Span::<&str, &[u8]>::new("<example>", b"hijklmn");
/// let span5 = Span::<&str, &[u8]>::new("<example>", b"");
///
/// // Note: the magic numbers below are the two bytes made up by "ÿ"
/// let mut comb = one_of1(&[b'a', b'b', b'c', 191, 195]);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(one_of1::OneOf1Recoverable {
///         byteset: &[b'a', b'b', b'c', 191, 195],
///         span:    span4,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Recoverable(one_of1::OneOf1Recoverable {
///         byteset: &[b'a', b'b', b'c', 191, 195],
///         span:    span5,
///     }))
/// );
/// ```
#[inline]
pub const fn one_of1<'b, F, S>(byteset: &'b [u8]) -> OneOf1<'b, F, S>
where
    F: Clone,
    S: Clone + OneOfBytes,
{
    OneOf1 { byteset, _f: PhantomData, _s: PhantomData }
}
