//  ONE OF 0.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:09:36
//  Last edited:
//    07 Mar 2025, 14:22:07
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`one_of0()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;
use ast_toolkit_span::range::SpanRange;

use crate::result::Result as SResult;
use crate::span::OneOfBytes;
use crate::{Combinator, ExpectsFormatter as _};


/***** FORMATTERS *****/
/// ExpectsFormatter for the [`OneOf0`] combinator.
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
        write!(f, "one of ")?;
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
/// Actually implements the [`one_of0()`]-combinator.
pub struct OneOf0<'b, F, S> {
    /// The set of bytes to one of.
    byteset: &'b [u8],
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:      PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:      PhantomData<S>,
}
impl<'b, F, S> Combinator<'static, F, S> for OneOf0<'b, F, S>
where
    F: Clone,
    S: Clone + OneOfBytes,
{
    type ExpectsFormatter = ExpectsFormatter<'b>;
    type Output = Span<F, S>;
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { byteset: self.byteset } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        let match_point: usize = input.one_of_bytes(SpanRange::Open, self.byteset);
        Ok((input.slice(match_point..), input.slice(..match_point)))
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many bytes from the start of a span as possible, as long as those
/// bytes are in the set of to-be-searched-for bytes.
///
/// This version accepts matching none of them. See [`one_of1()`](super::complete::one_of1()) (or
/// its streaming version, [`one_of1()`](super::streaming::one_of1())) to assert at least something
/// must be matched.
///
/// # Arguments
/// - `byteset`: A byte array(-like) that defines the set of bytes we are looking for.
///
/// # Returns
/// A combinator [`OneOf0`] that will match the prefix of input as long as those bytes are in
/// `byteset`.
///
/// # Fails
/// The returned combinator will never fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::bytes::one_of0;
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
/// let mut comb = one_of0(&[b'a', b'b', b'c', 191, 195]);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(comb.parse(span4), Ok((span4, span4.slice(..0))));
/// assert_eq!(comb.parse(span5), Ok((span5, span5.slice(..0))));
/// ```
#[inline]
pub const fn one_of0<'b, F, S>(byteset: &'b [u8]) -> OneOf0<'b, F, S>
where
    F: Clone,
    S: Clone + OneOfBytes,
{
    OneOf0 { byteset, _f: PhantomData, _s: PhantomData }
}
