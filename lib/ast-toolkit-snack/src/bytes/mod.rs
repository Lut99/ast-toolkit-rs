//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:37:49
//  Last edited:
//    03 May 2024, 13:32:13
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines value combinators that are matching raw byte sequences.
//

// Submodules
pub mod complete;
pub mod streaming;

use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

// Imports
use ast_toolkit_span::{Span, SpanRange};

use crate::span::{OneOfBytes, WhileBytes};
use crate::{Combinator, Expects, ExpectsFormatter, Result};


/***** LIBRARY FUNCTIONS *****/
/// Will attempt to match as many bytes from the start of a span as possible, as long as those bytes are in the set of to-be-searched-for bytes.
///
/// This version also accepts matching none of them. See [`one_of1()`] to match at least 1.
///
/// # Arguments
/// - `byteset`: A byte array(-like) that defines the set of characters we are looking for.
///
/// # Returns
/// A combinator [`OneOf0`] that will match the prefix of input as long as those bytes are in `byteset`.
///
/// # Fails
/// The returned combinator cannot fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::bytes::one_of0;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", b"abcdefg");
/// let span2 = Span::new("<example>", b"cdefghi");
/// let span3 = Span::new("<example>", b"hijklmn");
///
/// let mut comb = one_of0(b"abc");
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(3..), span1.slice(..3)));
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(1..), span2.slice(..1)));
/// assert_eq!(comb.parse(span3).unwrap(), (span3, Span::empty("<example>", b"hijklmn")));
/// ```
#[inline]
pub const fn one_of0<'b, F, S>(byteset: &'b [u8]) -> OneOf0<'b, F, S>
where
    F: Clone,
    S: Clone + OneOfBytes,
{
    OneOf0 { byteset, _f: PhantomData, _s: PhantomData }
}

/// Will attempt to match as many bytes from the start of a span as possible, as long as those bytes match a given predicate.
///
/// This version also accepts matching none of them. See [`complete::while1()`] or [`streaming::while1()`] to match at least 1.
///
/// # Arguments
/// - `predicate`: A closure that returns true for matching bytes, and false for non-matching bytes. All bytes that are matched are returned up to the first for which `predicate` returns false (if any).
///
/// # Returns
/// A combinator [`While0`] that will match the prefix of input as long as those bytes match the given `predicate`.
///
/// # Fails
/// The returned combinator cannot fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::bytes::while0;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", b"abcdefg");
/// let span2 = Span::new("<example>", b"cdefghi");
/// let span3 = Span::new("<example>", b"hijklmn");
///
/// let mut comb = while0(|b: u8| -> bool { b >= b'a' && b <= b'c' });
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(3..), span1.slice(..3)));
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(1..), span2.slice(..1)));
/// assert_eq!(comb.parse(span3).unwrap(), (span3, Span::empty("<example>", b"hijklmn")));
/// ```
#[inline]
pub const fn while0<F, S, P>(predicate: P) -> While0<F, S, P>
where
    F: Clone,
    S: Clone + WhileBytes,
    P: FnMut(u8) -> bool,
{
    While0 { predicate, _f: PhantomData, _s: PhantomData }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`OneOf0`] combinator.
#[derive(Debug)]
pub struct OneOf0Expects<'b> {
    /// The set of bytes we expect one of.
    byteset: &'b [u8],
}
impl<'b> Display for OneOf0Expects<'b> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'b> ExpectsFormatter for OneOf0Expects<'b> {
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

/// ExpectsFormatter for the [`While0`] combinator.
#[derive(Debug)]
pub struct While0Expects;
impl Display for While0Expects {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for While0Expects {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "specific bytes") }
}





/***** LIBRARY *****/
/// The combinator returned by [`one_of0()`].
pub struct OneOf0<'b, F, S> {
    /// The set of bytes to one of.
    byteset: &'b [u8],
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:      PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:      PhantomData<S>,
}
impl<'b, F, S> Expects<'b> for OneOf0<'b, F, S> {
    type Formatter = OneOf0Expects<'b>;

    #[inline]
    fn expects(&self) -> Self::Formatter { OneOf0Expects { byteset: self.byteset } }
}
impl<'b, F, S> Combinator<'b, F, S> for OneOf0<'b, F, S>
where
    F: Clone,
    S: Clone + OneOfBytes,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'b, Self::Output, F, S> {
        let match_point: usize = input.one_of_bytes(SpanRange::Open, self.byteset);
        Result::Ok(input.slice(match_point..), input.slice(..match_point))
    }
}

/// The combinator returned by [`while0()`].
pub struct While0<F, S, P> {
    /// The predicate used for matching.
    predicate: P,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f: PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<F, S, P> Expects<'static> for While0<F, S, P> {
    type Formatter = While0Expects;

    #[inline]
    fn expects(&self) -> Self::Formatter { While0Expects }
}
impl<F, S, P> Combinator<'static, F, S> for While0<F, S, P>
where
    F: Clone,
    S: Clone + WhileBytes,
    P: FnMut(u8) -> bool,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'static, Self::Output, F, S> {
        let match_point: usize = input.while_bytes(SpanRange::Open, &mut self.predicate);
        Result::Ok(input.slice(match_point..), input.slice(..match_point))
    }
}
