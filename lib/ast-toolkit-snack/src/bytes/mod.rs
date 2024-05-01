//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:37:49
//  Last edited:
//    01 May 2024, 17:35:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines value combinators that are matching raw byte sequences.
//

// Submodules
pub mod complete;
pub mod streaming;

use std::fmt::{Formatter, Result as FResult};
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
/// A closure that will perform the actualy match for the given `byteset`. Note that this closure doesn't ever fail, because matching none is OK.
#[inline]
pub fn one_of0<'b, F, S>(byteset: &'b [u8]) -> OneOf0<'b, F, S>
where
    F: Clone,
    S: Clone + OneOfBytes,
{
    OneOf0 { byteset, _f: Default::default(), _s: Default::default() }
}

/// Will attempt to match as many bytes from the start of a span as possible, as long as those bytes match a given predicate.
///
/// This version also accepts matching none of them. See [`complete::while1()`] or [`streaming::while1()`] to match at least 1.
///
/// # Arguments
/// - `predicate`: A closure that returns true for matching bytes, and false for non-matching bytes. All bytes that are matched are returned up to the first for which `predicate` returns false (if any).
///
/// # Returns
/// A closure that will perform the actualy match for the given `predicate`.
#[inline]
pub fn while0<F, S, P>(predicate: P) -> While0<F, S, P>
where
    F: Clone,
    S: Clone + WhileBytes,
    P: FnMut(u8) -> bool,
{
    While0 { predicate, _f: Default::default(), _s: Default::default() }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`OneOf0`] combinator.
pub struct OneOf0Expects<'b> {
    /// The set of bytes we expect one of.
    byteset: &'b [u8],
}
impl<'b> ExpectsFormatter for OneOf0Expects<'b> {
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
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
pub struct While0Expects;
impl ExpectsFormatter for While0Expects {
    #[inline]
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "specific bytes") }
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
