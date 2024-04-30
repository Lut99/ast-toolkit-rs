//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:37:49
//  Last edited:
//    30 Apr 2024, 16:29:49
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
use crate::{Combinator, Expects, Result};


/***** EXPECTS FUNCTIONS *****/
/// Defines what we expect from a bytes-based [`OneOf0`].
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `bytes`: The set of bytes we're expecting.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
pub(crate) fn expects_one_of0_bytes(f: &mut Formatter, bytes: &[u8]) -> FResult {
    write!(f, "some of ")?;
    for i in 0..bytes.len() {
        if i == 0 {
            write!(f, "{:#04x?}", unsafe { bytes.get_unchecked(i) })?;
        } else if i < bytes.len() - 1 {
            write!(f, ", {:#04x?}", unsafe { bytes.get_unchecked(i) })?;
        } else {
            write!(f, " or {:#04x?}", unsafe { bytes.get_unchecked(i) })?;
        }
    }
    Ok(())
}

/// Defines what we expect from a bytes-based [`OneOf1`].
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `bytes`: The set of bytes we're expecting.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
pub(crate) fn expects_one_of1_bytes(f: &mut Formatter, bytes: &[u8]) -> FResult {
    write!(f, "at least one of ")?;
    for i in 0..bytes.len() {
        if i == 0 {
            write!(f, "{:#04x?}", unsafe { bytes.get_unchecked(i) })?;
        } else if i < bytes.len() - 1 {
            write!(f, ", {:#04x?}", unsafe { bytes.get_unchecked(i) })?;
        } else {
            write!(f, " or {:#04x?}", unsafe { bytes.get_unchecked(i) })?;
        }
    }
    Ok(())
}

/// Defines what we expect from a [`Tag`].
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `tag`: The thing that we are looking for.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[inline]
pub(crate) fn expects_tag_bytes(f: &mut Formatter, tag: &[u8]) -> FResult { write!(f, "{tag:#04X?}") }

/// Defines what we expect from a [`While0`].
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
pub(crate) fn expects_while0_bytes(f: &mut Formatter) -> FResult { write!(f, "specific bytes") }

/// Defines what we expect from a [`While1`].
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
pub(crate) fn expects_while1_bytes(f: &mut Formatter) -> FResult { write!(f, "at least one specific byte") }





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
impl<'b, F, S> Expects for OneOf0<'b, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { expects_one_of0_bytes(f, self.byteset) }
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
impl<F, S, P> Expects for While0<F, S, P> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { expects_while0_bytes(f) }
}
impl<'c, F, S, P> Combinator<'c, F, S> for While0<F, S, P>
where
    F: Clone,
    S: Clone + WhileBytes,
    P: FnMut(u8) -> bool,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        let match_point: usize = input.while_bytes(SpanRange::Open, &mut self.predicate);
        Result::Ok(input.slice(match_point..), input.slice(..match_point))
    }
}
