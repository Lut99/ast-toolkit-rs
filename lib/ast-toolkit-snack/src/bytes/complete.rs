//  COMPLETE.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:43:32
//  Last edited:
//    01 May 2024, 17:47:55
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines some raw byte-matching value combinators that are complete,
//!   i.e., they consider not enough input a typical [`Failure`].
//

use std::fmt::{Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpanRange};

use crate::error::{Common, Failure};
use crate::span::{MatchBytes, OneOfBytes, WhileBytes};
use crate::{Combinator, Expects, ExpectsFormatter, Result};


/***** LIBRARY FUNCTIONS *****/
/// Will attempt to match as many bytes from the start of a span as possible, as long as those bytes are in the set of to-be-searched-for bytes.
///
/// This version does _not_ accept matching none of them. See [`one_of0()`] to also allow finding none.
///
/// # Arguments
/// - `byteset`: A byte array(-like) that defines the set of characters we are looking for.
///
/// # Returns
/// A closure that will perform the actualy match for the given `byteset`.
#[inline]
pub fn one_of1<'b, F, S>(byteset: &'b [u8]) -> OneOf1<'b, F, S>
where
    F: Clone,
    S: Clone + OneOfBytes,
{
    OneOf1 { byteset, _f: Default::default(), _s: Default::default() }
}

/// Matches a specific "tag", i.e., a sequence of bytes.
///
/// Useful for matching keywords.
///
/// # Arguments
/// - `tag`: The tag to match for.
///
/// # Returns
/// A [`Combinator`] that matches the given `tag`.
pub fn tag<'t, F, S>(tag: &'t [u8]) -> Tag<'t, F, S>
where
    F: Clone,
    S: Clone + MatchBytes,
{
    Tag { tag, _f: PhantomData::default(), _s: PhantomData::default() }
}

/// Will attempt to match as many bytes from the start of a span as possible, as long as those bytes match a given predicate.
///
/// This version does _not_ accept matching none of them. See [`while0()`](super::while0()) to also allow finding none.
///
/// # Arguments
/// - `predicate`: A closure that returns true for matching bytes, and false for non-matching bytes. All bytes that are matched are returned up to the first for which `predicate` returns false (if any).
///
/// # Returns
/// A closure that will perform the actualy match for the given `predicate`.
#[inline]
pub fn while1<F, S, P>(predicate: P) -> While1<F, S, P>
where
    F: Clone,
    S: Clone + WhileBytes,
    P: FnMut(u8) -> bool,
{
    While1 { predicate, _f: Default::default(), _s: Default::default() }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`OneOf1`] combinator.
#[derive(Debug)]
pub struct OneOf1Expects<'b> {
    /// The set of bytes we expect one of.
    byteset: &'b [u8],
}
impl<'b> ExpectsFormatter for OneOf1Expects<'b> {
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

/// ExpectsFormatter for the [`Tag`] combinator.
#[derive(Debug)]
pub struct TagExpects<'t> {
    /// The tag of bytes we expect one of.
    tag: &'t [u8],
}
impl<'t> ExpectsFormatter for TagExpects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "{:#04X?}", self.tag) }
}

/// ExpectsFormatter for the [`While1`] combinator.
#[derive(Debug)]
pub struct While1Expects;
impl ExpectsFormatter for While1Expects {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one specific byte") }
}





/***** LIBRARY *****/
/// The combinator returned by [`one_of1()`].
pub struct OneOf1<'b, F, S> {
    /// The set of bytes to one of.
    byteset: &'b [u8],
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:      PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:      PhantomData<S>,
}
impl<'b, F, S> Expects<'b> for OneOf1<'b, F, S> {
    type Formatter = OneOf1Expects<'b>;

    #[inline]
    fn expects(&self) -> Self::Formatter { OneOf1Expects { byteset: self.byteset } }
}
impl<'b, F, S> Combinator<'b, F, S> for OneOf1<'b, F, S>
where
    F: Clone,
    S: Clone + OneOfBytes,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'b, Self::Output, F, S> {
        let match_point: usize = input.one_of_bytes(SpanRange::Open, self.byteset);
        if match_point > 0 {
            Result::Ok(input.slice(match_point..), input.slice(..match_point))
        } else {
            Result::Fail(Failure::Common(Common::OneOf1Bytes { byteset: self.byteset, span: input.start_onwards() }))
        }
    }
}

/// The concrete combinator returned by `tag()`.
pub struct Tag<'t, F, S> {
    /// The actual tag that is being matched for.
    tag: &'t [u8],
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:  PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:  PhantomData<S>,
}
impl<'t, F, S> Expects<'t> for Tag<'t, F, S> {
    type Formatter = TagExpects<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter { TagExpects { tag: self.tag } }
}
impl<'t, F, S> Combinator<'t, F, S> for Tag<'t, F, S>
where
    F: Clone,
    S: Clone + MatchBytes,
{
    type Output = Span<F, S>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S> {
        // See if we can parse the input
        let match_point: usize = input.match_bytes(SpanRange::Open, self.tag);
        if match_point >= self.tag.len() {
            // Matched the entire tag
            #[cfg(debug_assertions)]
            assert!(match_point == self.tag.len());
            Result::Ok(input.slice(match_point..), input.slice(..match_point))
        } else {
            // Didn't match the entire tag
            Result::Fail(Failure::Common(Common::TagBytes { tag: self.tag, span: input.start_onwards() }))
        }
    }
}

/// The combinator returned by [`while1()`].
pub struct While1<F, S, P> {
    /// The predicate used for matching.
    predicate: P,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f: PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<F, S, P> Expects<'static> for While1<F, S, P> {
    type Formatter = While1Expects;

    #[inline]
    fn expects(&self) -> Self::Formatter { While1Expects }
}
impl<'c, F, S, P> Combinator<'static, F, S> for While1<F, S, P>
where
    F: Clone,
    S: Clone + WhileBytes,
    P: FnMut(u8) -> bool,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'static, Self::Output, F, S> {
        let match_point: usize = input.while_bytes(SpanRange::Open, &mut self.predicate);
        if match_point > 0 {
            Result::Ok(input.slice(match_point..), input.slice(..match_point))
        } else {
            Result::Fail(Failure::Common(Common::While1Utf8 { span: input.start_onwards() }))
        }
    }
}
