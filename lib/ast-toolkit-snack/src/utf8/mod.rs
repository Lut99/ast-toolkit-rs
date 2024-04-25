//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:37:29
//  Last edited:
//    25 Apr 2024, 18:06:46
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines value combinators that are matching UTF-8 sequences.
//!   
//!   Note this doesn't necessarily mean they are matching on _strings_. can
//!   also recognize (some) UTF-8 sequences in possible-UTF8 byte input.
//

// Submodules
pub mod complete;
pub mod streaming;

use std::fmt::{Formatter, Result as FResult};
use std::marker::PhantomData;

// Imports
use ast_toolkit_span::{Span, SpanRange};

use crate::span::{OneOfBytes, OneOfUtf8, WhileUtf8};
use crate::{Combinator, Expects, Result};


/***** LIBRARY FUNCTIONS *****/
/// Matches as many digits as possible.
///
/// This version also accepts matching none of them. See [`digit1()`] to match at least 1.
///
/// # Returns
/// A combinator that implements the actual operation.
#[inline]
pub fn digit0<F, S>(input: Span<F, S>) -> Digit0<F, S>
where
    F: Clone,
    S: Clone + WhileUtf8,
{
    Digit0 { _f: Default::default(), _s: Default::default() }
}

/// Will attempt to match as many characters from the start of a span as possible, as long as those characters are in the set of to-be-searched-for characters.
///
/// This version also accepts matching none of them. See [`complete::one_of1()`] or [`streaming::one_of1()`] to match at least 1.
///
/// # Arguments
/// - `charset`: An array(-like) of graphemes that defines the set of characters we are looking for.
///
/// # Returns
/// A closure that will perform the actualy match for the given `charset`. Note that this closure doesn't ever fail, because matching none is OK.
#[inline]
pub fn one_of0<'t, F, S>(charset: &'t [&'t str]) -> OneOf0<'t, F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    OneOf0 { charset, _f: Default::default(), _s: Default::default() }
}

/// Will attempt to match as many characters from the start of a span as possible, as long as those characters match a given predicate.
///
/// This version also accepts matching none of them. See [`complete::while1()`] or [`streaming::while1()`] to match at least 1.
///
/// # Arguments
/// - `predicate`: A closure that returns true for matching characters, and false for non-matching characters. All characters that are matched are returned up to the first for which `predicate` returns false (if any).
///
/// # Returns
/// A closure that will perform the actualy match for the given `predicate`.
#[inline]
pub fn while0<F, S, P>(predicate: P) -> While0<F, S, P>
where
    F: Clone,
    S: Clone + WhileUtf8,
    P: FnMut(&str) -> bool,
{
    While0 { predicate, _f: Default::default(), _s: Default::default() }
}

/// Matches as many whitespace characters as possible.
///
/// Specifically, will match as many as possible from the following set of whitespaces:
/// - A space (` `);
/// - A tab (`\t`);
/// - A carriage return (`\r`); or
/// - A newline (`\n`).
///
/// This version also accepts matching none of them. See [`complete::whitespace1()`] or [`streaming::whitespace1()`] to match at least 1.
///
/// # Returns
/// A combinator that implements the actual operation.
#[inline]
pub fn whitespace0<F, S>() -> Whitespace0<F, S>
where
    F: Clone,
    S: Clone + OneOfBytes,
{
    Whitespace0 { _f: Default::default(), _s: Default::default() }
}





/***** LIBRARY *****/
/// The combinator returned by [`digit0()`].
pub struct Digit0<F, S> {
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f: PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<F, S> Expects for Digit0<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "digits") }
}
impl<F, S> Combinator<F, S> for Digit0<F, S>
where
    F: Clone,
    S: Clone + WhileUtf8,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S> {
        While0 {
            predicate: |c: &str| -> bool {
                c.len() == 1 && {
                    let c: char = c.chars().next().unwrap();
                    c >= '0' && c <= '9'
                }
            },
            _f: Default::default(),
            _s: Default::default(),
        }
        .parse(input)
    }
}

/// The combinator returned by [`one_of0()`].
pub struct OneOf0<'t, F, S> {
    /// The set of characters to one of.
    charset: &'t [&'t str],
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:      PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:      PhantomData<S>,
}
impl<'t, F, S> Expects for OneOf0<'t, F, S> {
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        write!(f, "either ")?;
        for i in 0..self.charset.len() {
            if i == 0 {
                write!(f, "{:?}", unsafe { self.charset.get_unchecked(i) })?;
            } else if i < self.charset.len() - 1 {
                write!(f, ", {:?}", unsafe { self.charset.get_unchecked(i) })?;
            } else {
                write!(f, " or {:?}", unsafe { self.charset.get_unchecked(i) })?;
            }
        }
        Ok(())
    }
}
impl<'t, F, S> Combinator<F, S> for OneOf0<'t, F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S> {
        let match_point: usize = input.one_of_utf8(SpanRange::Open, self.charset);
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
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "specific characters") }
}
impl<F, S, P> Combinator<F, S> for While0<F, S, P>
where
    F: Clone,
    S: Clone + WhileUtf8,
    P: FnMut(&str) -> bool,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S> {
        let match_point: usize = input.while_utf8(SpanRange::Open, self.predicate);
        Result::Ok(input.slice(match_point..), input.slice(..match_point))
    }
}

/// The combinator returned by [`whitespace0()`].
pub struct Whitespace0<F, S> {
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f: PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<F, S> Expects for Whitespace0<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "whitespace") }
}
impl<F, S> Combinator<F, S> for Whitespace0<F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S> {
        // Note: last '\r\n' is a unicode windows line end :)
        OneOf0 { charset: &[" ", "\t", "\n", "\r", "\r\n"], _f: Default::default(), _s: Default::default() }.parse(input)
    }
}
