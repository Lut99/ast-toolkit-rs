//  COMPLETE.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:40:42
//  Last edited:
//    24 Apr 2024, 15:14:06
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines some UTF-8 value parsers that are complete, i.e., they
//!   consider not enough input an actualy failure.
//

use std::fmt::{Debug, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpanRange, Spannable, Spanning as _};

use crate::error_new::{expects_digit1, expects_one_of1_utf8, expects_while1_utf8};
use crate::fail::{DebugUnicodeSegmentation, Failure};
use crate::span::{OneOfBytes, OneOfUtf8, WhileUtf8};
use crate::{Combinator, Expects, Result};


/***** LIBRARY FUNCTIONS *****/
/// Will attempt to match as many characters from the start of a span as possible, as long as those characters are in the set of to-be-searched-for characters.
///
/// This version does _not_ accept matching none of them. See [`one_of0()`](super::one_of0()) to also allow finding none.
///
/// # Arguments
/// - `charset`: A byte array(-like) that defines the set of characters we are looking for.
///
/// # Returns
/// A closure that will perform the actualy match for the given `charset`.
#[inline]
pub fn one_of1<F, S, T>(charset: &'static T) -> OneOf1<F, S, T>
where
    T: DebugUnicodeSegmentation,
    &'static T: AsRef<[&'static str]>,
    F: Clone,
    S: Clone + OneOfUtf8,
{
    OneOf1 { charset, _f: Default::default(), _s: Default::default() }
}

/// Will attempt to match as many characters from the start of a span as possible, as long as those characters match a given predicate.
///
/// This version does _not_ accept matching none of them. See [`while0()`](super::while0()) to also allow finding none.
///
/// # Arguments
/// - `predicate`: A closure that returns true for matching characters, and false for non-matching characters. All characters that are matched are returned up to the first for which `predicate` returns false (if any).
///
/// # Returns
/// A closure that will perform the actualy match for the given `predicate`.
#[inline]
pub fn while1<F, S, P>(predicate: P) -> While1<F, S, P>
where
    F: Clone,
    S: Clone + WhileUtf8,
    P: FnMut(&str) -> bool,
{
    While1 { predicate, _f: Default::default(), _s: Default::default() }
}

/// Matches as many digits as possible.
///
/// This version also accepts matching none of them. See [`digit1()`] to match at least 1.
///
/// # Arguments
/// - `input`: The input to attempt to parse whitespace from.
///
/// # Returns
/// A tuple with the remainder that we didn't parse, and a [`Span`] that spans the whitespaces at the start if any.
///
/// # Fails
/// This function fails if no digits were found at the start of `input`.
#[inline]
pub fn digit1<F, S>(input: Span<F, S>) -> Result<Span<F, S>, F, S>
where
    F: Clone,
    S: Clone + OneOfBytes + Spannable,
{
    super::super::bytes::complete::one_of1(b" \t\r\n")(input).map_fail(|f| Failure::Digit1 { span: f.span() })
}

/// Matches as many whitespace characters as possible.
///
/// Specifically, will match as many as possible from the following set of whitespaces:
/// - A space (` `);
/// - A tab (`\t`);
/// - A carriage return (`\r`); or
/// - A newline (`\n`).
///
/// This version does NOT accept matching none of them. See [`whitespace0()`] for a version that does.
///
/// # Arguments
/// - `input`: The input to attempt to parse whitespace from.
///
/// # Returns
/// A tuple with the remainder that we didn't parse, and a [`Span`] that spans the whitespaces at the start if any.
///
/// # Fails
/// This function fails if no whitespaces were found at the start of `input`.
#[inline]
pub fn whitespace1<F, S>(input: Span<F, S>) -> Result<Span<F, S>, F, S>
where
    F: Clone,
    S: Clone + OneOfBytes + Spannable,
{
    super::super::bytes::complete::one_of1(b" \t\r\n")(input).map_fail(|f| Failure::Whitespace1 { span: f.span() })
}




/***** LIBRARY *****/
/// The combinator returned by [`one_of1()`].
pub struct OneOf1<F, S, T: 'static> {
    /// The set of characters to one of.
    charset: &'static T,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:      PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:      PhantomData<S>,
}
impl<F, S, T: Debug> Expects for OneOf1<F, S, T> {
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { expects_one_of1_utf8(f, self.charset) }
}
impl<F, S, T> Combinator<F, S> for OneOf1<F, S, T>
where
    T: DebugUnicodeSegmentation,
    &'static T: AsRef<[&'static str]>,
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S> {
        let match_point: usize = input.one_of_utf8(SpanRange::Open, <&'static T as AsRef<[&'static str]>>::as_ref(&self.charset));
        if match_point > 0 {
            Result::Ok(input.slice(match_point..), input.slice(..match_point))
        } else {
            Result::Fail(Failure::OneOfUtf81 { charset: self.charset, span: input.start_onwards() })
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
impl<F, S, P> Expects for While1<F, S, P> {
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { expects_while1_utf8(f) }
}
impl<F, S, P> Combinator<F, S> for While1<F, S, P>
where
    F: Clone,
    S: Clone + WhileUtf8,
    P: FnMut(&str) -> bool,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S> {
        let match_point: usize = input.while_utf8(SpanRange::Open, self.predicate);
        if match_point > 0 {
            Result::Ok(input.slice(match_point..), input.slice(..match_point))
        } else {
            Result::Fail(Failure::While1Utf8 { span: input.start_onwards() })
        }
    }
}

/// The combinator returned by [`digits1()`].
pub struct Digit1<F, S> {
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f: PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<F, S> Expects for Digit1<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { expects_digit1(f) }
}
impl<F, S> Combinator<F, S> for Digit1<F, S> {
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S> {
        While1 {
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
