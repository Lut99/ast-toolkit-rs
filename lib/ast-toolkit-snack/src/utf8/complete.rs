//  COMPLETE.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:40:42
//  Last edited:
//    30 Apr 2024, 16:33:36
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines some UTF-8 value parsers that are complete, i.e., they
//!   consider not enough input an actualy failure.
//

use std::fmt::{Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpanRange};

use super::{expects_digit1, expects_one_of1_utf8, expects_tag_utf8, expects_while1_utf8, expects_whitespace1};
use crate::error::{Common, Failure};
use crate::span::{MatchBytes, OneOfUtf8, WhileUtf8};
use crate::{Combinator, Expects, Result};


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::tag;
    use crate::error::{Common, Failure};
    use crate::{Combinator as _, Result};

    type Span = ast_toolkit_span::Span<&'static str, &'static str>;


    #[test]
    fn test_tag() {
        // Some success stories
        let input: Span = Span::new("<test>", "Hello, world!");
        let (rem, res) = tag(&"Hello").parse(input).unwrap();
        assert_eq!(rem, input.slice(5..));
        assert_eq!(res, input.slice(..5));
        let (rem, res) = tag(&", ").parse(rem).unwrap();
        assert_eq!(rem, input.slice(7..));
        assert_eq!(res, input.slice(5..7));
        let (rem, res) = tag(&"world!").parse(rem).unwrap();
        assert_eq!(rem, input.slice(13..));
        assert_eq!(res, input.slice(7..13));

        // Failure
        assert!(matches!(tag(&"Goodbye").parse(input), Result::Fail(Failure::Common(Common::TagUtf8 { .. }))));
        assert!(matches!(tag(&"Ho").parse(input), Result::Fail(Failure::Common(Common::TagUtf8 { .. }))));
        assert!(matches!(tag(&"hello, world!").parse(input), Result::Fail(Failure::Common(Common::TagUtf8 { .. }))));
    }
}





/***** LIBRARY FUNCTIONS *****/
/// Matches as many digits as possible.
///
/// This version also accepts matching none of them. See [`digit1()`] to match at least 1.
///
/// # Returns
/// A combinator that implements the actual operation.
#[inline]
pub fn digit1<F, S>() -> Digit1<F, S>
where
    F: Clone,
    S: Clone + WhileUtf8,
{
    Digit1 { _f: Default::default(), _s: Default::default() }
}

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
pub fn one_of1<'t, F, S, T>(charset: &'t [&'t str]) -> OneOf1<'t, F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    OneOf1 { charset, _f: Default::default(), _s: Default::default() }
}

/// Matches a specific "tag", i.e., a sequence of UTF-8 characters.
///
/// Useful for matching keywords.
///
/// # Arguments
/// - `tag`: The tag to match for.
///
/// # Returns
/// A [`Combinator`] that matches the given `tag`.
pub fn tag<'t, F, S>(tag: &'t str) -> Tag<'t, F, S>
where
    F: Clone,
    S: Clone + MatchBytes,
{
    Tag { tag, _f: PhantomData::default(), _s: PhantomData::default() }
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
/// # Returns
/// A combinator that implements the actual operation.
#[inline]
pub fn whitespace1<F, S>() -> Whitespace1<F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    Whitespace1 { _f: Default::default(), _s: Default::default() }
}




/***** LIBRARY *****/
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
impl<F, S> Combinator<'static, F, S> for Digit1<F, S>
where
    F: Clone,
    S: Clone + WhileUtf8,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'static, Self::Output, F, S> {
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

/// The combinator returned by [`one_of1()`].
pub struct OneOf1<'t, F, S> {
    /// The set of characters to one of.
    charset: &'t [&'t str],
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:      PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:      PhantomData<S>,
}
impl<'t, F, S> Expects for OneOf1<'t, F, S> {
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { expects_one_of1_utf8(f, self.charset) }
}
impl<'t, F, S> Combinator<'t, F, S> for OneOf1<'t, F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S> {
        let match_point: usize = input.one_of_utf8(SpanRange::Open, self.charset);
        if match_point > 0 {
            Result::Ok(input.slice(match_point..), input.slice(..match_point))
        } else {
            Result::Fail(Failure::Common(Common::OneOf1Utf8 { charset: self.charset, span: input.start_onwards() }))
        }
    }
}

/// The concrete combinator returned by `tag()`.
pub struct Tag<'t, F, S> {
    /// The actual tag that is being matched for.
    tag: &'t str,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:  PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:  PhantomData<S>,
}
impl<'t, F, S> Expects for Tag<'t, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { expects_tag_utf8(f, self.tag) }
}
impl<'t, F, S> Combinator<'t, F, S> for Tag<'t, F, S>
where
    F: Clone,
    S: Clone + MatchBytes,
{
    type Output = Span<F, S>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S> {
        // See if we can parse the input
        let tag: &'t [u8] = self.tag.as_bytes();
        let match_point: usize = input.match_bytes(SpanRange::Open, tag);
        if match_point >= tag.len() {
            // Matched the entire tag
            #[cfg(debug_assertions)]
            assert!(match_point == tag.len());
            Result::Ok(input.slice(match_point..), input.slice(..match_point))
        } else {
            // Didn't match the entire tag
            Result::Fail(Failure::Common(Common::TagUtf8 { tag: self.tag, span: input.start_onwards() }))
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
impl<F, S, P> Combinator<'static, F, S> for While1<F, S, P>
where
    F: Clone,
    S: Clone + WhileUtf8,
    P: FnMut(&str) -> bool,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'static, Self::Output, F, S> {
        let match_point: usize = input.while_utf8(SpanRange::Open, &mut self.predicate);
        if match_point > 0 {
            Result::Ok(input.slice(match_point..), input.slice(..match_point))
        } else {
            Result::Fail(Failure::Common(Common::While1Utf8 { span: input.start_onwards() }))
        }
    }
}

/// The combinator returned by [`whitespace1()`].
pub struct Whitespace1<F, S> {
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f: PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<F, S> Expects for Whitespace1<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { expects_whitespace1(f) }
}
impl<F, S> Combinator<'static, F, S> for Whitespace1<F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type Output = Span<F, S>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'static, Self::Output, F, S> {
        // Note: last '\r\n' is a unicode windows line end :)
        OneOf1 { charset: &[" ", "\t", "\n", "\r", "\r\n"], _f: Default::default(), _s: Default::default() }.parse(input)
    }
}
