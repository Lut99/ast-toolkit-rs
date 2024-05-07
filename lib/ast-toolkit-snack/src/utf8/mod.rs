//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:37:29
//  Last edited:
//    07 May 2024, 09:44:31
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

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

// Imports
use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::Span;

use crate::span::{OneOfBytes, OneOfUtf8, WhileUtf8};
use crate::{Combinator, Expects, ExpectsFormatter, Result};


/***** LIBRARY FUNCTIONS *****/
/// Matches as many digits as possible.
///
/// This version also accepts matching none of them. See [`digit1()`] to match at least 1.
///
/// # Returns
/// A combinator [`Digit0`] that matches only digits 0-9.
///
/// # Fails
/// The returned combinator does not fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::utf8::digit0;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "12345six");
/// let span2 = Span::new("<example>", "one23456");
///
/// let mut comb = digit0();
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
/// assert_eq!(comb.parse(span2).unwrap(), (span2, Span::empty("<example>", "one23456")));
/// ```
#[inline]
pub const fn digit0<F, S>() -> Digit0<F, S>
where
    F: Clone,
    S: Clone + WhileUtf8,
{
    Digit0 { _f: PhantomData, _s: PhantomData }
}

/// Will attempt to match as many characters from the start of a span as possible, as long as those characters are in the set of to-be-searched-for characters.
///
/// This version also accepts matching none of them. See [`complete::one_of1()`] or [`streaming::one_of1()`] to match at least 1.
///
/// # Arguments
/// - `charset`: An array(-like) of graphemes that defines the set of characters we are looking for.
///
/// # Returns
/// A combinator [`OneOf0`] that will match the prefix of input as long as those characters are in `charset`.
///
/// # Fails
/// The returned combinator cannot fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::utf8::one_of0;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "abcdefg");
/// let span2 = Span::new("<example>", "cdefghi");
/// let span3 = Span::new("<example>", "hijklmn");
///
/// let mut comb = one_of0(&["a", "b", "c"]);
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(3..), span1.slice(..3)));
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(1..), span2.slice(..1)));
/// assert_eq!(comb.parse(span3).unwrap(), (span3, Span::empty("<example>", "hijklmn")));
/// ```
#[inline]
pub const fn one_of0<'c, F, S>(charset: &'c [&'c str]) -> OneOf0<'c, F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    OneOf0 { charset, _f: PhantomData, _s: PhantomData }
}

/// Will attempt to match as many characters from the start of a span as possible, as long as those characters match a given predicate.
///
/// This version also accepts matching none of them. See [`complete::while1()`] or [`streaming::while1()`] to match at least 1.
///
/// # Arguments
/// - `predicate`: A closure that returns true for matching characters, and false for non-matching characters. All characters that are matched are returned up to the first for which `predicate` returns false (if any).
///
/// # Returns
/// A combinator [`While0`] that will match the prefix of input as long as those characters match the given `predicate`.
///
/// # Fails
/// The returned combinator cannot fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::utf8::while0;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "abcdefg");
/// let span2 = Span::new("<example>", "cdefghi");
/// let span3 = Span::new("<example>", "hijklmn");
///
/// let mut comb = while0(|c: &str| -> bool {
///     if c.len() != 1 {
///         return false;
///     }
///     let c: char = c.chars().next().unwrap();
///     c >= 'a' && c <= 'c'
/// });
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(3..), span1.slice(..3)));
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(1..), span2.slice(..1)));
/// assert_eq!(comb.parse(span3).unwrap(), (span3, Span::empty("<example>", "hijklmn")));
/// ```
#[inline]
pub const fn while0<F, S, P>(predicate: P) -> While0<F, S, P>
where
    F: Clone,
    S: Clone + WhileUtf8,
    P: FnMut(&str) -> bool,
{
    While0 { predicate, _f: PhantomData, _s: PhantomData }
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
/// A combinator [`Whitespace0`] that matches only whitespace characters (see above).
///
/// # Fails
/// The returned combinator does not fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::utf8::whitespace0;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "   \t\n  awesome");
/// let span2 = Span::new("<example>", "cool \n dope");
///
/// let mut comb = whitespace0();
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(7..), span1.slice(..7)));
/// assert_eq!(comb.parse(span2).unwrap(), (span2, Span::empty("<example>", "cool \n dope")));
/// ```
#[inline]
pub const fn whitespace0<F, S>() -> Whitespace0<F, S>
where
    F: Clone,
    S: Clone + OneOfBytes,
{
    Whitespace0 { _f: PhantomData, _s: PhantomData }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Digit0`] combinator.
#[derive(Debug)]
pub struct Digit0Expects;
impl Display for Digit0Expects {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for Digit0Expects {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "digits") }
}

/// ExpectsFormatter for the [`OneOf0`] combinator.
#[derive(Debug)]
pub struct OneOf0Expects<'c> {
    /// The set of bytes we expect one of.
    charset: &'c [&'c str],
}
impl<'c> Display for OneOf0Expects<'c> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'c> ExpectsFormatter for OneOf0Expects<'c> {
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        write!(f, "one of ")?;
        for i in 0..self.charset.len() {
            if i == 0 {
                // SAFETY: Loops prevents us from going outside of charset's length
                write!(f, "{:?}", unsafe { self.charset.get_unchecked(i) })?;
            } else if i < self.charset.len() - 1 {
                // SAFETY: Loops prevents us from going outside of charset's length
                write!(f, ", {:?}", unsafe { self.charset.get_unchecked(i) })?;
            } else {
                // SAFETY: Loops prevents us from going outside of charset's length
                write!(f, " or {:?}", unsafe { self.charset.get_unchecked(i) })?;
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
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "specific characters") }
}

/// ExpectsFormatter for the [`Whitespace0`] combinator.
#[derive(Debug)]
pub struct Whitespace0Expects;
impl Display for Whitespace0Expects {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for Whitespace0Expects {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "whitespace") }
}





/***** LIBRARY *****/
/// The combinator returned by [`digit0()`].
pub struct Digit0<F, S> {
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f: PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<F, S> Expects<'static> for Digit0<F, S> {
    type Formatter = Digit0Expects;

    #[inline]
    fn expects(&self) -> Self::Formatter { Digit0Expects }
}
impl<F, S> Combinator<'static, F, S> for Digit0<F, S>
where
    F: Clone,
    S: Clone + WhileUtf8,
{
    type Output = Span<F, S>;
    type Error = Infallible;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'static, Self::Output, F, S, Self::Error> {
        While0 {
            predicate: |c: &str| -> bool {
                c.len() == 1 && {
                    let c: char = c.chars().next().unwrap();
                    c >= '0' && c <= '9'
                }
            },
            _f: PhantomData,
            _s: PhantomData,
        }
        .parse(input)
    }
}

/// The combinator returned by [`one_of0()`].
pub struct OneOf0<'c, F, S> {
    /// The set of characters to one of.
    charset: &'c [&'c str],
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:      PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:      PhantomData<S>,
}
impl<'e, 'c: 'e, F, S> Expects<'e> for OneOf0<'c, F, S> {
    type Formatter = OneOf0Expects<'c>;

    #[inline]
    fn expects(&self) -> Self::Formatter { OneOf0Expects { charset: self.charset } }
}
impl<'e, 'c: 'e, F, S> Combinator<'e, F, S> for OneOf0<'c, F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type Output = Span<F, S>;
    type Error = Infallible;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S, Self::Error> {
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
impl<F, S, P> Expects<'static> for While0<F, S, P> {
    type Formatter = While0Expects;

    #[inline]
    fn expects(&self) -> Self::Formatter { While0Expects }
}
impl<F, S, P> Combinator<'static, F, S> for While0<F, S, P>
where
    F: Clone,
    S: Clone + WhileUtf8,
    P: FnMut(&str) -> bool,
{
    type Output = Span<F, S>;
    type Error = Infallible;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'static, Self::Output, F, S, Self::Error> {
        let match_point: usize = input.while_utf8(SpanRange::Open, &mut self.predicate);
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
impl<F, S> Expects<'static> for Whitespace0<F, S> {
    type Formatter = Whitespace0Expects;

    #[inline]
    fn expects(&self) -> Self::Formatter { Whitespace0Expects }
}
impl<F, S> Combinator<'static, F, S> for Whitespace0<F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type Output = Span<F, S>;
    type Error = Infallible;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'static, Self::Output, F, S, Self::Error> {
        // Note: last '\r\n' is a unicode windows line end :)
        OneOf0 { charset: &[" ", "\t", "\n", "\r", "\r\n"], _f: PhantomData, _s: PhantomData }.parse(input)
    }
}
