//  COMPLETE.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:40:42
//  Last edited:
//    06 May 2024, 16:33:43
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines some UTF-8 value parsers that are complete, i.e., they
//!   consider not enough input an actualy failure.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::{Span, Spanning as _};

use crate::error::{Common, Failure};
use crate::span::{MatchBytes, OneOfUtf8, WhileUtf8};
use crate::{Combinator, Expects, ExpectsFormatter, Result};


/***** LIBRARY FUNCTIONS *****/
/// Matches as many digits as possible.
///
/// This version also accepts matching none of them. See [`digit1()`] to match at least 1.
///
/// # Returns
/// A combinator [`Digit1`] that matches only digits 0-9.
///
/// # Fails
/// The returned combinator fails if it did not match at least one digit.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::utf8::complete::digit1;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "12345six");
/// let span2 = Span::new("<example>", "one23456");
///
/// let mut comb = digit1();
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::Digit1 { .. }))));
/// ```
#[inline]
pub const fn digit1<F, S>() -> Digit1<F, S>
where
    F: Clone,
    S: Clone + WhileUtf8,
{
    Digit1 { _f: PhantomData, _s: PhantomData }
}

/// Will attempt to match as many characters from the start of a span as possible, as long as those characters are in the set of to-be-searched-for characters.
///
/// This version does _not_ accept matching none of them. See [`one_of0()`](super::one_of0()) to also allow finding none.
///
/// # Arguments
/// - `charset`: A byte array(-like) that defines the set of characters we are looking for.
///
/// # Returns
/// A combinator [`OneOf1`] that will match the prefix of input as long as those characters are in `charset`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one character.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::utf8::complete::one_of1;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "abcdefg");
/// let span2 = Span::new("<example>", "cdefghi");
/// let span3 = Span::new("<example>", "hijklmn");
///
/// let mut comb = one_of1(&["a", "b", "c"]);
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(3..), span1.slice(..3)));
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(1..), span2.slice(..1)));
/// assert!(matches!(comb.parse(span3), SResult::Fail(Failure::Common(Common::OneOf1Utf8 { .. }))));
/// ```
#[inline]
pub const fn one_of1<'t, F, S>(charset: &'t [&'t str]) -> OneOf1<'t, F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    OneOf1 { charset, _f: PhantomData, _s: PhantomData }
}

/// Matches a specific "tag", i.e., a sequence of UTF-8 characters.
///
/// Useful for matching keywords.
///
/// # Arguments
/// - `tag`: The tag to match for.
///
/// # Returns
/// A combinator [`Tag`] that will match the prefix of input if it matches `tag`.
///
/// # Fails
/// The returned combinator fails if the prefix of the input was not `tag`.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = tag("Hello");
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::TagUtf8 { .. }))));
/// ```
pub const fn tag<'t, F, S>(tag: &'t str) -> Tag<'t, F, S>
where
    F: Clone,
    S: Clone + MatchBytes,
{
    Tag { tag, _f: PhantomData, _s: PhantomData }
}

/// Will attempt to match as many characters from the start of a span as possible, as long as those characters match a given predicate.
///
/// This version does _not_ accept matching none of them. See [`while0()`](super::while0()) to also allow finding none.
///
/// # Arguments
/// - `predicate`: A closure that returns true for matching characters, and false for non-matching characters. All characters that are matched are returned up to the first for which `predicate` returns false (if any).
///
/// # Returns
/// A combinator [`While1`] that will match the prefix of input as long as those characters match the given `predicate`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one character.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::utf8::complete::while1;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "abcdefg");
/// let span2 = Span::new("<example>", "cdefghi");
/// let span3 = Span::new("<example>", "hijklmn");
///
/// let mut comb = while1(|c: &str| -> bool {
///     if c.len() != 1 {
///         return false;
///     }
///     let c: char = c.chars().next().unwrap();
///     c >= 'a' && c <= 'c'
/// });
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(3..), span1.slice(..3)));
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(1..), span2.slice(..1)));
/// assert!(matches!(comb.parse(span3), SResult::Fail(Failure::Common(Common::While1Utf8 { .. }))));
/// ```
#[inline]
pub const fn while1<F, S, P>(predicate: P) -> While1<F, S, P>
where
    F: Clone,
    S: Clone + WhileUtf8,
    P: FnMut(&str) -> bool,
{
    While1 { predicate, _f: PhantomData, _s: PhantomData }
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
/// A combinator [`Whitespace1`] that matches only whitespace characters (see above).
///
/// # Fails
/// The returned combinator fails if it did not match at least one whitespace.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::utf8::complete::whitespace1;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "   \t\n  awesome");
/// let span2 = Span::new("<example>", "cool \n dope");
///
/// let mut comb = whitespace1();
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(7..), span1.slice(..7)));
/// assert!(matches!(
///     comb.parse(span2),
///     SResult::Fail(Failure::Common(Common::Whitespace1 { .. }))
/// ));
/// ```
#[inline]
pub const fn whitespace1<F, S>() -> Whitespace1<F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    Whitespace1 { _f: PhantomData, _s: PhantomData }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Digit1`] combinator.
#[derive(Debug)]
pub struct Digit1Expects;
impl Display for Digit1Expects {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for Digit1Expects {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one digit") }
}

/// ExpectsFormatter for the [`OneOf1`] combinator.
#[derive(Debug)]
pub struct OneOf1Expects<'c> {
    /// The set of bytes we expect one of.
    pub(crate) charset: &'c [&'c str],
}
impl<'c> Display for OneOf1Expects<'c> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'c> ExpectsFormatter for OneOf1Expects<'c> {
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        write!(f, "at least one of ")?;
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

/// ExpectsFormatter for the [`Tag`] combinator.
#[derive(Debug)]
pub struct TagExpects<'t> {
    /// The tag of characters we expect one of.
    pub(crate) tag: &'t str,
}
impl<'t> Display for TagExpects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for TagExpects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "{:?}", self.tag) }
}

/// ExpectsFormatter for the [`While1`] combinator.
#[derive(Debug)]
pub struct While1Expects;
impl Display for While1Expects {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for While1Expects {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one specific character") }
}

/// ExpectsFormatter for the [`Whitespace1`] combinator.
#[derive(Debug)]
pub struct Whitespace1Expects;
impl Display for Whitespace1Expects {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for Whitespace1Expects {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one whitespace") }
}





/***** LIBRARY *****/
/// The combinator returned by [`digits1()`].
pub struct Digit1<F, S> {
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f: PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<F, S> Expects<'static> for Digit1<F, S> {
    type Formatter = Digit1Expects;

    #[inline]
    fn expects(&self) -> Self::Formatter { Digit1Expects }
}
impl<F, S> Combinator<'static, F, S> for Digit1<F, S>
where
    F: Clone,
    S: Clone + WhileUtf8,
{
    type Output = Span<F, S>;
    type Error = Infallible;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'static, Self::Output, F, S, Self::Error> {
        let mut comb = While1 {
            predicate: |c: &str| -> bool {
                c.len() == 1 && {
                    let c: char = c.chars().next().unwrap();
                    c >= '0' && c <= '9'
                }
            },
            _f: PhantomData,
            _s: PhantomData,
        };
        match comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, res),
            Result::Fail(Failure::NotEnough { needed, span }) => Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(fail) => Result::Fail(Failure::Common(Common::Digit1 { span: fail.span() })),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// The combinator returned by [`one_of1()`].
pub struct OneOf1<'c, F, S> {
    /// The set of characters to one of.
    charset: &'c [&'c str],
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:      PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:      PhantomData<S>,
}
impl<'c, F, S> Expects<'c> for OneOf1<'c, F, S> {
    type Formatter = OneOf1Expects<'c>;

    #[inline]
    fn expects(&self) -> Self::Formatter { OneOf1Expects { charset: self.charset } }
}
impl<'c, F, S> Combinator<'c, F, S> for OneOf1<'c, F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type Output = Span<F, S>;
    type Error = Infallible;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S, Self::Error> {
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
    type Error = Infallible;

    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S, Self::Error> {
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
impl<F, S, P> Expects<'static> for While1<F, S, P> {
    type Formatter = While1Expects;

    #[inline]
    fn expects(&self) -> Self::Formatter { While1Expects }
}
impl<F, S, P> Combinator<'static, F, S> for While1<F, S, P>
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
impl<F, S> Expects<'static> for Whitespace1<F, S> {
    type Formatter = Whitespace1Expects;

    #[inline]
    fn expects(&self) -> Self::Formatter { Whitespace1Expects }
}
impl<F, S> Combinator<'static, F, S> for Whitespace1<F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type Output = Span<F, S>;
    type Error = Infallible;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'static, Self::Output, F, S, Self::Error> {
        let mut comb = OneOf1 { charset: &[" ", "\t", "\n", "\r", "\r\n"], _f: PhantomData, _s: PhantomData };
        match comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, res),
            Result::Fail(Failure::NotEnough { needed, span }) => Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(fail) => Result::Fail(Failure::Common(Common::Whitespace1 { span: fail.span() })),
            Result::Error(err) => Result::Error(err),
        }
    }
}
