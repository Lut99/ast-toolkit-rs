//  COMPLETE.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:35:22
//  Last edited:
//    30 Apr 2024, 16:32:05
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines 'complete' versions of the sequence combinators, which will
//!   consider not enough input a hard failure.
//

use std::fmt::{Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use crate::{Combinator, Expects, ExpectsFormatter, Result};


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::pair;
    use crate::error::{Common, Failure};
    use crate::utf8::complete::tag;
    use crate::{Combinator as _, Result};

    type Span = ast_toolkit_span::Span<&'static str, &'static str>;


    #[test]
    fn test_pair() {
        // Some success stories
        let input: Span = Span::new("<test>", "Hello, world!");
        let (rem, (res1, res2)) = pair(tag(&"Hello"), tag(&", world!")).parse(input).unwrap();
        println!("\"{}\"", rem.value());
        assert!(rem.is_empty());
        assert_eq!(res1, input.slice(0..5));
        assert_eq!(res2, input.slice(5..));

        // Failure
        assert!(matches!(tag(&"Goodbye").parse(input), Result::Fail(Failure::Common(Common::TagUtf8 { .. }))));
        assert!(matches!(tag(&"Ho").parse(input), Result::Fail(Failure::Common(Common::TagUtf8 { .. }))));
        assert!(matches!(tag(&"hello, world!").parse(input), Result::Fail(Failure::Common(Common::TagUtf8 { .. }))));
    }
}





/***** EXPECTS FUNCTIONS *****/
/// Defines what we expect from a [`Pair`].
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `expects1`: Some nested [`ExpectsFormatter`] of the first combinator that is being applied.
/// - `expects2`: Some nested [`ExpectsFormatter`] of the second combinator that is being applied.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[inline]
pub(crate) fn expects_pair(f: &mut Formatter, indent: usize, expects1: ExpectsFormatter, expects2: ExpectsFormatter) -> FResult {
    write!(f, "first ")?;
    <ExpectsFormatter as Expects>::fmt(&expects1, f, indent)?;
    write!(f, ", then ")?;
    <ExpectsFormatter as Expects>::fmt(&expects2, f, indent)
}





/***** LIBRARY FUNCTIONS *****/
/// Applies the first combinator, then applies the second.
///
/// # Arguments
/// - `first`: The first combinator to match.
/// - `second`: The second combinator to match.
///
/// # Returns
/// A tuple containing the outputs of both combinators.
///
/// # Errors
/// This function will inherit errors of the input combinators, in-order.
#[inline]
pub fn pair<'c, F, S, C1, C2>(first: C1, second: C2) -> Tuple<F, S, (C1, C2)>
where
    C1: Combinator<'c, F, S>,
    C2: Combinator<'c, F, S>,
{
    Tuple { tuple: (first, second), _f: Default::default(), _s: Default::default() }
}

/// Applies a tuple of combinators, in-order.
///
/// This combinator will try the given ones one-by-one. It either returns the first non-OK [`Result`], or a same-sized tuple of all the combinator's results.
///
/// # Note
/// Actually, this combinator is really a no-op, and simply returns the given combinator. This is possible because tuples simply implement [`Combinator`].  
/// Only tuples up to a size of 16 is implemented. For more, consider nesting tuples within each other.
///
/// # Arguments
/// - `combs`: The tuple of combinators to apply.
///
/// # Returns
/// A tuple containing the outputs of all combinators.
///
/// # Errors
/// This function will inherit errors of the input combinators, in-order.
#[inline]
pub fn tuple<'c, F, S, C>(combs: C) -> Tuple<F, S, C>
where
    C: Combinator<'c, F, S>,
{
    Tuple { tuple: combs, _f: Default::default(), _s: Default::default() }
}

/// Applies the first combinator, then applies the second and discards the result.
///
/// This is useful to parse trailing comma's, for example, where you don't care about the second but do need to parse it to enforce syntax.
///
/// # Arguments
/// - `first`: The first combinator to match.
/// - `sep`: The second combinator to match and then who's result to discard.
///
/// # Returns
/// The output of the first combinator, but only if both match.
///
/// # Errors
/// This function will inherit errors of the input combinators, in-order.
#[inline]
pub fn terminated<'c, F, S, C1, C2>(first: C1, sep: C2) -> Terminated<F, S, C1, C2>
where
    C1: Combinator<'c, F, S>,
    C2: Combinator<'c, F, S>,
{
    Terminated { first, sep, _f: Default::default(), _s: Default::default() }
}

/// Applies the first combinator and discards the result, then applies the second.
///
/// This is useful to parse preceding colons, for example, where you don't care about the first but do need to parse it to enforce syntax.
///
/// # Arguments
/// - `sep`: The first combinator to match and then who's result to discard.
/// - `second`: The second combinator to match.
///
/// # Returns
/// The output of the second combinator, but only if both match.
///
/// # Errors
/// This function will inherit errors of the input combinators, in-order.
#[inline]
pub fn preceded<'c, F, S, C1, C2>(sep: C1, second: C2) -> Preceded<F, S, C1, C2>
where
    C1: Combinator<'c, F, S>,
    C2: Combinator<'c, F, S>,
{
    Preceded { sep, second, _f: Default::default(), _s: Default::default() }
}

/// Parses two values separated by some combinator who's value we don't care about.
///
/// This is useful to parse some separated values.
///
/// # Arguments
/// - `first`: The first combinator to match.
/// - `sep`: The second combinator to match and then discard the value of.
/// - `third`: The third combinator to match.
///
/// # Returns
/// A tuple with the output of the `first` and `second` combinators, but only if all three match.
///
/// # Errors
/// This function will inherit errors of the input combinators, in-order.
#[inline]
pub fn separated_pair<'c, F, S, C1, C2, C3>(first: C1, sep: C2, third: C3) -> SeparatedPair<F, S, C1, C2, C3>
where
    C1: Combinator<'c, F, S>,
    C2: Combinator<'c, F, S>,
    C3: Combinator<'c, F, S>,
{
    SeparatedPair { first, sep, third, _f: Default::default(), _s: Default::default() }
}

/// Parses one value surrounded by a preceding and a terminating combinator we don't care about.
///
/// This is useful to parse some parenthesis, where we only care about the middle value but not anything else.
///
/// # Arguments
/// - `sep1`: The first combinator to match and then discard the value of.
/// - `second`: The second combinator to match.
/// - `sep3`: The third combinator to match and then discard the value of.
///
/// # Returns
/// The output of the `second` combinator, but only if all three match.
///
/// # Errors
/// This function will inherit errors of the input combinators, in-order.
#[inline]
pub fn delimited<'c, F, S, C1, C2, C3>(sep1: C1, second: C2, sep3: C3) -> Delimited<F, S, C1, C2, C3>
where
    C1: Combinator<'c, F, S>,
    C2: Combinator<'c, F, S>,
    C3: Combinator<'c, F, S>,
{
    Delimited { sep1, second, sep3, _f: Default::default(), _s: Default::default() }
}





/***** LIBRARY COMBINATORS *****/
/// Combinator returned by [`pair()`] and [`tuple()`].
pub struct Tuple<F, S, T> {
    /// The tuple combinator to wrap.
    tuple: T,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:    PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:    PhantomData<S>,
}
impl<F, S, T: Expects> Expects for Tuple<F, S, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult { self.tuple.fmt(f, indent) }
}
impl<'c, F, S, T: Combinator<'c, F, S>> Combinator<'c, F, S> for Tuple<F, S, T> {
    type Output = T::Output;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> { self.tuple.parse(input) }
}

/// Combinator returned by [`terminated()`].
pub struct Terminated<F, S, C1, C2> {
    /// The first combinator, which we parse.
    first: C1,
    /// The second combinator, which we parse but discard.
    sep:   C2,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:    PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:    PhantomData<S>,
}
impl<F, S, C1: Expects, C2: Expects> Expects for Terminated<F, S, C1, C2> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        self.first.fmt(f, indent)?;
        write!(f, ", then ")?;
        self.sep.fmt(f, indent)
    }
}
impl<'c, F, S, C1: Combinator<'c, F, S>, C2: Combinator<'c, F, S>> Combinator<'c, F, S> for Terminated<F, S, C1, C2> {
    type Output = C1::Output;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // Parse the first first
        let (rem, res): (Span<F, S>, C1::Output) = match self.first.parse(input) {
            Result::Ok(rem, res) => (rem, res),
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Parse the second
        match self.sep.parse(rem) {
            Result::Ok(rem, _) => Result::Ok(rem, res),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// Combinator returned by [`preceded()`].
pub struct Preceded<F, S, C1, C2> {
    /// The first combinator, which we parse but discard.
    sep:    C1,
    /// The second combinator, which we parse.
    second: C2,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:     PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:     PhantomData<S>,
}
impl<F, S, C1: Expects, C2: Expects> Expects for Preceded<F, S, C1, C2> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        self.sep.fmt(f, indent)?;
        write!(f, ", then ")?;
        self.second.fmt(f, indent)
    }
}
impl<'c, F, S, C1: Combinator<'c, F, S>, C2: Combinator<'c, F, S>> Combinator<'c, F, S> for Preceded<F, S, C1, C2> {
    type Output = C2::Output;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // Parse the first first
        let rem: Span<F, S> = match self.sep.parse(input) {
            Result::Ok(rem, _) => rem,
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Parse the second
        match self.second.parse(rem) {
            Result::Ok(rem, res) => Result::Ok(rem, res),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// Combinator returned by [`separated_pair()`].
pub struct SeparatedPair<F, S, C1, C2, C3> {
    /// The first combinator, which we parse.
    first: C1,
    /// The second combinator, which we parse but discard.
    sep:   C2,
    /// The third combinator, which we parse.
    third: C3,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:    PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:    PhantomData<S>,
}
impl<F, S, C1: Expects, C2: Expects, C3: Expects> Expects for SeparatedPair<F, S, C1, C2, C3> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        self.first.fmt(f, indent)?;
        write!(f, ", ")?;
        self.sep.fmt(f, indent)?;
        write!(f, ", then ")?;
        self.third.fmt(f, indent)
    }
}
impl<'c, F, S, C1: Combinator<'c, F, S>, C2: Combinator<'c, F, S>, C3: Combinator<'c, F, S>> Combinator<'c, F, S>
    for SeparatedPair<F, S, C1, C2, C3>
{
    type Output = (C1::Output, C3::Output);

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // Parse the first first
        let (rem, res1): (Span<F, S>, C1::Output) = match self.first.parse(input) {
            Result::Ok(rem, res) => (rem, res),
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Parse the second, discarding the result
        let rem: Span<F, S> = match self.sep.parse(rem) {
            Result::Ok(rem, _) => rem,
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Finally, parse the third
        match self.third.parse(rem) {
            Result::Ok(rem, res3) => Result::Ok(rem, (res1, res3)),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// Combinator returned by [`delimited()`].
pub struct Delimited<F, S, C1, C2, C3> {
    /// The first combinator, which we parse but discard.
    sep1:   C1,
    /// The second combinator, which we parse.
    second: C2,
    /// The third combinator, which we parse but discard.
    sep3:   C3,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:     PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:     PhantomData<S>,
}
impl<F, S, C1: Expects, C2: Expects, C3: Expects> Expects for Delimited<F, S, C1, C2, C3> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        self.sep1.fmt(f, indent)?;
        write!(f, ", ")?;
        self.second.fmt(f, indent)?;
        write!(f, ", then ")?;
        self.sep3.fmt(f, indent)
    }
}
impl<'c, F, S, C1: Combinator<'c, F, S>, C2: Combinator<'c, F, S>, C3: Combinator<'c, F, S>> Combinator<'c, F, S> for Delimited<F, S, C1, C2, C3> {
    type Output = C2::Output;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // Parse the first first, discarding the result
        let rem: Span<F, S> = match self.sep1.parse(input) {
            Result::Ok(rem, _) => rem,
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Parse the second
        let (rem, res): (Span<F, S>, C2::Output) = match self.second.parse(rem) {
            Result::Ok(rem, res) => (rem, res),
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Finally, parse the third and discard its result also
        match self.sep3.parse(rem) {
            Result::Ok(rem, _) => Result::Ok(rem, res),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}
