//  COMPLETE.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:35:22
//  Last edited:
//    26 Apr 2024, 16:49:39
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

use crate::{Combinator, Expects, ExpectsExt as _, ExpectsFormatter, Result};


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::pair;
    use crate::error_new::{Common, Failure};
    use crate::utf8::complete::tag;
    use crate::{Combinator as _, Result};

    type Span = ast_toolkit_span::Span<&'static str, &'static str>;


    #[test]
    fn test_pair() {
        // Some success stories
        let input: Span = Span::new("<test>", "Hello, world!");
        let (rem, (res1, res2)) = pair(tag(&"Hello"), tag(&", world!"))(input).unwrap();
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
pub fn pair<F, S, C1: Combinator<F, S>, C2: Combinator<F, S>>(first: C1, second: C2) -> (C1, C2) { (first, second) }

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
pub fn tuple<F, S, C: Combinator<F, S>>(mut combs: C) -> impl FnMut(Span<F, S>) -> Result<C::Output, F, S> { move |input| combs.parse(input) }

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
pub fn terminated<F, S, C1: Combinator<F, S>, C2: Combinator<F, S>>(
    mut first: C1,
    mut sep: C2,
) -> impl FnMut(Span<F, S>) -> Result<C1::Output, F, S> {
    move |input: Span<F, S>| -> Result<C1::Output, F, S> {
        let (rem, res1) = match first.parse(input) {
            Result::Ok(rem, res) => (rem, res),
            Result::Fail(f) => return Result::Fail(f),
            Result::Error(f) => return Result::Error(f),
        };
        match sep.parse(rem) {
            Result::Ok(rem, _) => Result::Ok(rem, res1),
            Result::Fail(f) => Result::Fail(f),
            Result::Error(f) => Result::Error(f),
        }
    }
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
pub fn preceded<F, S, C1: Combinator<F, S>, C2: Combinator<F, S>>(mut sep: C1, mut second: C2) -> impl FnMut(Span<F, S>) -> Result<C2::Output, F, S> {
    move |input: Span<F, S>| -> Result<C2::Output, F, S> {
        let rem = match sep.parse(input) {
            Result::Ok(rem, _) => rem,
            Result::Fail(f) => return Result::Fail(f),
            Result::Error(f) => return Result::Error(f),
        };
        match second.parse(rem) {
            Result::Ok(rem, res2) => Result::Ok(rem, res2),
            Result::Fail(f) => Result::Fail(f),
            Result::Error(f) => Result::Error(f),
        }
    }
}

/// Parses two values separated by some combinator who's value we don't care about.
///
/// This is useful to parse some separated values.
///
/// # Arguments
/// - `first`: The first combinator to match.
/// - `sep`: The second combinator to match and then discard the value of.
/// - `second`: The third combinator to match.
///
/// # Returns
/// A tuple with the output of the `first` and `second` combinators, but only if all three match.
///
/// # Errors
/// This function will inherit errors of the input combinators, in-order.
pub fn separated_pair<F, S, C1: Combinator<F, S>, C2: Combinator<F, S>, C3: Combinator<F, S>>(
    mut first: C1,
    mut sep: C2,
    mut second: C3,
) -> impl FnMut(Span<F, S>) -> Result<(C1::Output, C3::Output), F, S> {
    move |input: Span<F, S>| -> Result<(C1::Output, C3::Output), F, S> {
        let (rem, res1) = match first.parse(input) {
            Result::Ok(rem, res1) => (rem, res1),
            Result::Fail(f) => return Result::Fail(f),
            Result::Error(f) => return Result::Error(f),
        };
        let rem = match sep.parse(rem) {
            Result::Ok(rem, _) => rem,
            Result::Fail(f) => return Result::Fail(f),
            Result::Error(f) => return Result::Error(f),
        };
        match second.parse(rem) {
            Result::Ok(rem, res3) => Result::Ok(rem, (res1, res3)),
            Result::Fail(f) => Result::Fail(f),
            Result::Error(f) => Result::Error(f),
        }
    }
}

/// Parses one value surrounded by a preceding and a terminating combinator we don't care about.
///
/// This is useful to parse some parenthesis, where we only care about the middle value but not anything else.
///
/// # Arguments
/// - `pre_sep`: The first combinator to match and then discard the value of.
/// - `first`: The second combinator to match.
/// - `aft_sep`: The third combinator to match and then discard the value of.
///
/// # Returns
/// The output of the `first` combinator, but only if all three match.
///
/// # Errors
/// This function will inherit errors of the input combinators, in-order.
pub fn delimited<F, S, C1: Combinator<F, S>, C2: Combinator<F, S>, C3: Combinator<F, S>>(
    mut pre_sep: C1,
    mut first: C2,
    mut aft_sep: C3,
) -> impl FnMut(Span<F, S>) -> Result<C2::Output, F, S> {
    move |input: Span<F, S>| -> Result<C2::Output, F, S> {
        let rem = match pre_sep.parse(input) {
            Result::Ok(rem, _) => rem,
            Result::Fail(f) => return Result::Fail(f),
            Result::Error(f) => return Result::Error(f),
        };
        let (rem, res2) = match first.parse(rem) {
            Result::Ok(rem, res2) => (rem, res2),
            Result::Fail(f) => return Result::Fail(f),
            Result::Error(f) => return Result::Error(f),
        };
        match aft_sep.parse(rem) {
            Result::Ok(rem, _) => Result::Ok(rem, res2),
            Result::Fail(f) => Result::Fail(f),
            Result::Error(f) => Result::Error(f),
        }
    }
}





/***** LIBRARY COMBINATORS *****/
