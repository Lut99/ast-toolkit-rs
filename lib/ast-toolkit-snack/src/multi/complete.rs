//  COMPLETE.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:47:21
//  Last edited:
//    05 Apr 2024, 18:00:50
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines complete versions of some `multi` combinators, which do not
//!   consider reaching EOF prematurely in any special way.
//

#[cfg(feature = "punctuated")]
use ast_toolkit_punctuated::{Punctuated, PunctuatedTrailing};
use ast_toolkit_span::Span;

use crate::fail::Failure;
use crate::{Combinator, Result};


/***** LIBRARY *****/
/// Attempts to apply the given combinator exactly `N` times.
///
/// All the parsed values are put into a [`Vec`] of `N` elements.
///
/// # Arguments
/// - `n`: The number of times to apply the combinator.
/// - `comb`: The combinator to apply `N` times.
///
/// # Returns
/// A new combinator that will apply the given `comb`inator `N` times.
///
/// # Fails
/// This function returns a [`Failure::ManyN`] if it failed to apply `comb` exactly `N` times.
pub fn many_n<F: Clone, S: Clone, C: Combinator<F, S>>(n: usize, mut comb: C) -> impl FnMut(Span<F, S>) -> Result<Vec<C::Output>, F, S> {
    move |input: Span<F, S>| -> Result<Vec<C::Output>, F, S> {
        let mut rem = input;
        let mut res = Vec::with_capacity(n);
        for i in 0..n {
            match comb.parse(rem.clone()) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(new_res);
                },
                Result::Fail(fail) => return Result::Fail(Failure::ManyN { times: n, got: i, fail: Box::new(fail) }),
                Result::Error(err) => return Result::Error(err),
            }
        }
        #[cfg(debug_assertions)]
        assert_eq!(res.len(), n);
        Result::Ok(rem, res)
    }
}

/// Attempts to apply the given combinator exactly `N` times, parsing separators in between.
///
/// All the parsed values are put into a [`Vec`] of `N` elements.
///
/// # Arguments
/// - `n`: The number of times to apply the `value` combinator.
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation, discarding them, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator `N` times.
///
/// Note that any trailing punctuation is not parsed by this list.
///
/// # Fails
/// This function returns a [`Failure::ManyN`] if it failed to apply `value` exactly `N` times.
pub fn separated_list_n<F: Clone, S: Clone, CV: Combinator<F, S>, CP: Combinator<F, S>>(
    n: usize,
    mut value: CV,
    mut punct: CP,
) -> impl FnMut(Span<F, S>) -> Result<Vec<CV::Output>, F, S> {
    move |input: Span<F, S>| -> Result<Vec<CV::Output>, F, S> {
        // Do nothing if n == 0
        if n == 0 {
            return Result::Ok(input, Vec::new());
        }

        // First parse a possible first value
        let (mut rem, mut res): (Span<F, S>, Vec<CV::Output>) = match value.parse(input.clone()) {
            Result::Ok(rem, res) => (rem, vec![res]),
            Result::Fail(fail) => return Result::Fail(Failure::SeparatedListNValue { times: n, got: 0, fail: Box::new(fail) }),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        for i in 1..n {
            // Only parse punctuation after the first value has been parsed
            let punct_rem: Span<F, S> = match punct.parse(rem.clone()) {
                Result::Ok(rem, _) => rem,
                // If we fail, that's the end of the list
                Result::Fail(fail) => return Result::Fail(Failure::SeparatedListNPunct { times: n, got: i, fail: Box::new(fail) }),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match value.parse(punct_rem) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(new_res);
                },
                Result::Fail(fail) => return Result::Fail(Failure::SeparatedListNValue { times: n, got: i, fail: Box::new(fail) }),
                Result::Error(err) => return Result::Error(err),
            }
        }

        // OK, we now made it this far
        #[cfg(debug_assertions)]
        assert_eq!(res.len(), n);
        Result::Ok(rem, res)
    }
}

/// Attempts to apply the given combinator exactly `N` times, parsing separators in between.
///
/// All the parsed values are put into a [`Punctuated`] of `N` elements.
///
/// # Arguments
/// - `n`: The number of times to apply the `value` combinator.
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator `N` times.
///
/// Note that any trailing punctuation is not parsed by this list.
///
/// # Fails
/// This function returns a [`Failure::PunctuatedN`] if it failed to apply `value` exactly `N` times.
pub fn punctuated_n<F: Clone, S: Clone, CV: Combinator<F, S>, CP: Combinator<F, S>>(
    n: usize,
    mut value: CV,
    mut punct: CP,
) -> impl FnMut(Span<F, S>) -> Result<Punctuated<CV::Output, CP::Output>, F, S> {
    move |input: Span<F, S>| -> Result<Punctuated<CV::Output, CP::Output>, F, S> {
        // Do nothing if n == 0
        if n == 0 {
            return Result::Ok(input, Punctuated::new());
        }

        // First parse a possible first value
        let mut res: Punctuated<CV::Output, CP::Output> = Punctuated::new();
        let mut rem: Span<F, S> = match value.parse(input) {
            Result::Ok(rem, value_res) => {
                res.push_first(value_res);
                rem
            },
            Result::Fail(fail) => return Result::Fail(Failure::PunctuatedNValue { times: n, got: 0, fail: Box::new(fail) }),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        for i in 1..n {
            // Only parse punctuation after the first value has been parsed
            let (punct_rem, punct_res): (Span<F, S>, CP::Output) = match punct.parse(rem.clone()) {
                Result::Ok(rem, res) => (rem, res),
                // If we fail, that's the end of the list
                Result::Fail(fail) => return Result::Fail(Failure::PunctuatedNPunct { times: n, got: i, fail: Box::new(fail) }),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match value.parse(punct_rem) {
                Result::Ok(value_rem, value_res) => {
                    rem = value_rem;
                    res.push(punct_res, value_res);
                },
                Result::Fail(fail) => return Result::Fail(Failure::PunctuatedNValue { times: n, got: i, fail: Box::new(fail) }),
                Result::Error(err) => return Result::Error(err),
            }
        }

        // OK, we now made it this far
        #[cfg(debug_assertions)]
        assert_eq!(res.len(), n);
        Result::Ok(rem, res)
    }
}

/// Attempts to apply the given combinator exactly `N` times, parsing separators in between.
///
/// All the parsed values are put into a [`PunctuatedTrailing`] of `N` elements.
///
/// # Arguments
/// - `n`: The number of times to apply the `value` combinator.
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator `N` times.
///
/// Note that any trailing punctuation is parsed by this combinator and added to the returned result.
///
/// # Fails
/// This function returns a [`Failure::PunctuatedTrailingN`] if it failed to apply `value` exactly `N` times.
pub fn punctuated_trailing_n<F: Clone, S: Clone, CV: Combinator<F, S>, CP: Combinator<F, S>>(
    n: usize,
    mut value: CV,
    mut punct: CP,
) -> impl FnMut(Span<F, S>) -> Result<PunctuatedTrailing<CV::Output, CP::Output>, F, S> {
    move |input: Span<F, S>| -> Result<PunctuatedTrailing<CV::Output, CP::Output>, F, S> {
        // Do nothing if n == 0
        if n == 0 {
            return Result::Ok(input, PunctuatedTrailing::new());
        }

        // First parse a possible first value
        let mut res: PunctuatedTrailing<CV::Output, CP::Output> = PunctuatedTrailing::new();
        let mut rem: Span<F, S> = match value.parse(input) {
            Result::Ok(rem, value_res) => {
                res.push_value(value_res);
                rem
            },
            Result::Fail(fail) => return Result::Fail(Failure::PunctuatedTrailingNValue { times: n, got: 0, fail: Box::new(fail) }),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        for i in 1..n {
            // Only parse punctuation after the first value has been parsed
            let punct_rem: Span<F, S> = match punct.parse(rem.clone()) {
                Result::Ok(rem, punct_res) => {
                    res.push_punct(punct_res);
                    rem
                },
                // If we fail, that's the end of the list
                Result::Fail(fail) => return Result::Fail(Failure::PunctuatedTrailingNPunct { times: n, got: i, fail: Box::new(fail) }),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match value.parse(punct_rem) {
                Result::Ok(value_rem, value_res) => {
                    rem = value_rem;
                    res.push_value(value_res);
                },
                Result::Fail(fail) => return Result::Fail(Failure::PunctuatedTrailingNValue { times: n, got: i, fail: Box::new(fail) }),
                Result::Error(err) => return Result::Error(err),
            }
        }

        // OK, we now made it this far
        #[cfg(debug_assertions)]
        assert_eq!(res.len(), n);
        Result::Ok(rem, res)
    }
}
