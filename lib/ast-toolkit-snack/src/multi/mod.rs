//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:46:57
//  Last edited:
//    05 Apr 2024, 17:53:55
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators that repeat other combinators multiple times. A
//!   _variable_ number of times.
//

// Submodules
pub mod complete;
pub mod streaming;

// Imports
#[cfg(feature = "punctuated")]
use ast_toolkit_punctuated::{Punctuated, PunctuatedTrailing};
use ast_toolkit_span::Span;

use crate::{Combinator, Result};


/***** LIBRARY *****/
/// Attempts to apply the given combinator as many times as possible.
///
/// All the parsed values are put into a [`Vec`].
///
/// Note that this combinator is OK with matching no input, and can therefore not fail.
/// If you want at least one, see [`many1()`] instead.
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `comb`inator as much as humanly possible.
pub fn many0<F: Clone, S: Clone, C: Combinator<F, S>>(mut comb: C) -> impl FnMut(Span<F, S>) -> Result<Vec<C::Output>, F, S> {
    move |input: Span<F, S>| -> Result<Vec<C::Output>, F, S> {
        let mut rem: Span<F, S> = input;
        let mut res: Vec<C::Output> = Vec::with_capacity(1);
        loop {
            match comb.parse(rem.clone()) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(new_res);
                },
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}

/// Attempts to apply the given combinator as many times as possible, but at least once.
///
/// All the parsed values are put into a [`Vec`].
///
/// Note that this combinator is NOT okay with matching no input, and therefore fails if nothing could be matched.
/// See [`many0()`] for a version that is more lenient in this.
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `comb`inator as much as humanly possible.
///
/// # Fails
/// This function returns the failure returned by `value` if not at least one of those was parsed.
pub fn many1<F: Clone, S: Clone, C: Combinator<F, S>>(mut comb: C) -> impl FnMut(Span<F, S>) -> Result<Vec<C::Output>, F, S> {
    move |input: Span<F, S>| -> Result<Vec<C::Output>, F, S> {
        // Run the combinator at least once
        let (mut rem, mut res): (Span<F, S>, Vec<C::Output>) = match comb.parse(input) {
            Result::Ok(rem, res) => (rem, vec![res]),
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Then run it as much as we can get away with
        loop {
            match comb.parse(rem.clone()) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(new_res);
                },
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}



/// Attempts to parse a list of things separated by other things, discarding the parsed other things as we go.
///
/// This is useful for parsing lists where we don't care about the separator.
///
/// All the parsed values are put into a [`Vec`].
///
/// Note that this combinator is OK with matching no input, and can therefore not fail.
/// If you want at least one, see [`separated_list1()`] instead.
///
/// # Arguments
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation, discarding them, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator as much as humanly possible.
///
/// Note that any trailing punctuation is not parsed by this combinator.
pub fn separated_list0<F: Clone, S: Clone, CV: Combinator<F, S>, CP: Combinator<F, S>>(
    mut value: CV,
    mut punct: CP,
) -> impl FnMut(Span<F, S>) -> Result<Vec<CV::Output>, F, S> {
    move |input: Span<F, S>| -> Result<Vec<CV::Output>, F, S> {
        // First parse a possible first value
        let (mut rem, mut res): (Span<F, S>, Vec<CV::Output>) = match value.parse(input.clone()) {
            Result::Ok(rem, res) => (rem, vec![res]),
            Result::Fail(_) => return Result::Ok(input, vec![]),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        loop {
            // Only parse punctuation after the first value has been parsed
            let punct_rem: Span<F, S> = match punct.parse(rem.clone()) {
                Result::Ok(rem, _) => rem,
                // If we fail, that's the end of the list
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match value.parse(punct_rem) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(new_res);
                },
                Result::Fail(_) => {
                    // We don't return the punctuated parsed but only the OG one to not consume it
                    return Result::Ok(rem, res);
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}

/// Attempts to parse a list of things separated by other things, discarding the parsed other things as we go.
///
/// This is useful for parsing lists where we don't care about the separator.
///
/// All the parsed values are put into a [`Vec`].
///
/// Note that this combinator is NOT okay with matching no input, and therefore fails if nothing could be matched.
/// See [`separated_list0()`] for a version that is more lenient in this.
///
/// # Arguments
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation, discarding them, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator as much as humanly possible.
///
/// Note that any trailing punctuation is not parsed by this combinator.
///
/// # Fails
/// This function returns the failure returned by `value` if not at least one of those was parsed.
pub fn separated_list1<F: Clone, S: Clone, CV: Combinator<F, S>, CP: Combinator<F, S>>(
    mut value: CV,
    mut punct: CP,
) -> impl FnMut(Span<F, S>) -> Result<Vec<CV::Output>, F, S> {
    move |input: Span<F, S>| -> Result<Vec<CV::Output>, F, S> {
        // First parse a possible first value
        let (mut rem, mut res): (Span<F, S>, Vec<CV::Output>) = match value.parse(input) {
            Result::Ok(rem, res) => (rem, vec![res]),
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        loop {
            // Only parse punctuation after the first value has been parsed
            let punct_rem: Span<F, S> = match punct.parse(rem.clone()) {
                Result::Ok(rem, _) => rem,
                // If we fail, that's the end of the list
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match value.parse(punct_rem) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(new_res);
                },
                Result::Fail(_) => {
                    // We don't return the punctuated parsed but only the OG one to not consume it
                    return Result::Ok(rem, res);
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}



/// Attempts to parse a list of things separated by other things, keeping the parsed other things as we go.
///
/// This is useful for parsing lists where we care about the separator.
///
/// All the parsed values are put into a [`Punctuated`].
///
/// Note that this combinator is OK with matching no input, and can therefore not fail.
/// If you want at least one, see [`punctuated1()`] instead.
///
/// # Arguments
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation,until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator as much as humanly possible.
///
/// Note that any trailing punctuation is not parsed by this combinator.
#[cfg(feature = "punctuated")]
pub fn punctuated0<F: Clone, S: Clone, CV: Combinator<F, S>, CP: Combinator<F, S>>(
    mut value: CV,
    mut punct: CP,
) -> impl FnMut(Span<F, S>) -> Result<Punctuated<CV::Output, CP::Output>, F, S> {
    move |input: Span<F, S>| -> Result<Punctuated<CV::Output, CP::Output>, F, S> {
        let mut res: Punctuated<CV::Output, CP::Output> = Punctuated::new();

        // First parse a possible first value
        let mut rem: Span<F, S> = match value.parse(input.clone()) {
            Result::Ok(new_rem, new_res) => {
                res.push_first(new_res);
                new_rem
            },
            Result::Fail(_) => return Result::Ok(input, res),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        loop {
            // Only parse punctuation after the first value has been parsed
            let (punct_rem, punct_res): (Span<F, S>, CP::Output) = match punct.parse(rem.clone()) {
                Result::Ok(rem, res) => (rem, res),
                // If we fail, that's the end of the list
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match value.parse(punct_rem) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(punct_res, new_res);
                },
                Result::Fail(_) => {
                    // We don't return the punctuated parsed but only the OG one to not consume it
                    return Result::Ok(rem, res);
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}

/// Attempts to parse a list of things separated by other things, keeping the parsed other things as we go.
///
/// This is useful for parsing lists where we care about the separator.
///
/// All the parsed values are put into a [`Punctuated`].
///
/// Note that this combinator is NOT okay with matching no input, and therefore fails if nothing could be matched.
/// See [`punctuated0()`] for a version that is more lenient in this.
///
/// # Arguments
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation,until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator as much as humanly possible.
///
/// Note that any trailing punctuation is not parsed by this combinator.
///
/// # Fails
/// This function returns the failure returned by `value` if not at least one of those was parsed.
#[cfg(feature = "punctuated")]
pub fn punctuated1<F: Clone, S: Clone, CV: Combinator<F, S>, CP: Combinator<F, S>>(
    mut value: CV,
    mut punct: CP,
) -> impl FnMut(Span<F, S>) -> Result<Punctuated<CV::Output, CP::Output>, F, S> {
    move |input: Span<F, S>| -> Result<Punctuated<CV::Output, CP::Output>, F, S> {
        let mut res: Punctuated<CV::Output, CP::Output> = Punctuated::new();

        // First parse a possible first value
        let mut rem: Span<F, S> = match value.parse(input) {
            Result::Ok(new_rem, new_res) => {
                res.push_first(new_res);
                new_rem
            },
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        loop {
            // Only parse punctuation after the first value has been parsed
            let (punct_rem, punct_res): (Span<F, S>, CP::Output) = match punct.parse(rem.clone()) {
                Result::Ok(rem, res) => (rem, res),
                // If we fail, that's the end of the list
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match value.parse(punct_rem) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(punct_res, new_res);
                },
                Result::Fail(_) => {
                    // We don't return the punctuated parsed but only the OG one to not consume it
                    return Result::Ok(rem, res);
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}



/// Attempts to parse a list of things separated by other things, keeping the parsed other things as we go.
///
/// This is useful for parsing lists where we care about the separator.
///
/// All the parsed values are put into a [`PunctuatedTrailing`].
///
/// Note that this combinator is OK with matching no input, and can therefore not fail.
/// If you want at least one, see [`punctuated_trailing1()`] instead.
///
/// # Arguments
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation,until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator as much as humanly possible.
///
/// Note that any trailing punctuation is parsed by this combinator and added to the returned result.
#[cfg(feature = "punctuated")]
pub fn punctuated_trailing0<F: Clone, S: Clone, CV: Combinator<F, S>, CP: Combinator<F, S>>(
    mut value: CV,
    mut punct: CP,
) -> impl FnMut(Span<F, S>) -> Result<PunctuatedTrailing<CV::Output, CP::Output>, F, S> {
    move |input: Span<F, S>| -> Result<PunctuatedTrailing<CV::Output, CP::Output>, F, S> {
        let mut res: PunctuatedTrailing<CV::Output, CP::Output> = PunctuatedTrailing::new();

        // First parse a possible first value
        let mut rem: Span<F, S> = match value.parse(input.clone()) {
            Result::Ok(new_rem, new_res) => {
                res.push_value(new_res);
                new_rem
            },
            Result::Fail(_) => return Result::Ok(input, res),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        loop {
            // Only parse punctuation after the first value has been parsed
            let punct_rem: Span<F, S> = match punct.parse(rem.clone()) {
                Result::Ok(rem, punct_res) => {
                    res.push_punct(punct_res);
                    rem
                },
                // If we fail, that's the end of the list
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match value.parse(punct_rem) {
                Result::Ok(value_rem, value_res) => {
                    rem = value_rem;
                    res.push_value(value_res);
                },
                Result::Fail(_) => {
                    // We don't return the punctuated parsed but only the OG one to not consume it
                    return Result::Ok(rem, res);
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}

/// Attempts to parse a list of things separated by other things, keeping the parsed other things as we go.
///
/// This is useful for parsing lists where we care about the separator.
///
/// All the parsed values are put into a [`PunctuatedTrailing`].
///
/// Note that this combinator is NOT okay with matching no input, and therefore fails if nothing could be matched.
/// See [`punctuated_trailing0()`] for a version that is more lenient in this.
///
/// # Arguments
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator as much as humanly possible.
///
/// Note that any trailing punctuation is parsed by this combinator and added to the returned result.
///
/// # Fails
/// This function returns the failure returned by `value` if not at least one of those was parsed.
#[cfg(feature = "punctuated")]
pub fn punctuated_trailing1<F: Clone, S: Clone, CV: Combinator<F, S>, CP: Combinator<F, S>>(
    mut value: CV,
    mut punct: CP,
) -> impl FnMut(Span<F, S>) -> Result<PunctuatedTrailing<CV::Output, CP::Output>, F, S> {
    move |input: Span<F, S>| -> Result<PunctuatedTrailing<CV::Output, CP::Output>, F, S> {
        let mut res: PunctuatedTrailing<CV::Output, CP::Output> = PunctuatedTrailing::new();

        // First parse a possible first value
        let mut rem: Span<F, S> = match value.parse(input) {
            Result::Ok(new_rem, new_res) => {
                res.push_value(new_res);
                new_rem
            },
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        loop {
            // Only parse punctuation after the first value has been parsed
            let punct_rem: Span<F, S> = match punct.parse(rem.clone()) {
                Result::Ok(rem, punct_res) => {
                    res.push_punct(punct_res);
                    rem
                },
                // If we fail, that's the end of the list
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match value.parse(punct_rem) {
                Result::Ok(value_rem, value_res) => {
                    rem = value_rem;
                    res.push_value(value_res);
                },
                Result::Fail(_) => {
                    // We don't return the punctuated parsed but only the OG one to not consume it
                    return Result::Ok(rem, res);
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}
