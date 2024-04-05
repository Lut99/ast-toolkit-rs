//  COMPLETE.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:40:42
//  Last edited:
//    05 Apr 2024, 13:41:44
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines some UTF-8 value parsers that are complete, i.e., they
//!   consider not enough input an actualy failure.
//

use ast_toolkit_span::{OneOfBytes, OneOfUtf8, Span};

use crate::fail::{DebugAsRef, Failure};
use crate::Result;


/***** LIBRARY *****/
/// Will attempt to match as many characters from the start of a span as possible, as long as those characters are in the set of to-be-searched-for characters.
///
/// This version does _not_ accept matching none of them. See [`one_of0()`] to also allow finding none.
///
/// # Arguments
/// - `charset`: A byte array(-like) that defines the set of characters we are looking for.
///
/// # Returns
/// A closure that will perform the actualy match for the given `charset`.
#[inline]
pub fn one_of1<T, F, S>(charset: &'static T) -> impl FnMut(Span<F, S>) -> Result<Span<F, S>, F, S>
where
    T: DebugAsRef,
    &'static T: AsRef<[&'static str]>,
    F: Clone,
    S: Clone + OneOfUtf8,
{
    move |input: Span<F, S>| -> Result<Span<F, S>, F, S> {
        let match_point: usize = input.one_of_utf8(<&'static T as AsRef<[&'static str]>>::as_ref(&charset));
        if match_point > 0 {
            Result::Ok(input.slice(match_point..), input.slice(..match_point))
        } else {
            Result::Fail(Failure::OneOfUtf81 { charset })
        }
    }
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
    S: Clone + OneOfBytes,
{
    super::super::bytes::complete::one_of1(b" \t\r\n")(input).map_fail(|_| Failure::Digit1)
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
    S: Clone + OneOfBytes,
{
    super::super::bytes::complete::one_of1(b" \t\r\n")(input).map_fail(|_| Failure::Whitespace1)
}
