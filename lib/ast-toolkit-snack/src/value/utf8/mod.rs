//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:37:29
//  Last edited:
//    05 Apr 2024, 13:40:20
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

// Imports
use ast_toolkit_span::{OneOfBytes, OneOfUtf8, Span};

use crate::Result;


/***** LIBRARY *****/
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
pub fn one_of0<'s, T, F, S>(charset: T) -> impl FnMut(Span<F, S>) -> Result<Span<F, S>, F, S>
where
    T: AsRef<[&'s str]>,
    F: Clone,
    S: Clone + OneOfUtf8,
{
    move |input: Span<F, S>| -> Result<Span<F, S>, F, S> {
        let match_point: usize = input.one_of_utf8(charset.as_ref());
        Result::Ok(input.slice(match_point..), input.slice(..match_point))
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
/// This function doesn't fail, as returning 0 whitespaces is OK too.
#[inline]
pub fn digit0<F, S>(input: Span<F, S>) -> Result<Span<F, S>, F, S>
where
    F: Clone,
    S: Clone + OneOfBytes,
{
    super::bytes::one_of0(b"0123456789")(input)
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
/// This function doesn't fail, as returning 0 whitespaces is OK too.
#[inline]
pub fn whitespace0<F, S>(input: Span<F, S>) -> Result<Span<F, S>, F, S>
where
    F: Clone,
    S: Clone + OneOfBytes,
{
    super::bytes::one_of0(b" \t\r\n")(input)
}
