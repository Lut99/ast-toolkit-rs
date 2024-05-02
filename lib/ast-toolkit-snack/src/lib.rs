//  LIB.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:37:24
//  Last edited:
//    02 May 2024, 11:38:54
//  Auto updated?
//    Yes
//
//  Description:
//!   The sun is coming out, the birds are tweeting... It's that time of
//!   the year again! Lut99 makes another re-code of `nom`!
//!   
//!   Provides a parser-combinator framework heavily inspired by
//!   [nom](https://github.com/rust-bakery/nom), except that it gives up a
//!   little bit of performance over a more human-friendly debug experience.
//

// Declare submodules
pub mod branch;
pub mod bytes;
pub mod combinator;
pub mod error;
pub mod multi;
pub mod sequence;
pub mod span;
pub mod tuple;
pub mod utf8;

// Imports
use std::fmt::{Debug, Display, Formatter, Result as FResult};

use ast_toolkit_span::Span;
use enum_debug::EnumDebug;
use error::{Error, Failure};


/***** CONSTANTS *****/
/// Defines the indent size for every level in [`Expects`] formatting.
#[allow(unused)]
const EXPECTS_INDENT_SIZE: usize = 4;





/***** LIBRARY *****/
/// A trait that allows something (probably a combinator) to express what it expect(s|ed) as input.
///
/// This actually serves as an interface to an external type that does all the work. This is important in producing [`Failure`]s with lenient lifetimes.
pub trait Expects<'t> {
    /// The type of formatter that actually handles the work.
    type Formatter: 't + ExpectsFormatter;

    /// Returns an [`ExpectsFormatter`] that does the actual formatting.
    ///
    /// This [`Formatter`] may implement [`Display`] to show a fully-fledged error string.
    ///
    /// # Returns
    /// A [`Self::Formatter`](Expects::Formatter) that can be used to create the expects string.
    fn expects(&self) -> Self::Formatter;
}

/// A trait implemented by [`Expects::Formatter`]s.
///
/// This trait actually produces expect-strings.
pub trait ExpectsFormatter: Debug + Display {
    /// Formats the thing that this Expects expected as input.
    ///
    /// The string written should be something along the lines of filling in `XXX` in:
    /// ```plain
    /// Expected XXX.
    /// ```
    ///
    /// # Arguments
    /// - `f`: Some [`Formatter`] to write to.
    /// - `indent`: If this formatter writes newlines, they should be indented by this amount.
    ///
    /// # Errors
    /// This function should only error if it failed to write to the given `f`ormatter.
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult;
}



/// The trait that unifies ALL snack parser combinators.
pub trait Combinator<'t, F, S>: Expects<'t> {
    /// The output type for this Combinator.
    type Output;

    /// Runs the parser on the given input to produce something of output [`Self::Output`](Combinator::Output).
    ///
    /// # Arguments
    /// - `input`: The input to parse.
    ///
    /// # Returns
    /// A snack [`Result`] that encodes:
    /// - [`Result::Ok((rem, output))`]: We parsed an `output` of type `R`, with `rem` left unparsed.
    /// - [`Result::Fail(fail)`]: We failed to parse with reason `fail`, but another parser might still parse it.
    /// - [`Result::Error(err)`]: We failed to parse with reason `err` and we know the input is in an unrecoverable state (e.g., exhausted all branches).
    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S>;
}



/// Defines the shape of the output of [`Combinators`].
#[derive(Debug, EnumDebug)]
pub enum Result<'a, R, F, S> {
    /// An `output` of type `R` was parsed (1), with the remainder left unparsed (0).
    Ok(Span<F, S>, R),
    /// Failed to parse input with the given reason, but recoverably so.
    Fail(Failure<'a, F, S>),
    /// Failed to parse input with the given reason, but unrecoverably so.
    Error(Error<'a, F, S>),
}
impl<'a, R, F, S> Result<'a, R, F, S> {
    /// Maps this result's [`Result::Fail`]-case.
    ///
    /// # Arguments
    /// - `fail`: The [`Failure`] to return in case `self` is a [`Result::Fail`].
    ///
    /// # Returns
    /// The given `fail`ure if `self` is a [`Result::Fail`]. Else, `self` is returned as-is.
    #[inline]
    pub fn map_fail(self, map_fn: impl FnOnce(Failure<F, S>) -> Failure<F, S>) -> Self {
        match self {
            Self::Ok(rem, res) => Self::Ok(rem, res),
            Self::Fail(fail) => Self::Fail(map_fn(fail)),
            Self::Error(err) => Self::Error(err),
        }
    }

    /// Unwraps this Result as if it's a [`Result::Ok`].
    ///
    /// # Returns
    /// A tuple with a span describing the unparsed source, and the parsed object, respectively.
    ///
    /// # Panics
    /// This function panics if this Result is not [`Result::Ok`].
    #[inline]
    pub fn unwrap(self) -> (Span<F, S>, R) {
        if let Self::Ok(rem, res) = self {
            (rem, res)
        } else {
            panic!("Cannot unwrap a Result::{} as a Result::Ok", self.variant());
        }
    }

    /// Returns if this Result is [`Result::Ok`].
    ///
    /// # Returns
    /// True if so or false otherwise.
    #[inline]
    pub fn is_ok(&self) -> bool { matches!(self, Self::Ok(_, _)) }

    /// Returns if this Result is [`Result::Fail`].
    ///
    /// # Returns
    /// True if so or false otherwise.
    #[inline]
    pub fn is_fail(&self) -> bool { matches!(self, Self::Fail(_)) }

    /// Returns if this Result is [`Result::Error`].
    ///
    /// # Returns
    /// True if so or false otherwise.
    #[inline]
    pub fn is_err(&self) -> bool { matches!(self, Self::Error(_)) }

    /// Returns if this Result is either [`Result::Fail`] or [`Result::Error`].
    ///
    /// # Returns
    /// True if so or false otherwise.
    #[inline]
    pub fn is_not_ok(&self) -> bool { !self.is_ok() }
}
