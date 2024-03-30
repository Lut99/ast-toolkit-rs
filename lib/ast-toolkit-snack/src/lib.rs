//  LIB.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:37:24
//  Last edited:
//    30 Mar 2024, 12:25:33
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
pub mod complete;
pub mod error;
pub mod fail;
pub mod streaming;

// Imports
use ast_toolkit_span::{Span, Spannable};
use enum_debug::EnumDebug;


/***** LIBRARY *****/
/// The trait that unifies ALL (...output) snack parser combinators.
pub trait Combinator<F, S> {
    /// The output type for this Combinator.
    type Output;

    /// Runs the parser on the given input to produce something of output [`Self::Output`](Combinator::Output).
    ///
    /// # Arguments
    /// - `input`: The input to parse.
    ///
    /// # Returns
    /// A snack [`Result`] that encodes:
    /// - [`SnackResult::Ok((rem, output))`]: We parsed an `output` of type `R`, with `rem` left unparsed.
    /// - [`SnackResult::Fail(fail)`]: We failed to parse with reason `fail`, but another parser might still parse it.
    /// - [`SnackResult::Error(err)`]: We failed to parse with reason `err` and we know the input is in an unrecoverable state (e.g., exhausted all branches).
    fn parse(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S>;
}

// Default impls
impl<R, F, S, T: FnMut(Span<F, S>) -> Result<R, F, S>> Combinator<F, S> for T {
    type Output = R;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<R, F, S> { self(input) }
}



/// Defines the shape of the output of [`Combinators`].
#[derive(Clone, Copy, Debug, EnumDebug)]
pub enum Result<R, F, S> {
    /// An `output` of type `R` was parsed (1), with the remainder left unparsed (0).
    Ok(Span<F, S>, R),
    /// Failed to parse input with the given reason, but recoverably so.
    Fail(fail::Failure),
    /// Failed to parse input with the given reason, but unrecoverably so.
    Error(error::Error),
}
impl<R, F, S> Result<R, F, S> {
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
impl<R: Eq, F, S: Eq + Spannable> Eq for Result<R, F, S> {}
impl<R: PartialEq, F, S: PartialEq + Spannable> PartialEq for Result<R, F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ok(rem1, res1), Self::Ok(rem2, res2)) => rem1 == rem2 && res1 == res2,
            (Self::Fail(fail1), Self::Fail(fail2)) => fail1.is_same(fail2),
            (Self::Error(err1), Self::Error(err2)) => err1.is_same(err2),

            // Otherwise, different variants, always bad
            _ => false,
        }
    }
}
