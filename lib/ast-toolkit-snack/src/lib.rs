//  LIB.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:37:24
//  Last edited:
//    20 Mar 2024, 16:36:00
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
use ast_toolkit_span::Span;
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
    Ok((Span<F, S>, R)),
    /// Failed to parse input with the given reason, but recoverably so.
    Fail(fail::Failure),
    /// Failed to parse input with the given reason, but unrecoverably so.
    Error(error::Error),
}
