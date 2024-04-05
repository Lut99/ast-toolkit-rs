//  ERROR.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:51:38
//  Last edited:
//    05 Apr 2024, 11:37:46
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines `snack`'s extensive error type.
//

use std::error;
use std::fmt::{Display, Formatter, Result as FResult};

use ast_toolkit_span::{Span, SpanRange};

use crate::fail::{failure_impl, Failure};
use crate::{Combinator, Result};


/***** COMBINATORS *****/
/// "Commits" this parser path, by turning any [`Failure`]s that occur into [`Error`]s.
///
/// This is useful for when you have recognized a specific path and are now expecting a certain input.
/// And example of this would be matching parenthesis, where you always want to match the second one after you found the first.
///
/// # Arguments
/// - `comb`: Some [`Combinator`] that will be executed.
///
/// # Returns
/// The result of `comb`.
///
/// # Errors
/// If `comb` fails, then it is re-casted as an [`Error`]. Any errors already being emitted by `comb` are passed as-is.
pub fn commit<F, S, C: Combinator<F, S>>(mut comb: C) -> impl FnMut(Span<F, S>) -> Result<C::Output, F, S> {
    move |input: Span<F, S>| -> Result<C::Output, F, S> {
        match comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, res),
            Result::Fail(fail) => Result::Error(fail.into()),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// Provides some context to what it is we're parsing.
///
/// This is useful for explaining to the user why something is being parsed. This is commonly used in combination with [`commit()`] to show what route was being chosen.
///
/// For example, if you're parsing perenthesis, one might see:
/// ```ignore
/// sequence::pair(complete::tag("("), error::context("parenthesis", error::commit(sequence::pair(complete::tag("Hello, world!"), complete::tag(")")))));
/// ```
///
/// # Arguments
/// - `context`: Some string describing the context. This should be filling in `X` in: `While parsing X`.
/// - `comb`: Some [`Combinator`] that will be executed in the given `context`.
///
/// # Returns
/// The result of `comb`.
///
/// # Fails
/// This combinator fails if `comb` fails. These are propagated untouched.
///
/// # Errors
/// If `comb` error, then its error is wrapped in an [`Error::Context`].
pub fn context<F, S, C>(context: &'static str, mut comb: C) -> impl FnMut(Span<F, S>) -> Result<C::Output, F, S>
where
    F: Clone,
    S: Clone,
    C: Combinator<F, S>,
{
    move |input: Span<F, S>| -> Result<C::Output, F, S> {
        // Extract the start of the range that is contextualized
        let start_range: SpanRange = match input.range() {
            SpanRange::Closed(s, _) => SpanRange::ClosedOpen(s),
            SpanRange::ClosedOpen(s) => SpanRange::ClosedOpen(s),
            SpanRange::OpenClosed(_) => SpanRange::Open,
            SpanRange::Open => SpanRange::Open,
            SpanRange::Empty => SpanRange::Empty,
        };

        // Run the parsing, wrapping any errors
        match comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, res),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(Error::Context { context, start_range, err: Box::new(err) }),
        }
    }
}





/***** LIBRARY *****/
failure_impl! {
    /// `snack`'s extensive error type that can be used to generate explanatory diagnostics.
    ///
    /// One can think of this as a superset of [`Failure`], as any recoverable error may be turned into an unrecoverable one.
    #[derive(Clone, Debug)]
    pub enum Error {
        /// Defines that some nested error has some context string and span.
        Context {
            /// The context string describing what it is we're parsing.
            context: &'static str,
            /// Some span that indicates where this context started.
            start_range: SpanRange,
            /// The error that we wrap and display with context.
            err: Box<Self>,
        },
    }
}
impl From<Failure> for Error {
    #[inline]
    fn from(value: Failure) -> Self {
        match value {
            Failure::Digit1 => Self::Digit1,
            Failure::OneOfBytes1 { byteset } => Self::OneOfBytes1 { byteset },
            Failure::OneOfUtf81 { charset } => Self::OneOfUtf81 { charset },
            Failure::Tag { tag } => Self::Tag { tag },
            Failure::Whitespace1 => Self::Whitespace1,
        }
    }
}
