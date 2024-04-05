//  ERROR.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:51:38
//  Last edited:
//    05 Apr 2024, 19:27:10
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines `snack`'s extensive error type.
//

use std::error;
use std::fmt::{Display, Formatter, Result as FResult};

use ast_toolkit_span::Span;

use crate::fail::{failure_impl, Failure};
use crate::{Combinator, Result};


/***** ERRORS *****/
/// Defines an error that may occur when casting [`Failure`]s to [`Error`]s.
#[derive(Debug)]
pub enum FromFailureError {
    /// Failed to cast [`Failure::NotEnough`] because it is [`Failure`]-only.
    NotEnough,
}
impl Display for FromFailureError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use FromFailureError::*;
        match self {
            NotEnough => write!(f, "Cannot convert Failure::NotEnough to an Error because there is no equivalent in Error"),
        }
    }
}
impl error::Error for FromFailureError {}





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
/// # Fails
/// This function may fail if `comb` is streaming and returns [`Failure::NotEnough`]. This is not casted to an error because not enough should only be resolved as an error when more input is attempted to be gotten but that fails.
///
/// # Errors
/// If `comb` fails, then it is re-casted as an [`Error`]. Any errors already being emitted by `comb` are passed as-is.
///
/// Note that one type of failure is never re-casted: and that is [`Failure::NotEnough`]. See [Fails](#Fails) above for more details.
pub fn commit<F, S, C: Combinator<F, S>>(mut comb: C) -> impl FnMut(Span<F, S>) -> Result<C::Output, F, S> {
    move |input: Span<F, S>| -> Result<C::Output, F, S> {
        match comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, res),
            Result::Fail(Failure::NotEnough { span }) => Result::Fail(Failure::NotEnough { span }),
            // SAFETY: We can `unwrap()` because we caught the only case for which it fails above.
            Result::Fail(fail) => Result::Error(fail.try_into().unwrap()),
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
        // Run the parsing, wrapping any errors
        let span: Span<F, S> = input.start_onwards();
        match comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, res),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(Error::Context { context, span, err: Box::new(err) }),
        }
    }
}





/***** LIBRARY *****/
failure_impl! {
    /// `snack`'s extensive error type that can be used to generate explanatory diagnostics.
    ///
    /// One can think of this as a superset of [`Failure`], as any recoverable error may be turned into an unrecoverable one.
    #[derive(Clone, Debug)]
    pub enum Error<F, S> {
        /// Defines that some nested error has some context string and span.
        Context {
            /// The context string describing what it is we're parsing.
            context: &'static str,
            /// Some span that indicates where this context started.
            span: Span<F, S>,
            /// The error that we wrap and display with context.
            err: Box<Self>,
        },
    }
    impl Display {
        Context { context, .. } => write!(f, "The above occurred while parsing {context}"),
    }
    impl<F, S> Spanning<F, S> {
        Context { span, err, .. } => span.join(&err.span()).expect("`span` and `err` are from different source texts; cannot merge them!"),
    }
}
impl<F, S> TryFrom<Failure<F, S>> for Error<F, S> {
    type Error = FromFailureError;

    #[inline]
    fn try_from(value: Failure<F, S>) -> std::result::Result<Self, Self::Error> {
        match value {
            Failure::Alt { branches } => Ok(Self::Alt {
                branches: branches.into_iter().map(|fail| fail.try_into()).collect::<std::result::Result<Vec<Self>, FromFailureError>>()?,
            }),
            Failure::Custom { problem } => Ok(Self::Custom { problem }),
            Failure::Digit1 { span } => Ok(Self::Digit1 { span }),
            Failure::ManyN { times, got, fail } => Ok(Self::ManyN { times, got, fail: Box::new((*fail).try_into()?) }),
            Failure::Not { span } => Ok(Self::Not { span }),
            Failure::NotEnough { .. } => Err(FromFailureError::NotEnough),
            Failure::OneOfBytes1 { byteset, span } => Ok(Self::OneOfBytes1 { byteset, span }),
            Failure::OneOfUtf81 { charset, span } => Ok(Self::OneOfUtf81 { charset, span }),
            Failure::PunctuatedNPunct { times, got, fail } => Ok(Self::PunctuatedNPunct { times, got, fail: Box::new((*fail).try_into()?) }),
            Failure::PunctuatedNValue { times, got, fail } => Ok(Self::PunctuatedNValue { times, got, fail: Box::new((*fail).try_into()?) }),
            Failure::PunctuatedTrailingNPunct { times, got, fail } => {
                Ok(Self::PunctuatedTrailingNPunct { times, got, fail: Box::new((*fail).try_into()?) })
            },
            Failure::PunctuatedTrailingNValue { times, got, fail } => {
                Ok(Self::PunctuatedTrailingNValue { times, got, fail: Box::new((*fail).try_into()?) })
            },
            Failure::SeparatedListNPunct { times, got, fail } => Ok(Self::SeparatedListNPunct { times, got, fail: Box::new((*fail).try_into()?) }),
            Failure::SeparatedListNValue { times, got, fail } => Ok(Self::SeparatedListNValue { times, got, fail: Box::new((*fail).try_into()?) }),
            Failure::Tag { tag, span } => Ok(Self::Tag { tag, span }),
            Failure::Whitespace1 { span } => Ok(Self::Whitespace1 { span }),
        }
    }
}
