//  ERROR.rs
//    by Lut99
//
//  Created:
//    07 Apr 2024, 17:58:35
//  Last edited:
//    23 Apr 2024, 17:48:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines problems raised by parsing with `snack`.
//!   
//!   There two types of problems defined by the crate:
//!   - Recoverable problems, called [`Failure`]s; and
//!   - Unrecoverable problems, called [`Error`]s.
//!   
//!   Note, however, that they share quite some overlap, because the [`commit()`]
//!   combinator allows promoting (almost) all recoverable [`Failure`]s into
//!   non-recoverable [`Error`]s. The common set that can do this is called
//!   [`Common`].
//

use std::error;
use std::fmt::{Display, Formatter, Result as FResult};

use ast_toolkit_span::{Span, Spanning};
use enum_debug::EnumDebug;

use crate::Expects;


/***** ERRORS *****/
/// Defines an error that may occur when casting [`Failure`]s to [`Error`]s.
#[derive(Debug)]
pub struct TryFromFailureError(String);
impl Display for TryFromFailureError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Cannot convert Failure::{} to an Error because there is no equivalent in Error", self.0)
    }
}
impl error::Error for TryFromFailureError {}





/***** EXPECTS *****/
/// Defines what we expect from an [`Alt`](crate::branch::alt).
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Some indentation level to apply when writing new lines.
/// - `branches`: Some iterator yielding [`Expects`] for all branches.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
pub(crate) fn expects_alt<'b>(f: &mut Formatter, indent: usize, branches: impl IntoIterator<Item = &'b dyn Expects>) -> FResult {
    writeln!(f, "one of:")?;
    for b in branches {
        write!(f, "{} - ", (0..indent).map(|_| ' ').collect::<String>())?;
        b.fmt(f, indent + 1)?;
        writeln!(f)?;
    }
    writeln!(f)
}





/***** LIBRARY *****/
/// Defines a common set of problems raised by snack combinators.
///
/// These are usually emitted as recoverable [`Failure`]s, but can be turned
/// into unrecoverable [`Error`]s by usage of the [`commit()`]-combinator. Both
/// enums also define a small set that cannot do this change (i.e., that cannot
/// be made unrecoverable or that isn't recoverable in the first place).
#[derive(Clone, Debug, EnumDebug)]
pub enum Common<F, S> {
    /// All possible branches in an [`alt()`](super::branch::alt())-combinator have failed.
    ///
    /// Note that, if there is only one possible branch, Alt acts more like a pass-through in terms of expecting.
    Alt { branches: Vec<Self>, span: Span<F, S> },
}

impl<F, S> Expects for Common<F, S> {
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        match self {
            Self::Alt { branches, .. } => expects_alt(f, indent, branches.iter().map(|b| -> &dyn Expects { b })),
        }
    }
}
impl<F: Clone, S: Clone> Spanning<F, S> for Common<F, S> {
    fn span(&self) -> Span<F, S> {
        match self {
            Self::Alt { span, .. } => span.clone(),
        }
    }
}



/// Defines a problems emitted by snack combinators that are recoverable.
#[derive(Clone, Debug, EnumDebug)]
pub enum Failure<F, S> {
    /// There wasn't enough input data and the combinator was a streaming combinator.
    ///
    /// The `needed` is an optional hint provided by the combinator to guess how many more bytes would be needed.
    NotEnough { needed: Option<usize>, span: Span<F, S> },

    /// It's a type of failure that can be made a non-recoverable [`Error`].
    Common(Common<F, S>),
}

impl<F, S> Expects for Failure<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        match self {
            Self::NotEnough { needed, .. } => {
                if let Some(needed) = needed {
                    write!(f, "{needed} more input bytes")
                } else {
                    write!(f, "more input")
                }
            },

            Self::Common(p) => <Common<F, S> as Expects>::fmt(p, f, indent),
        }
    }
}
impl<F: Clone, S: Clone> Spanning<F, S> for Failure<F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::NotEnough { span, .. } => span.clone(),

            Self::Common(p) => p.span(),
        }
    }
}

impl<F, S> From<Common<F, S>> for Failure<F, S> {
    #[inline]
    fn from(value: Common<F, S>) -> Self { Self::Common(value) }
}



/// Defines a problems emitted by snack combinators that are non-recoverable.
#[derive(Clone, Debug, EnumDebug)]
pub enum Error<F, S> {
    /// There is a specific context in which the parsing failed.
    ///
    /// This is usually used through the [`context()`]-combinator, and allows
    /// one to hint to the user that something larger was being parsed (e.g., expressions).
    Context { context: &'static str, span: Span<F, S>, err: Box<Self> },

    /// It's a type of error that can come from recoverable [`Failure`]s.
    Common(Common<F, S>),
}

impl<F, S> Expects for Error<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        match self {
            Self::Context { context, .. } => {
                write!(f, "{context}")
            },

            Self::Common(p) => <Common<F, S> as Expects>::fmt(p, f, indent),
        }
    }
}
impl<F: Clone, S: Clone> Spanning<F, S> for Error<F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::Context { span, .. } => span.clone(),

            Self::Common(p) => p.span(),
        }
    }
}

impl<F, S> From<Common<F, S>> for Error<F, S> {
    #[inline]
    fn from(value: Common<F, S>) -> Self { Self::Common(value) }
}
impl<F, S> TryFrom<Failure<F, S>> for Error<F, S> {
    type Error = TryFromFailureError;

    #[inline]
    fn try_from(value: Failure<F, S>) -> Result<Self, Self::Error> {
        match value {
            Failure::Common(p) => Ok(Self::Common(p)),
            other => Err(TryFromFailureError(other.variant().to_string())),
        }
    }
}
