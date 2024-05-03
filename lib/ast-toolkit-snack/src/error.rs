//  ERROR.rs
//    by Lut99
//
//  Created:
//    07 Apr 2024, 17:58:35
//  Last edited:
//    03 May 2024, 11:56:18
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
use std::fmt::{Debug, Display, Formatter, Result as FResult};

use ast_toolkit_span::{Span, Spanning};
use enum_debug::EnumDebug;

use crate::bytes::complete::{OneOf1Expects as OneOf1BytesExpects, TagExpects as TagBytesExpects, While1Expects as While1BytesExpects};
use crate::combinator::NotExpects;
use crate::multi::Many1Expects;
use crate::utf8::complete::{
    Digit1Expects, OneOf1Expects as OneOf1Utf8Expects, TagExpects as TagUtf8Expects, While1Expects as While1Utf8Expects, Whitespace1Expects,
};
use crate::ExpectsFormatter;


/***** ERRORS *****/
/// Defines an error that may occur when casting [`Commons`]s to [`Failure`]s or [`Error`]s.
#[derive(Debug)]
pub struct TryFromCommonError(String, &'static str);
impl Display for TryFromCommonError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Cannot convert Common::{} to {} because there is no equivalent in Error", self.0, self.1)
    }
}
impl error::Error for TryFromCommonError {}

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





/***** LIBRARY *****/
/// Defines a common set of problems raised by snack combinators.
///
/// These are usually emitted as recoverable [`Failure`]s, but can be turned
/// into unrecoverable [`Error`]s by usage of the [`commit()`]-combinator. Both
/// enums also define a small set that cannot do this change (i.e., that cannot
/// be made unrecoverable or that isn't recoverable in the first place).
#[derive(Debug, EnumDebug)]
pub enum Common<'a, F, S> {
    /// All possible branches in an [`alt()`](crate::branch::alt())-combinator have failed.
    ///
    /// Note that, if there is only one possible branch, Alt acts more like a pass-through in terms of expecting.
    Alt { branches: Vec<Self>, fmt: Box<dyn 'a + ExpectsFormatter>, span: Span<F, S> },
    /// Failed to match at least one digit.
    Digit1 { span: Span<F, S> },
    /// Failed to match a combinator at least once.
    Many1 { fail: Box<Self>, nested_fmt: Box<dyn 'a + ExpectsFormatter> },
    /// Failed to match a combinator exactly N times.
    ManyN { n: usize, i: usize, fail: Box<Self>, nested_fmt: Box<dyn 'a + ExpectsFormatter> },
    /// Failed to _not_ apply a combinator.
    Not { nested_fmt: Box<dyn 'a + ExpectsFormatter>, span: Span<F, S> },
    /// Expected at least one of the following bytes.
    OneOf1Bytes { byteset: &'a [u8], span: Span<F, S> },
    /// Expected at least one of the following characters.
    OneOf1Utf8 { charset: &'a [&'a str], span: Span<F, S> },
    /// Failed to parse at least one value in a punctuated list of sorts.
    PunctuatedList1 { value_fail: Box<Self>, value_fmt: Box<dyn 'a + ExpectsFormatter>, punct_fmt: Box<dyn 'a + ExpectsFormatter> },
    /// Failed to match a combinator exactly N times, separated by some other thing, where the punctuation was what we failed to parse.
    PunctuatedListNPunct {
        n: usize,
        i: usize,
        punct_fail: Box<Self>,
        value_fmt: Box<dyn 'a + ExpectsFormatter>,
        punct_fmt: Box<dyn 'a + ExpectsFormatter>,
    },
    /// Failed to match a combinator exactly N times, separated by some other thing, where the value was what we failed to parse.
    PunctuatedListNValue {
        n: usize,
        i: usize,
        value_fail: Box<Self>,
        value_fmt: Box<dyn 'a + ExpectsFormatter>,
        punct_fmt: Box<dyn 'a + ExpectsFormatter>,
    },
    /// Failed to match something particular with byte version of the [`tag()`](crate::bytes::complete::tag())-combinator.
    TagBytes { tag: &'a [u8], span: Span<F, S> },
    /// Failed to match something particular with UTF-8 version of the [`tag()`](crate::utf8::complete::tag())-combinator.
    TagUtf8 { tag: &'a str, span: Span<F, S> },
    /// Failed to match something matching a predicate with the byte-version of [`while1()`](crate::bytes::complete::while1())-combinator.
    While1Bytes { span: Span<F, S> },
    /// Failed to match something matching a predicate with the UTF-8-version of [`while1()`](crate::utf8::complete::while1())-combinator.
    While1Utf8 { span: Span<F, S> },
    /// Failed to match at least one whitespace.
    Whitespace1 { span: Span<F, S> },
}

impl<'a, F, S> Display for Common<'a, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Alt { fmt, .. } => write!(f, "{fmt}"),
            Self::Digit1 { .. } => write!(f, "{}", Digit1Expects),
            Self::Many1 { nested_fmt, .. } => write!(f, "{}", Many1Expects { fmt: nested_fmt }),
            Self::ManyN { i, n, nested_fmt, .. } => {
                write!(f, "Expected at least {} more repetitions of ", *n - *i)?;
                nested_fmt.expects_fmt(f, 0)
            },
            Self::Not { nested_fmt, .. } => write!(f, "{}", NotExpects { fmt: nested_fmt }),
            Self::OneOf1Bytes { byteset, .. } => write!(f, "{}", OneOf1BytesExpects { byteset }),
            Self::OneOf1Utf8 { charset, .. } => write!(f, "{}", OneOf1Utf8Expects { charset }),
            Self::PunctuatedList1 { value_fmt, punct_fmt, .. } => {
                write!(f, "Expected ")?;
                value_fmt.expects_fmt(f, 0)?;
                write!(f, " as first entry in ")?;
                punct_fmt.expects_fmt(f, 0)?;
                write!(f, "-separated list")
            },
            Self::PunctuatedListNPunct { n, value_fmt, punct_fmt, .. } => {
                write!(f, "Expected ")?;
                value_fmt.expects_fmt(f, 0)?;
                write!(f, " to separate elements in ")?;
                punct_fmt.expects_fmt(f, 0)?;
                write!(f, "-list of {n} elements")
            },
            Self::PunctuatedListNValue { n, i, value_fmt, punct_fmt, .. } => {
                write!(f, "Expected ")?;
                value_fmt.expects_fmt(f, 0)?;
                write!(f, " as entry {} in ", i + 1)?;
                punct_fmt.expects_fmt(f, 0)?;
                write!(f, "-separated list of {n} elements")
            },
            Self::TagBytes { tag, .. } => write!(f, "{}", TagBytesExpects { tag }),
            Self::TagUtf8 { tag, .. } => write!(f, "{}", TagUtf8Expects { tag }),
            Self::While1Bytes { .. } => write!(f, "{}", While1BytesExpects),
            Self::While1Utf8 { .. } => write!(f, "{}", While1Utf8Expects),
            Self::Whitespace1 { .. } => write!(f, "{}", Whitespace1Expects),
        }
    }
}
impl<'a, F: Clone, S: Clone> Spanning<F, S> for Common<'a, F, S> {
    fn span(&self) -> Span<F, S> {
        match self {
            Self::Alt { span, .. } => span.clone(),
            Self::Digit1 { span } => span.clone(),
            Self::Many1 { fail, .. } => fail.span(),
            Self::ManyN { fail, .. } => fail.span(),
            Self::Not { span, .. } => span.clone(),
            Self::OneOf1Bytes { span, .. } => span.clone(),
            Self::OneOf1Utf8 { span, .. } => span.clone(),
            Self::PunctuatedList1 { value_fail, .. } => value_fail.span(),
            Self::PunctuatedListNPunct { punct_fail, .. } => punct_fail.span(),
            Self::PunctuatedListNValue { value_fail, .. } => value_fail.span(),
            Self::TagBytes { span, .. } => span.clone(),
            Self::TagUtf8 { span, .. } => span.clone(),
            Self::While1Bytes { span } => span.clone(),
            Self::While1Utf8 { span } => span.clone(),
            Self::Whitespace1 { span } => span.clone(),
        }
    }
}



/// Defines a problems emitted by snack combinators that are recoverable.
#[derive(Debug, EnumDebug)]
pub enum Failure<'a, F, S> {
    /// There wasn't enough input data and the combinator was a streaming combinator.
    ///
    /// The `needed` is an optional hint provided by the combinator to guess how many more bytes would be needed.
    NotEnough { needed: Option<usize>, span: Span<F, S> },

    /// It's a type of failure that can be made a non-recoverable [`Error`].
    Common(Common<'a, F, S>),
}

impl<'a, F, S> Display for Failure<'a, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult {
        match self {
            Self::NotEnough { needed, .. } => {
                if let Some(needed) = needed {
                    write!(f, "{needed} more input bytes")
                } else {
                    write!(f, "more input")
                }
            },

            Self::Common(p) => <Common<'a, F, S> as Display>::fmt(p, f),
        }
    }
}
impl<'a, F: Clone, S: Clone> Spanning<F, S> for Failure<'a, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::NotEnough { span, .. } => span.clone(),

            Self::Common(p) => p.span(),
        }
    }
}

impl<'a, F, S> From<Common<'a, F, S>> for Failure<'a, F, S> {
    #[inline]
    fn from(value: Common<'a, F, S>) -> Self { Self::Common(value) }
}
impl<'a, F, S> TryFrom<Failure<'a, F, S>> for Common<'a, F, S> {
    type Error = TryFromCommonError;

    #[inline]
    fn try_from(value: Failure<'a, F, S>) -> Result<Self, Self::Error> {
        match value {
            Failure::Common(c) => Ok(c),
            Failure::NotEnough { .. } => Err(TryFromCommonError(value.variant().to_string(), "a Failure")),
        }
    }
}



/// Defines a problems emitted by snack combinators that are non-recoverable.
#[derive(Debug, EnumDebug)]
pub enum Error<'a, F, S> {
    /// There is a specific context in which the parsing failed.
    ///
    /// This is usually used through the [`context()`]-combinator, and allows
    /// one to hint to the user that something larger was being parsed (e.g., expressions).
    Context { context: &'static str, span: Span<F, S>, err: Box<Self> },

    /// It's a type of error that can come from recoverable [`Failure`]s.
    Common(Common<'a, F, S>),
}

impl<'a, F, S> Display for Error<'a, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult {
        match self {
            Self::Context { context, .. } => {
                write!(f, "{context}")
            },

            Self::Common(p) => <Common<'a, F, S> as Display>::fmt(p, f),
        }
    }
}
impl<'a, F: Clone, S: Clone> Spanning<F, S> for Error<'a, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::Context { span, .. } => span.clone(),

            Self::Common(p) => p.span(),
        }
    }
}

impl<'a, F, S> From<Common<'a, F, S>> for Error<'a, F, S> {
    #[inline]
    fn from(value: Common<'a, F, S>) -> Self { Self::Common(value) }
}
impl<'a, F, S> TryFrom<Error<'a, F, S>> for Common<'a, F, S> {
    type Error = TryFromCommonError;

    #[inline]
    fn try_from(value: Error<'a, F, S>) -> Result<Self, Self::Error> {
        match value {
            Error::Common(c) => Ok(c),
            Error::Context { .. } => Err(TryFromCommonError(value.variant().to_string(), "an Error")),
        }
    }
}
impl<'a, F, S> TryFrom<Failure<'a, F, S>> for Error<'a, F, S> {
    type Error = TryFromFailureError;

    #[inline]
    fn try_from(value: Failure<'a, F, S>) -> Result<Self, Self::Error> {
        match value {
            Failure::Common(p) => Ok(Self::Common(p)),
            other => Err(TryFromFailureError(other.variant().to_string())),
        }
    }
}
