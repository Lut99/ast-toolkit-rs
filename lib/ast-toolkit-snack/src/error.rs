//  ERROR.rs
//    by Lut99
//
//  Created:
//    07 Apr 2024, 17:58:35
//  Last edited:
//    02 May 2024, 11:10:05
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
    Alt { branches: Vec<Self>, span: Span<F, S> },
    /// Failed to match at least one digit.
    Digit1 { span: Span<F, S> },
    /// Failed to match a combinator at least once.
    Many1 { fail: Box<Self> },
    /// Failed to match a combinator exactly N times.
    ManyN { n: usize, i: usize, fail: Box<Self> },
    /// Failed to _not_ apply a combinator.
    Not { expects: Box<dyn 'a + ExpectsFormatter>, span: Span<F, S> },
    /// Expected at least one of the following bytes.
    OneOf1Bytes { byteset: &'a [u8], span: Span<F, S> },
    /// Expected at least one of the following characters.
    OneOf1Utf8 { charset: &'a [&'a str], span: Span<F, S> },
    /// Failed to match a combinator at least once, separated by some other thing.
    #[cfg(feature = "punctuated")]
    Punctuated1 { fail: Box<Self> },
    /// Failed to match a combinator exactly N times, separated by some other thing, where the punctuation was what we failed to parse.
    #[cfg(feature = "punctuated")]
    PunctuatedNPunct { n: usize, i: usize, values: Box<dyn 'a + ExpectsFormatter>, puncts: Box<Self> },
    /// Failed to match a combinator exactly N times, separated by some other thing, where the value was what we failed to parse.
    #[cfg(feature = "punctuated")]
    PunctuatedNValue { n: usize, i: usize, values: Box<Self>, puncts: Box<dyn 'a + ExpectsFormatter> },
    /// Failed to match a combinator at least once, separated by some other thing.
    #[cfg(feature = "punctuated")]
    PunctuatedTrailing1 { fail: Box<Self> },
    /// Failed to match a combinator exactly N times, separated by some other thing, where the punctuation was what we failed to parse.
    #[cfg(feature = "punctuated")]
    PunctuatedTrailingNPunct { n: usize, i: usize, values: Box<dyn 'a + ExpectsFormatter>, puncts: Box<Self> },
    /// Failed to match a combinator exactly N times, separated by some other thing, where the value was what we failed to parse.
    #[cfg(feature = "punctuated")]
    PunctuatedTrailingNValue { n: usize, i: usize, values: Box<Self>, puncts: Box<dyn 'a + ExpectsFormatter> },
    /// Failed to match a combinator at least once, separated by some other thing.
    SeparatedList1 { fail: Box<Self> },
    /// Failed to match a combinator exactly N times, separated by some other thing, where the punctuation was what we failed to parse.
    SeparatedListNPunct { n: usize, i: usize, values: Box<dyn 'a + ExpectsFormatter>, puncts: Box<Self> },
    /// Failed to match a combinator exactly N times, separated by some other thing, where the value was what we failed to parse.
    SeparatedListNValue { n: usize, i: usize, values: Box<Self>, puncts: Box<dyn 'a + ExpectsFormatter> },
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

// impl<'a, F, S> Expects for Common<'a, F, S> {
//     fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
//         match self {
//             Self::Alt { branches, .. } => expects_alt(f, indent, branches.iter().map(|b| -> &dyn Expects { b })),
//             Self::Digit1 { .. } => expects_digit1(f),
//             Self::Many1 { fail } => expects_many1(f, indent, fail.expects()),
//             Self::ManyN { n, i: _, fail } => expects_many_n(f, indent, *n, fail.expects()),
//             Self::Not { expects, .. } => expects_not(f, indent, expects),
//             Self::OneOf1Bytes { byteset, .. } => expects_one_of1_bytes(f, byteset),
//             Self::OneOf1Utf8 { charset, .. } => expects_one_of1_utf8(f, charset),
//             #[cfg(feature = "punctuated")]
//             Self::Punctuated1 { fail } => expects_separated_list1_fail(f, indent, fail.expects()),
//             #[cfg(feature = "punctuated")]
//             Self::PunctuatedNPunct { n, i: _, values, puncts } => expects_separated_list_n_punct(f, indent, *n, *values, puncts.expects()),
//             #[cfg(feature = "punctuated")]
//             Self::PunctuatedNValue { n, i: _, values, puncts } => expects_separated_list_n_value(f, indent, *n, values.expects(), *puncts),
//             #[cfg(feature = "punctuated")]
//             Self::PunctuatedTrailing1 { fail } => expects_punctuated_trailing1_fail(f, indent, fail.expects()),
//             #[cfg(feature = "punctuated")]
//             Self::PunctuatedTrailingNPunct { n, i: _, values, puncts } => {
//                 expects_punctuated_trailing_n_punct(f, indent, *n, *values, puncts.expects())
//             },
//             #[cfg(feature = "punctuated")]
//             Self::PunctuatedTrailingNValue { n, i: _, values, puncts } => {
//                 expects_punctuated_trailing_n_value(f, indent, *n, values.expects(), *puncts)
//             },
//             Self::SeparatedList1 { fail } => expects_separated_list1_fail(f, indent, fail.expects()),
//             Self::SeparatedListNPunct { n, i: _, values, puncts } => expects_separated_list_n_punct(f, indent, *n, *values, puncts.expects()),
//             Self::SeparatedListNValue { n, i: _, values, puncts } => expects_separated_list_n_value(f, indent, *n, values.expects(), *puncts),
//             Self::TagBytes { tag, .. } => expects_tag_bytes(f, tag),
//             Self::TagUtf8 { tag, .. } => expects_tag_utf8(f, tag),
//             Self::While1Utf8 { .. } => expects_while1_utf8(f),
//             Self::Whitespace1 { .. } => expects_whitespace1(f),
//         }
//     }
// }
impl<'a, F: Clone, S: Clone> Spanning<F, S> for Common<'a, F, S> {
    fn span(&self) -> Span<F, S> {
        match self {
            Self::Alt { span, .. } => span.clone(),
            Self::Digit1 { span } => span.clone(),
            Self::Many1 { fail } => fail.span(),
            Self::ManyN { fail, .. } => fail.span(),
            Self::Not { span, .. } => span.clone(),
            Self::OneOf1Bytes { span, .. } => span.clone(),
            Self::OneOf1Utf8 { span, .. } => span.clone(),
            #[cfg(feature = "punctuated")]
            Self::Punctuated1 { fail } => fail.span(),
            #[cfg(feature = "punctuated")]
            Self::PunctuatedNPunct { puncts, .. } => puncts.span(),
            #[cfg(feature = "punctuated")]
            Self::PunctuatedNValue { values, .. } => values.span(),
            #[cfg(feature = "punctuated")]
            Self::PunctuatedTrailing1 { fail } => fail.span(),
            #[cfg(feature = "punctuated")]
            Self::PunctuatedTrailingNPunct { puncts, .. } => puncts.span(),
            #[cfg(feature = "punctuated")]
            Self::PunctuatedTrailingNValue { values, .. } => values.span(),
            Self::SeparatedList1 { fail } => fail.span(),
            Self::SeparatedListNPunct { puncts, .. } => puncts.span(),
            Self::SeparatedListNValue { values, .. } => values.span(),
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

// impl<'a, F, S> Expects for Failure<'a, F, S> {
//     #[inline]
//     fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
//         match self {
//             Self::NotEnough { needed, .. } => {
//                 if let Some(needed) = needed {
//                     write!(f, "{needed} more input bytes")
//                 } else {
//                     write!(f, "more input")
//                 }
//             },

//             Self::Common(p) => <Common<F, S> as Expects>::fmt(p, f, indent),
//         }
//     }
// }
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

// impl<'a, F, S> Expects for Error<'a, F, S> {
//     #[inline]
//     fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
//         match self {
//             Self::Context { context, .. } => {
//                 write!(f, "{context}")
//             },

//             Self::Common(p) => <Common<F, S> as Expects>::fmt(p, f, indent),
//         }
//     }
// }
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
