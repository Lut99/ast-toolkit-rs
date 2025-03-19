//  RESULT.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 16:52:42
//  Last edited:
//    19 Mar 2025, 14:57:21
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines the things returned from [`Combinator`](crate::Combinator)s,
//!   including the error types.
//

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{self, Display, Formatter, Result as FResult};

use ast_toolkit_span::{Span, Spannable, Spanning};
use better_derive::{Debug, Eq, PartialEq};

use crate::{ExpectsFormatter, ParseError};


/***** LIBRARY *****/
/// The return type of all snack [`Combinator`](crate::Combinator)s.
///
/// It is essentially a three-way return type but separated in two levels to use the stock [`Result`] (so that `?` works).
pub type Result<T, E1, E2, S> = std::result::Result<(Span<S>, T), SnackError<E1, E2, S>>;



/// The main snack error type.
///
/// Snack, like nom, returns three possible errors:
/// 1. Recoverable errors, which means that [branches](crate::branch::alt) can continue searching;
/// 2. Fatal errors, which means that branches should stop searching (the branch was correct but
///    the input is malformed); and
/// 3. Not enough input, which is only relevant for streaming versions of combinators. In this
///    case, it signals that the branch _looks_ incorrect/incomplete, but that additional things
///    can be given after the current end-of-file that collapses the correctness one way or another.
#[derive(Debug)]
pub enum SnackError<E1, E2, S> {
    /// It's a recoverable error.
    ///
    /// This means that any [branch::alt](crate::branch::alt) combinator should try another branch
    /// and might still have some luck.
    Recoverable(E1),
    /// It's a non-recoverable error.
    ///
    /// This means that any [branch::alt](crate::branch::alt) combinator should stop searching.
    /// You can interpret this error as "correct branch, but malformed input". An example is a
    /// missing closing parenthesis.
    Fatal(E2),
    /// The parser is not sure whether an error occurred.
    ///
    /// This means that the current branch is _probably_ incorrect or malformed, specifically due
    /// to being incomplete somehow (more was expected). More input may be given to turn this error
    /// into a complete phrase.
    NotEnough {
        /// How much more input should (at least) be given to make it correct - if this is known.
        ///
        /// Note the size is given in _bytes_.
        needed: Option<usize>,
        /// The span pointing to the end of the input stream.
        span:   Span<S>,
    },
}
impl<E1, E2, S> Display for SnackError<E1, E2, S>
where
    E1: Display,
    E2: Display,
{
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Recoverable(err) => err.fmt(f),
            Self::Fatal(err) => err.fmt(f),
            Self::NotEnough { needed, .. } => {
                write!(f, "Unexpected end-of-file")?;
                if let Some(needed) = needed {
                    write!(f, " (expected {needed} more input tokens)")?;
                }
                Ok(())
            },
        }
    }
}
impl<E1, E2, S> Eq for SnackError<E1, E2, S>
where
    E1: Eq,
    E2: Eq,
    S: Spannable,
{
}
impl<E1, E2, S> PartialEq for SnackError<E1, E2, S>
where
    E1: PartialEq,
    E2: PartialEq,
    S: Spannable,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Only match other with the same variant
            (Self::Recoverable(err1), Self::Recoverable(err2)) => err1 == err2,
            (Self::Fatal(err1), Self::Fatal(err2)) => err1 == err2,
            (Self::NotEnough { needed: needed1, span: span1 }, Self::NotEnough { needed: needed2, span: span2 }) => {
                needed1 == needed2 && span1 == span2
            },

            // Anything else never equals
            _ => false,
        }
    }
}
impl<E1, E2, S> Spanning<S> for SnackError<E1, E2, S>
where
    E1: Spanning<S>,
    E2: Spanning<S>,
    S: Clone,
{
    #[inline]
    fn span(&self) -> Cow<Span<S>> {
        match self {
            Self::Recoverable(err) => err.span(),
            Self::Fatal(err) => err.span(),
            Self::NotEnough { span, .. } => Cow::Borrowed(span),
        }
    }

    #[inline]
    fn into_span(self) -> Span<S> {
        match self {
            Self::Recoverable(err) => err.into_span(),
            Self::Fatal(err) => err.into_span(),
            Self::NotEnough { span, .. } => span,
        }
    }
}



/// Defines a common [recoverable](SnackError::Recoverable) error type that simply describes what
/// was expected.
#[derive(Debug, Eq, PartialEq)]
pub struct Expected<O, S> {
    /// The formatter that will tell us what was expected.
    pub fmt:  O,
    /// The span that tells us where we expected it.
    pub span: Span<S>,
}
impl<O: ExpectsFormatter, S> Display for Expected<O, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { <O as Display>::fmt(&self.fmt, f) }
}
impl<O: ExpectsFormatter, S: Spannable> Error for Expected<O, S> {}
impl<O, S: Clone> Spanning<S> for Expected<O, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S>
    where
        Self: Sized,
    {
        self.span
    }
}



/// Defines a wrapper around a normal [`Error`] to extend it with a source location.
///
/// I.e., turns an [`Error`] into a [`ParseError`](super::ParseError).
///
/// # Examples
/// The following code would not run:
/// ```compile_fail
/// use std::num::ParseIntError;
/// use std::str::FromStr as _;
/// use ast_toolkit_snack::ParseError;
///
/// fn is_parse_error<T: ParseError<S>, S: Clone>(err: T) {}
///
/// is_parse_error(u32::from_str("a").unwrap_err());
/// ```
///
/// We can use [`SpanningError`] to patch it:
/// ```rust
/// use std::num::ParseIntError;
/// use std::str::FromStr as _;
///
/// use ast_toolkit_snack::ParseError;
/// use ast_toolkit_snack::result::SpanningError;
/// use ast_toolkit_span::Span;
///
/// fn is_parse_error<T: ParseError<S>, S: Clone>(err: T) {}
///
/// is_parse_error(SpanningError { err: u32::from_str("a").unwrap_err(), span: Span::new("a") });
/// ```
#[derive(Debug, Eq, PartialEq)]
pub struct SpanningError<E, S> {
    /// The error wrapped.
    pub err:  E,
    /// The span that points to where it occurs.
    pub span: Span<S>,
}
impl<E: Display, S> Display for SpanningError<E, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { self.err.fmt(f) }
}
impl<E: Error, S: Spannable> Error for SpanningError<E, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> { self.err.source() }
}
impl<E, S: Clone> Spanning<S> for SpanningError<E, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}

/// Implements a [`ParseError`] that is [`Box`]ed.
///
/// This is a separate type because [`Box`] does not implement [`Error`] unless the wrapped type is
/// [`Sized`] (see <https://users.rust-lang.org/t/why-box-dyn-error-is-not-sized/61642/4>).
///
/// We fix it by defining this custom type ourselves which _does_ implement [`Error`] and,
/// therefore, [`ParseError`].
pub struct BoxedParseError<'e, S: Clone>(pub Box<dyn 'e + ParseError<S>>);
impl<'e, S: Clone> BoxedParseError<'e, S> {
    #[inline]
    pub fn new(err: impl 'e + ParseError<S>) -> Self { Self(Box::new(err) as Box<dyn 'e + ParseError<S>>) }
}
impl<'e, S: Clone> fmt::Debug for BoxedParseError<'e, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let Self(err) = self;
        let mut fmt = f.debug_tuple("BoxedParseError");
        fmt.field(err);
        fmt.finish()
    }
}
impl<'e, S: Clone> Display for BoxedParseError<'e, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { <Box<dyn ParseError<S>> as Display>::fmt(&self.0, f) }
}
impl<'e, S: Clone> Error for BoxedParseError<'e, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> { self.0.source() }
}
impl<'e, S: Clone> Spanning<S> for BoxedParseError<'e, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { self.0.span() }
    #[inline]
    fn into_span(self) -> Span<S> { self.0.into_span() }
}
