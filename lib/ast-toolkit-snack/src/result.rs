//  RESULT.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 16:52:42
//  Last edited:
//    22 Apr 2025, 11:50:58
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines the things returned from [`Combinator`](crate::Combinator)s,
//!   including the error types.
//

use std::borrow::Cow;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::{self, Display, Formatter, Result as FResult};

use ast_toolkit_span::{Span, Spannable, Spanning};
use better_derive::{Debug, Eq, PartialEq};

use crate::ExpectsFormatter;
use crate::boxed::{BoxableParseError, BoxedParseError};


/***** IMPORT ALIASES *****/
/// Assembles all of the interfaces.
pub mod prelude {
    pub use super::ResultExt;
}





/***** ERRORS *****/
/// Auxillary type for the [`SnackError`] for when it is [`cut()`](SnackError::cut()).
#[derive(Debug, Eq, PartialEq)]
pub enum CutError<E1, E2> {
    /// It was originally a [`SnackError::Recoverable`] error.
    Recoverable(E1),
    /// It was originally also a [`SnackError::Fatal`] error.
    Fatal(E2),
}
impl<E1: Display, E2: Display> Display for CutError<E1, E2> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Recoverable(err) => err.fmt(f),
            Self::Fatal(err) => err.fmt(f),
        }
    }
}
impl<E1: fmt::Debug + Display, E2: fmt::Debug + Display> Error for CutError<E1, E2> {}
impl<E1: Spanning<S>, E2: Spanning<S>, S: Clone> Spanning<S> for CutError<E1, E2> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> {
        match self {
            Self::Recoverable(err) => err.span(),
            Self::Fatal(err) => err.span(),
        }
    }

    #[inline]
    fn into_span(self) -> Span<S> {
        match self {
            Self::Recoverable(err) => err.into_span(),
            Self::Fatal(err) => err.into_span(),
        }
    }
}





/***** INTERFACES *****/
/// Defines some [`SnackError`]s transparently on [`Result`]s.
pub trait ResultExt<T, E1, E2, S>: Sized {
    /// "Cuts" the [`SnackError`] in this Result.
    ///
    /// This is a shortcut for using the [`cut()`](crate::error::cut())-combinator. In essence, it
    /// will turn all [recoverable](SnackError::Recoverable) errors into
    /// [fatal](SnackError::Fatal), ones, "cutting" the branching search.
    ///
    /// # Returns
    /// A Result with a [`SnackError`] which is either [`SnackError::Fatal`] or
    /// [`SnackError::NotEnough`].
    fn cut(self) -> std::result::Result<T, SnackError<Infallible, CutError<E1, E2>, S>>;

    /// "Uncuts" the [`SnackError`] in this Result.
    ///
    /// This is a shortcut for using the [`uncut()`](crate::error::uncut())-combinator. In essence,
    /// it will turn all [fatal](SnackError::Fatal) errors into
    /// [recoverable](SnackError::Recoverable) ones, "catching" the fatal error and instead
    /// allowing parent branches to be searched again.
    ///
    /// # Returns
    /// A Result with a [`SnackError`] which is either [`SnackError::Recoverable`] or
    /// [`SnackError::NotEnough`].
    fn uncut(self) -> std::result::Result<T, SnackError<CutError<E1, E2>, Infallible, S>>;
}

impl<T, E1, E2, S> ResultExt<T, E1, E2, S> for ::std::result::Result<T, SnackError<E1, E2, S>> {
    #[inline]
    fn cut(self) -> std::result::Result<T, SnackError<Infallible, CutError<E1, E2>, S>> {
        match self {
            Ok(res) => Ok(res),
            Err(err) => Err(err.cut()),
        }
    }

    #[inline]
    fn uncut(self) -> std::result::Result<T, SnackError<CutError<E1, E2>, Infallible, S>> {
        match self {
            Ok(res) => Ok(res),
            Err(err) => Err(err.uncut()),
        }
    }
}





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
#[derive(Debug, Eq, PartialEq)]
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
impl<E1, E2, S> SnackError<E1, E2, S> {
    /// "Cuts" this SnackError.
    ///
    /// This is a shortcut for using the [`cut()`](crate::error::cut())-combinator. In essence, it
    /// will turn all [recoverable](SnackError::Recoverable) errors into
    /// [fatal](SnackError::Fatal), ones, "cutting" the branching search.
    ///
    /// # Returns
    /// A SnackError which is either [`SnackError::Fatal`] or [`SnackError::NotEnough`].
    #[inline]
    pub fn cut(self) -> SnackError<Infallible, CutError<E1, E2>, S> {
        match self {
            Self::Recoverable(err) => SnackError::Fatal(CutError::Recoverable(err)),
            Self::Fatal(err) => SnackError::Fatal(CutError::Fatal(err)),
            Self::NotEnough { needed, span } => SnackError::NotEnough { needed, span },
        }
    }

    /// "Uncuts" this SnackError.
    ///
    /// This is a shortcut for using the [`uncut()`](crate::error::uncut())-combinator. In essence,
    /// it will turn all [fatal](SnackError::Fatal) errors into
    /// [recoverable](SnackError::Recoverable) ones, "catching" the fatal error and instead
    /// allowing parent branches to be searched again.
    ///
    /// # Returns
    /// A SnackError which is either [`SnackError::Recoverable`] or [`SnackError::NotEnough`].
    #[inline]
    pub fn uncut(self) -> SnackError<CutError<E1, E2>, Infallible, S> {
        match self {
            Self::Recoverable(err) => SnackError::Recoverable(CutError::Recoverable(err)),
            Self::Fatal(err) => SnackError::Recoverable(CutError::Fatal(err)),
            Self::NotEnough { needed, span } => SnackError::NotEnough { needed, span },
        }
    }
}
impl<E1: BoxableParseError<S>, E2: BoxableParseError<S>, S: Clone> SnackError<E1, E2, S> {
    /// Boxes the two errors in the SnackError.
    ///
    /// This is different from calling [`BoxableParseError::boxed()`] on the SnackError, as that
    /// boxes the error as a whole, whereas this function boxes its innards.
    ///
    /// # Returns
    /// An identical SnackError that has box `E1` and `E2` replaced with [`BoxedParseError`]s.
    #[inline]
    pub fn into_boxed<'e1, 'e2>(self) -> SnackError<BoxedParseError<'e1, S>, BoxedParseError<'e2, S>, S>
    where
        E1: 'e1,
        E2: 'e2,
    {
        match self {
            Self::Recoverable(err) => SnackError::Recoverable(err.boxed()),
            Self::Fatal(err) => SnackError::Fatal(err.boxed()),
            Self::NotEnough { needed, span } => SnackError::NotEnough { needed, span },
        }
    }
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
impl<'a, O: ExpectsFormatter, S: Spannable<'a>> Error for Expected<O, S> {}
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
impl<'a, E: Error, S: Spannable<'a>> Error for SpanningError<E, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> { self.err.source() }
}
impl<E, S: Clone> Spanning<S> for SpanningError<E, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}
