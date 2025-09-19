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

use ast_toolkit_span::{Span, Spannable, Spanning, SpanningInf, SpanningMut, SpanningRef};

use crate::boxed::{BoxableParseError, BoxedParseError};
use crate::{ExpectsFormatter, ParseError};


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
    fn get_span(&self) -> Option<Cow<'_, Span<S>>> {
        match self {
            Self::Recoverable(err) => err.get_span(),
            Self::Fatal(err) => err.get_span(),
        }
    }

    #[inline]
    fn take_span(self) -> Option<Span<S>> {
        match self {
            Self::Recoverable(err) => err.take_span(),
            Self::Fatal(err) => err.take_span(),
        }
    }
}
impl<E1: SpanningInf<S>, E2: SpanningInf<S>, S: Clone> SpanningInf<S> for CutError<E1, E2> {
    #[inline]
    fn span(&self) -> Cow<'_, Span<S>> {
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
impl<E1: SpanningRef<S>, E2: SpanningRef<S>, S: Clone> SpanningRef<S> for CutError<E1, E2> {
    #[inline]
    fn span_ref(&self) -> &Span<S> {
        match self {
            Self::Recoverable(err) => err.span_ref(),
            Self::Fatal(err) => err.span_ref(),
        }
    }
}
impl<E1: SpanningMut<S>, E2: SpanningMut<S>, S: Clone> SpanningMut<S> for CutError<E1, E2> {
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> {
        match self {
            Self::Recoverable(err) => err.span_mut(),
            Self::Fatal(err) => err.span_mut(),
        }
    }
}
impl<E1: ParseError<S>, E2: ParseError<S>, S: Clone> ParseError<S> for CutError<E1, E2> {
    #[inline]
    #[track_caller]
    fn more_might_fix(&self) -> bool {
        match self {
            Self::Recoverable(err) => err.more_might_fix(),
            Self::Fatal(err) => err.more_might_fix(),
        }
    }

    #[inline]
    #[track_caller]
    fn needed_to_fix(&self) -> Option<usize> {
        match self {
            Self::Recoverable(err) => err.needed_to_fix(),
            Self::Fatal(err) => err.needed_to_fix(),
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
    fn cut(self) -> Result<T, Infallible, CutError<E1, E2>, S>;

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
    fn uncut(self) -> Result<T, CutError<E1, E2>, Infallible, S>;



    /// Maps the recoverable error in the [`SnackError`] embedded in this Result to another one.
    ///
    /// # Arguments
    /// - `map`: Some [`FnOnce`] that does the mapping.
    ///
    /// # Returns
    /// A Result that, if it was an [`Err`] with [`SnackError::Recoverable`], has the result of the
    /// `map` instead of the original error. If it was anything else, it is untouched.
    fn map_recoverable<E>(self, map: impl FnOnce(E1) -> E) -> Result<T, E, E2, S>;

    /// Maps the fatal error in the [`SnackError`] embedded in this Result to another one.
    ///
    /// # Arguments
    /// - `map`: Some [`FnOnce`] that does the mapping.
    ///
    /// # Returns
    /// A Result that, if it was an [`Err`] with [`SnackError::Fatal`], has the result of the `map`
    /// instead of the original error. If it was anything else, it is untouched.
    fn map_fatal<E>(self, map: impl FnOnce(E2) -> E) -> Result<T, E1, E, S>;
}

impl<T, E1, E2, S> ResultExt<T, E1, E2, S> for Result<T, E1, E2, S> {
    #[inline]
    fn cut(self) -> Result<T, Infallible, CutError<E1, E2>, S> {
        match self {
            Ok(res) => Ok(res),
            Err(err) => Err(err.cut()),
        }
    }

    #[inline]
    fn uncut(self) -> Result<T, CutError<E1, E2>, Infallible, S> {
        match self {
            Ok(res) => Ok(res),
            Err(err) => Err(err.uncut()),
        }
    }



    #[inline]
    fn map_recoverable<E>(self, map: impl FnOnce(E1) -> E) -> Result<T, E, E2, S> {
        match self {
            Ok(res) => Ok(res),
            Err(err) => Err(err.map_recoverable(map)),
        }
    }

    #[inline]
    fn map_fatal<E>(self, map: impl FnOnce(E2) -> E) -> Result<T, E1, E, S> {
        match self {
            Ok(res) => Ok(res),
            Err(err) => Err(err.map_fatal(map)),
        }
    }
}





/***** LIBRARY *****/
/// The return type of all snack [`Combinator`](crate::Combinator)s.
///
/// It is essentially a three-way return type but separated in two levels to use the stock [`Result`] (so that `?` works).
pub type Result<T, E1, E2, S> = std::result::Result<(Span<S>, T), SnackError<E1, E2>>;



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
pub enum SnackError<E1, E2> {
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
}
impl<E1, E2> SnackError<E1, E2> {
    /// "Cuts" this SnackError.
    ///
    /// This is a shortcut for using the [`cut()`](crate::error::cut())-combinator. In essence, it
    /// will turn all [recoverable](SnackError::Recoverable) errors into
    /// [fatal](SnackError::Fatal), ones, "cutting" the branching search.
    ///
    /// # Returns
    /// A SnackError which is either [`SnackError::Fatal`] or [`SnackError::NotEnough`].
    #[inline]
    pub fn cut(self) -> SnackError<Infallible, CutError<E1, E2>> {
        match self {
            Self::Recoverable(err) => SnackError::Fatal(CutError::Recoverable(err)),
            Self::Fatal(err) => SnackError::Fatal(CutError::Fatal(err)),
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
    pub fn uncut(self) -> SnackError<CutError<E1, E2>, Infallible> {
        match self {
            Self::Recoverable(err) => SnackError::Recoverable(CutError::Recoverable(err)),
            Self::Fatal(err) => SnackError::Recoverable(CutError::Fatal(err)),
        }
    }



    /// Maps the recoverable error in this SnackError to another one.
    ///
    /// # Arguments
    /// - `map`: Some [`FnOnce`] that does the mapping.
    ///
    /// # Returns
    /// A SnackError that, if it was a [`SnackError::Recoverable`], has the result of the `map`
    /// instead of the original error. If it was anything else, it is untouched.
    #[inline]
    pub fn map_recoverable<E>(self, map: impl FnOnce(E1) -> E) -> SnackError<E, E2> {
        match self {
            Self::Recoverable(err) => SnackError::Recoverable(map(err)),
            Self::Fatal(err) => SnackError::Fatal(err),
        }
    }

    /// Maps the fatal error in this SnackError to another one.
    ///
    /// # Arguments
    /// - `map`: Some [`FnOnce`] that does the mapping.
    ///
    /// # Returns
    /// A SnackError that, if it was a [`SnackError::Fatal`], has the result of the `map`
    /// instead of the original error. If it was anything else, it is untouched.
    #[inline]
    pub fn map_fatal<E>(self, map: impl FnOnce(E2) -> E) -> SnackError<E1, E> {
        match self {
            Self::Recoverable(err) => SnackError::Recoverable(err),
            Self::Fatal(err) => SnackError::Fatal(map(err)),
        }
    }
}
impl<E1, E2> SnackError<E1, E2> {
    /// Boxes the two errors in the SnackError.
    ///
    /// This is different from calling [`BoxableParseError::boxed()`] on the SnackError, as that
    /// boxes the error as a whole, whereas this function boxes its innards.
    ///
    /// # Returns
    /// An identical SnackError that has box `E1` and `E2` replaced with [`BoxedParseError`]s.
    #[inline]
    pub fn into_boxed<'e1, 'e2, S>(self) -> SnackError<BoxedParseError<'e1, S>, BoxedParseError<'e2, S>>
    where
        E1: 'e1 + BoxableParseError<S>,
        E2: 'e2 + BoxableParseError<S>,
        S: Clone,
    {
        match self {
            Self::Recoverable(err) => SnackError::Recoverable(err.boxed()),
            Self::Fatal(err) => SnackError::Fatal(err.boxed()),
        }
    }
}
impl<E1, E2> Display for SnackError<E1, E2>
where
    E1: Display,
    E2: Display,
{
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Recoverable(err) => err.fmt(f),
            Self::Fatal(err) => err.fmt(f),
            // Self::NotEnough { needed, .. } => {
            //     write!(f, "Unexpected end-of-file")?;
            //     if let Some(needed) = needed {
            //         write!(f, " (expected {needed} more input tokens)")?;
            //     }
            //     Ok(())
            // },
        }
    }
}
impl<E1: Error, E2: Error> Error for SnackError<E1, E2> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Recoverable(err) => err.source(),
            Self::Fatal(err) => err.source(),
        }
    }
}
impl<E1: Spanning<S>, E2: Spanning<S>, S: Clone> Spanning<S> for SnackError<E1, E2> {
    #[inline]
    fn get_span(&self) -> Option<Cow<'_, Span<S>>> {
        match self {
            Self::Recoverable(err) => err.get_span(),
            Self::Fatal(err) => err.get_span(),
        }
    }

    #[inline]
    fn take_span(self) -> Option<Span<S>> {
        match self {
            Self::Recoverable(err) => err.take_span(),
            Self::Fatal(err) => err.take_span(),
        }
    }
}
impl<E1: SpanningInf<S>, E2: SpanningInf<S>, S: Clone> SpanningInf<S> for SnackError<E1, E2> {
    #[inline]
    fn span(&self) -> Cow<'_, Span<S>> {
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
impl<E1: SpanningRef<S>, E2: SpanningRef<S>, S: Clone> SpanningRef<S> for SnackError<E1, E2> {
    #[inline]
    fn span_ref(&self) -> &Span<S> {
        match self {
            Self::Recoverable(err) => err.span_ref(),
            Self::Fatal(err) => err.span_ref(),
        }
    }
}
impl<E1: SpanningMut<S>, E2: SpanningMut<S>, S: Clone> SpanningMut<S> for SnackError<E1, E2> {
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> {
        match self {
            Self::Recoverable(err) => err.span_mut(),
            Self::Fatal(err) => err.span_mut(),
        }
    }
}
impl<E1: ParseError<S>, E2: ParseError<S>, S: Clone> ParseError<S> for SnackError<E1, E2> {
    #[inline]
    #[track_caller]
    fn more_might_fix(&self) -> bool {
        match self {
            Self::Recoverable(err) => err.more_might_fix(),
            Self::Fatal(err) => err.more_might_fix(),
        }
    }

    #[inline]
    #[track_caller]
    fn needed_to_fix(&self) -> Option<usize> {
        match self {
            Self::Recoverable(err) => err.needed_to_fix(),
            Self::Fatal(err) => err.needed_to_fix(),
        }
    }
}



/// Defines a common [recoverable](SnackError::Recoverable) error type that simply describes what
/// was expected.
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'s, F, S>, bound = (F: r#trait, S: Spannable<'s>))]
pub struct Expected<F, S> {
    /// The formatter that will tell us what was expected.
    pub fmt:     F,
    /// If not [`None`], then more bytes could fix this problem.
    ///
    /// The contents of the `Option` encodes an optional estimate for the minimum number of bytes
    /// that might fix this error.
    pub fixable: Option<Option<usize>>,
    /// The span that tells us where we expected it.
    pub span:    Span<S>,
}
impl<F: ExpectsFormatter, S> Display for Expected<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { <F as Display>::fmt(&self.fmt, f) }
}
impl<'a, F: ExpectsFormatter, S: Spannable<'a>> Error for Expected<F, S> {}
impl<F, S: Clone> Spanning<S> for Expected<F, S> {
    #[inline]
    fn get_span(&self) -> Option<Cow<'_, Span<S>>> { Some(Cow::Borrowed(&self.span)) }

    #[inline]
    fn take_span(self) -> Option<Span<S>> { Some(self.span) }
}
impl<F, S: Clone> SpanningInf<S> for Expected<F, S> {
    #[inline]
    fn span(&self) -> Cow<'_, Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}
impl<F, S: Clone> SpanningRef<S> for Expected<F, S> {
    #[inline]
    fn span_ref(&self) -> &Span<S> { &self.span }
}
impl<F, S: Clone> SpanningMut<S> for Expected<F, S> {
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> { &mut self.span }
}
impl<'s, F: ExpectsFormatter, S: Clone + Spannable<'s>> ParseError<S> for Expected<F, S> {
    #[inline]
    fn more_might_fix(&self) -> bool { self.fixable.is_some() }

    #[inline]
    fn needed_to_fix(&self) -> Option<usize> { self.fixable.flatten() }
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
/// is_parse_error(SpanningError {
///     err:     u32::from_str("a").unwrap_err(),
///     fixable: None,
///     span:    Span::new("a"),
/// });
/// ```
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'s,E, S>, bound = (E: r#trait, S: Spannable<'s>))]
pub struct SpanningError<E, S> {
    /// The error wrapped.
    pub err:     E,
    /// If not [`None`], then more bytes could fix this problem.
    ///
    /// The contents of the `Option` encodes an optional estimate for the minimum number of bytes
    /// that might fix this error.
    pub fixable: Option<Option<usize>>,
    /// The span that points to where it occurs.
    pub span:    Span<S>,
}
impl<E, S> SpanningError<E, S> {
    /// Constructs a new SpanningError around your normal `E`rror.
    ///
    /// This function will assume that more input does not fix your error. See
    /// [`SpanningError::new_fixable`] for more details.
    ///
    /// # Arguments
    /// - `err`: The `E`rror to wrap around.
    /// - `span`: Some [`Span`] indicating where in the source text the error occurred.
    ///
    /// # Returns
    /// A new [`SpanningError`] that is a [`ParseError`].
    #[inline]
    pub const fn new(err: E, span: Span<S>) -> Self { Self { err, fixable: None, span } }

    /// Constructs a new SpanningError around your normal `E`rror, with additional information that
    /// it may be fixed when more input is shown. This is practical to do when your error is
    /// essentially an "unexpected-end-of-file" so that users that get their implementation
    /// incrementally know to get more to potentially fix this error.
    ///
    /// # Arguments
    /// - `err`: The `E`rror to wrap around.
    /// - `min_elems`: An estimate of the _minimum_ number of additional elements to fetch
    ///   such that the error _may_ be fixed. If this is unknown, then [`None`] should be given.
    /// - `span`: Some [`Span`] indicating where in the source text the error occurred.
    ///
    /// # Returns
    /// A new [`SpanningError`] that is a [`ParseError`].
    #[inline]
    pub const fn new_fixable(err: E, min_elems: Option<usize>, span: Span<S>) -> Self { Self { err, fixable: Some(min_elems), span } }
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
    fn get_span(&self) -> Option<Cow<'_, Span<S>>> { Some(Cow::Borrowed(&self.span)) }

    #[inline]
    fn take_span(self) -> Option<Span<S>> { Some(self.span) }
}
impl<E, S: Clone> SpanningInf<S> for SpanningError<E, S> {
    #[inline]
    fn span(&self) -> Cow<'_, Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}
impl<E, S: Clone> SpanningRef<S> for SpanningError<E, S> {
    #[inline]
    fn span_ref(&self) -> &Span<S> { &self.span }
}
impl<E, S: Clone> SpanningMut<S> for SpanningError<E, S> {
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> { &mut self.span }
}
impl<'s, E: Error, S: Clone + Spannable<'s>> ParseError<S> for SpanningError<E, S> {
    #[inline]
    fn more_might_fix(&self) -> bool { self.fixable.is_some() }

    #[inline]
    fn needed_to_fix(&self) -> Option<usize> { self.fixable.flatten() }
}
