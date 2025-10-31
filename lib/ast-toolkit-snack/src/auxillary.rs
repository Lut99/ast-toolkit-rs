//  AUXILLARY.rs
//    by Lut99
//
//  Description:
//!   Defines some auxillary utilities that are aiding in the use of the main
//!   [`spec`](crate::spec) ones.
//

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};

use ast_toolkit_loc::{Loc, Located};

use crate::spec::{ExpectsFormatter, ParseError, Result as SResult};


/***** ERROR TYPES *****/
/// Defines a common [recoverable](crate::spec::SnackError::Recoverable) error type that simply
/// describes what was expected.
#[derive(Debug, Eq, PartialEq)]
pub struct Expected<F> {
    /// The formatter that will tell us what was expected.
    pub fmt:     F,
    /// If not [`None`], then more bytes could fix this problem.
    ///
    /// The contents of the `Option` encodes an optional estimate for the minimum number of bytes
    /// that might fix this error.
    pub fixable: Option<Option<usize>>,
    /// The location that tells us where we expected it.
    pub loc:     Loc,
}
impl<F: ExpectsFormatter> Display for Expected<F> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { <F as Display>::fmt(&self.fmt, f) }
}
impl<'a, F: ExpectsFormatter> Error for Expected<F> {}
impl<F> Located for Expected<F> {
    #[inline]
    fn loc(&self) -> Loc { self.loc }
}
impl<F: ExpectsFormatter> ParseError for Expected<F> {
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
#[derive(Debug, Eq, PartialEq)]
pub struct SpanningError<E> {
    /// The error wrapped.
    pub err:     E,
    /// If not [`None`], then more bytes could fix this problem.
    ///
    /// The contents of the `Option` encodes an optional estimate for the minimum number of bytes
    /// that might fix this error.
    pub fixable: Option<Option<usize>>,
    /// The location that points to where it occurs.
    pub loc:     Loc,
}
impl<E> SpanningError<E> {
    /// Constructs a new SpanningError around your normal `E`rror.
    ///
    /// This function will assume that more input does not fix your error. See
    /// [`SpanningError::new_fixable`] for more details.
    ///
    /// # Arguments
    /// - `err`: The `E`rror to wrap around.
    /// - `loc`: Some [`Loc`] indicating where in the source text the error occurred.
    ///
    /// # Returns
    /// A new SpanningError that is a [`ParseError`].
    #[inline]
    pub const fn new(err: E, loc: Loc) -> Self { Self { err, fixable: None, loc } }

    /// Constructs a new SpanningError around your normal `E`rror, with additional information that
    /// it may be fixed when more input is shown. This is practical to do when your error is
    /// essentially an "unexpected-end-of-file" so that users that get their implementation
    /// incrementally know to get more to potentially fix this error.
    ///
    /// # Arguments
    /// - `err`: The `E`rror to wrap around.
    /// - `min_elems`: An estimate of the _minimum_ number of additional elements to fetch
    ///   such that the error _may_ be fixed. If this is unknown, then [`None`] should be given.
    /// - `loc`: Some [`Loc`] indicating where in the source text the error occurred.
    ///
    /// # Returns
    /// A new SpanningError that is a [`ParseError`].
    #[inline]
    pub const fn new_fixable(err: E, min_elems: Option<usize>, loc: Loc) -> Self { Self { err, fixable: Some(min_elems), loc } }
}
impl<E: Display> Display for SpanningError<E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { self.err.fmt(f) }
}
impl<E: Error> Error for SpanningError<E> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> { self.err.source() }
}
impl<E> Located for SpanningError<E> {
    #[inline]
    fn loc(&self) -> Loc { self.loc }
}
impl<E: Error> ParseError for SpanningError<E> {
    #[inline]
    fn more_might_fix(&self) -> bool { self.fixable.is_some() }

    #[inline]
    fn needed_to_fix(&self) -> Option<usize> { self.fixable.flatten() }
}



/// Auxillary type for the [`SnackError`](crate::spec::SnackError) for when it is
/// [`cut()`](crate::spec::SnackError::cut()).
#[derive(Debug, Eq, PartialEq)]
pub enum CutError<E1, E2> {
    /// It was originally a [`SnackError::Recoverable`](crate::spec::SnackError::Recoverable)
    /// error.
    Recoverable(E1),
    /// It was originally also a [`SnackError::Fatal`](crate::spec::SnackError::Fatal) error.
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
impl<E1: Debug + Display, E2: Debug + Display> Error for CutError<E1, E2> {}
impl<E1: Located, E2: Located> Located for CutError<E1, E2> {
    #[inline]
    fn loc(&self) -> Loc {
        match self {
            Self::Recoverable(err) => err.loc(),
            Self::Fatal(err) => err.loc(),
        }
    }
}
impl<E1: ParseError, E2: ParseError> ParseError for CutError<E1, E2> {
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
/// Defines some [`SnackError`](crate::spec::SnackError)'s functions transparently on [`Result`]s.
pub trait ResultExt<'s, T, E1, E2, S>: Sized {
    /// "Cuts" the [`SnackError`](crate::spec::SnackError) in this Result.
    ///
    /// This is a shortcut for using the [`cut()`](crate::error::cut())-combinator. In essence, it
    /// will turn all [recoverable](crate::spec::SnackError::Recoverable) errors into
    /// [fatal](crate::spec::SnackError::Fatal), ones, "cutting" the branching search.
    ///
    /// # Returns
    /// A Result with a [`SnackError::Fatal`](crate::spec::SnackError::Fatal) wrapping a
    /// [`CutError`].
    fn cut(self) -> SResult<'s, T, Infallible, CutError<E1, E2>, S>;

    /// "Uncuts" the [`SnackError`](crate::spec::SnackError) in this Result.
    ///
    /// This is a shortcut for using the [`uncut()`](crate::error::uncut())-combinator. In essence,
    /// it will turn all [fatal](crate::spec::SnackError::Fatal) errors into
    /// [recoverable](crate::spec::SnackError::Recoverable) ones, "catching" the fatal error and
    /// instead allowing parent branches to be searched again.
    ///
    /// # Returns
    /// A Result with a [`SnackError::Recoverable`](crate::spec::SnackError::Recoverable) wrapping
    /// a [`CutError`].
    fn uncut(self) -> SResult<'s, T, CutError<E1, E2>, Infallible, S>;



    /// Maps the recoverable error in the [`SnackError`](crate::spec::SnackError) embedded in this
    /// Result to another one.
    ///
    /// # Arguments
    /// - `map`: Some [`FnOnce`] that does the mapping.
    ///
    /// # Returns
    /// A Result that, if it was an [`Err`] with
    /// [`SnackError::Recoverable`](crate::spec::SnackError::Recoverable), has the result of the
    /// `map` instead of the original error. If it was anything else, it is untouched.
    fn map_recoverable<E>(self, map: impl FnOnce(E1) -> E) -> SResult<'s, T, E, E2, S>;

    /// Maps the fatal error in the [`SnackError`](crate::spec::SnackError) embedded in this Result
    /// to another one.
    ///
    /// # Arguments
    /// - `map`: Some [`FnOnce`] that does the mapping.
    ///
    /// # Returns
    /// A Result that, if it was an [`Err`] with
    /// [`SnackError::Fatal`](crate::spec::SnackError::Fatal), has the result of the `map` instead
    /// of the original error. If it was anything else, it is untouched.
    fn map_fatal<E>(self, map: impl FnOnce(E2) -> E) -> SResult<'s, T, E1, E, S>;
}

impl<'s, T, E1, E2, S> ResultExt<'s, T, E1, E2, S> for SResult<'s, T, E1, E2, S> {
    #[inline]
    fn cut(self) -> SResult<'s, T, Infallible, CutError<E1, E2>, S> {
        match self {
            Ok(res) => Ok(res),
            Err(err) => Err(err.cut()),
        }
    }

    #[inline]
    fn uncut(self) -> SResult<'s, T, CutError<E1, E2>, Infallible, S> {
        match self {
            Ok(res) => Ok(res),
            Err(err) => Err(err.uncut()),
        }
    }



    #[inline]
    fn map_recoverable<E>(self, map: impl FnOnce(E1) -> E) -> SResult<'s, T, E, E2, S> {
        match self {
            Ok(res) => Ok(res),
            Err(err) => Err(err.map_recoverable(map)),
        }
    }

    #[inline]
    fn map_fatal<E>(self, map: impl FnOnce(E2) -> E) -> SResult<'s, T, E1, E, S> {
        match self {
            Ok(res) => Ok(res),
            Err(err) => Err(err.map_fatal(map)),
        }
    }
}
