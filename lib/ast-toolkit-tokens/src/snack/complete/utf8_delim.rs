//  UTF 8 DELIM.rs
//    by Lut99
//
//  Created:
//    13 Mar 2025, 20:38:17
//  Last edited:
//    13 Mar 2025, 22:06:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a parser for delimiting tokens.
//

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_snack::Combinator;
use ast_toolkit_snack::result::{Result as SResult, SnackError};
use ast_toolkit_snack::span::MatchBytes;
use ast_toolkit_snack::utf8::complete::tag;
use ast_toolkit_span::{Span, Spanning};
use better_derive::{Debug, Eq, PartialEq};

use crate::Utf8Delimiter;


/***** ERRORS *****/
/// Defines recoverable errors for the [`utf8_delim()`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub enum Recoverable<E, F, S> {
    /// Failed to parse the opening delimiter.
    OpenKeyword { what: &'static str, span: Span<F, S> },
    /// Failed to parse the bit in between the delimiters.
    Inner { err: E },
}
impl<E: Display, F, S> Display for Recoverable<E, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::OpenKeyword { what, .. } => write!(f, "Expected opening delimiter {what:?}"),
            Self::Inner { .. } => write!(f, "Failed to parse delimiter contents"),
        }
    }
}
impl<E: 'static + Error, F, S> Error for Recoverable<E, F, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::OpenKeyword { .. } => None,
            Self::Inner { err } => Some(err),
        }
    }
}
impl<E: Spanning<F, S>, F: Clone, S: Clone> Spanning<F, S> for Recoverable<E, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::OpenKeyword { span, .. } => span.clone(),
            Self::Inner { err } => err.span(),
        }
    }

    #[inline]
    fn into_span(self) -> Span<F, S> {
        match self {
            Self::OpenKeyword { span, .. } => span,
            Self::Inner { err } => err.into_span(),
        }
    }
}

/// Defines fatal errors for the [`utf8_delim()`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub enum Fatal<E, F, S> {
    /// The inner parser failed fatally.
    Inner { err: E },
    /// We failed to find the closing delimiter.
    CloseKeyword { what: &'static str, span: Span<F, S> },
}
impl<E: Display, F, S> Display for Fatal<E, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Inner { err } => err.fmt(f),
            Self::CloseKeyword { what, .. } => write!(f, "Expected closing delimiter {what:?}"),
        }
    }
}
impl<E: 'static + Error, F, S> Error for Fatal<E, F, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Inner { err } => Some(err),
            Self::CloseKeyword { .. } => None,
        }
    }
}
impl<E: Spanning<F, S>, F: Clone, S: Clone> Spanning<F, S> for Fatal<E, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::Inner { err } => err.span(),
            Self::CloseKeyword { span, .. } => span.clone(),
        }
    }

    #[inline]
    fn into_span(self) -> Span<F, S>
    where
        Self: Sized,
    {
        match self {
            Self::Inner { err } => err.into_span(),
            Self::CloseKeyword { span, .. } => span,
        }
    }
}





/***** EXPECTSFORMATTERS *****/
/// ExpectsFormatter for the [`utf8_delim()`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<O> {
    /// The opening character.
    pub left:  &'static str,
    /// The middle expects.
    pub inner: O,
    /// The closing character.
    pub right: &'static str,
}
impl<O: ast_toolkit_snack::ExpectsFormatter> Display for ExpectsFormatter<O> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as ast_toolkit_snack::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl<O: ast_toolkit_snack::ExpectsFormatter> ast_toolkit_snack::ExpectsFormatter for ExpectsFormatter<O> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        self.inner.expects_fmt(f, indent)?;
        write!(f, " delimited by '{}{}'", self.left, self.right)
    }
}





/***** COMBINATORS *****/
/// Implements the [`utf8_delim()`]-combinator.
pub struct Utf8Delim<T, C, F, S> {
    /// The type of token to parse.
    _t:   PhantomData<T>,
    /// The combinator that parses in between the parenthesis.
    comb: C,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, T, C, F, S> Combinator<'t, F, S> for Utf8Delim<T, C, F, S>
where
    T: Utf8Delimiter<F, S>,
    C: Combinator<'t, F, S>,
    F: Clone,
    S: Clone + MatchBytes,
{
    type ExpectsFormatter = ExpectsFormatter<C::ExpectsFormatter>;
    type Output = (C::Output, T);
    type Recoverable = Recoverable<C::Recoverable, F, S>;
    type Fatal = Fatal<C::Fatal, F, S>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { left: T::OPEN_TOKEN, inner: self.comb.expects(), right: T::CLOSE_TOKEN } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        // Attempt to parse the opening delimiter
        let (rem, open): (Span<F, S>, Span<F, S>) = match tag(T::OPEN_TOKEN).parse(input) {
            Ok(res) => res,
            Err(SnackError::Recoverable(err)) => {
                return Err(SnackError::Recoverable(Recoverable::OpenKeyword { what: T::OPEN_TOKEN, span: err.into_span() }));
            },
            Err(SnackError::Fatal(_) | SnackError::NotEnough { .. }) => unreachable!(),
        };

        // Attempt to parse the middle bit
        let (rem, inner): (Span<F, S>, C::Output) = match self.comb.parse(rem) {
            Ok(res) => res,
            Err(SnackError::Recoverable(err)) => return Err(SnackError::Recoverable(Recoverable::Inner { err })),
            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Inner { err })),
            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
        };

        // Finally, parse the closing delim
        match tag(T::CLOSE_TOKEN).parse(rem) {
            Ok((rem, close)) => Ok((rem, (inner, T::from((open, close))))),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Fatal(Fatal::CloseKeyword { what: T::CLOSE_TOKEN, span: err.into_span() })),
            Err(SnackError::Fatal(_) | SnackError::NotEnough { .. }) => unreachable!(),
        }
    }
}





/***** LIBRARY *****/
/// Parses something delimited by a [`Utf8Delimiter`](crate::Utf8Delimiter).
///
/// # Generics
/// - `T`: The [`Utf8Token`](crate::Utf8Token) to parse.
/// - `C`: The combinator that is used to match the delimited contents.
/// - `F`: The type of the From-string in input [`Span`]s.
/// - `S`: The type of the Source-string in input [`Span`]s.
///
/// # Arguments
/// - `comb`: A combinator that can be used to parse the content that is delimited by `T`.
///
/// # Returns
/// A combinator [`Utf8Delim`] that parses the delimiters and the bit in between.
///
/// # Fails
/// The returned combinator fails if the opening token was not found or if the given `comb` fails.
///
/// # Errors
/// The returned combinator fails unrecoverably if it failed to parse the closing delimiter after
/// parsing the opening delimiter and `comb`; or if `comb` fails fatally.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
/// use ast_toolkit_tokens::snack::complete::utf8_delim;
/// use ast_toolkit_tokens::{utf8_delim, utf8_delim_snack};
///
/// // Define the tokens
/// utf8_delim!(Parens, "(", ")");
/// utf8_delim_snack!(Parens);
///
/// // Let's test some inputs
/// let span1 = Span::new("<example>", "(foo)");
/// let span2 = Span::new("<example>", "(bar)");
/// let span3 = Span::new("<example>", "foo");
/// let span4 = Span::new("<example>", "(foo");
///
/// let mut comb1 = utf8_delim::<Parens<_, _>, _, _, _>(tag("foo"));
/// assert_eq!(
///     comb1.parse(span1),
///     Ok((
///         span1.slice(5..),
///         (span1.slice(1..4), Parens { open: span1.slice(0..1), close: span1.slice(4..5) })
///     ))
/// );
/// assert_eq!(
///     comb1.parse(span2),
///     Err(SnackError::Recoverable(utf8_delim::Recoverable::Inner {
///         err: tag::Recoverable { tag: "foo", span: span2.slice(1..) },
///     }))
/// );
///
/// // Alternative way of getting the same parser
/// let mut comb2 = Parens::parser(tag("foo"));
/// assert_eq!(
///     comb2.parse(span3),
///     Err(SnackError::Recoverable(utf8_delim::Recoverable::OpenKeyword {
///         what: "(",
///         span: span3,
///     }))
/// );
/// assert_eq!(
///     comb2.parse(span4),
///     Err(SnackError::Fatal(utf8_delim::Fatal::CloseKeyword {
///         what: ")",
///         span: span4.slice(4..),
///     }))
/// );
/// ```
#[inline]
pub const fn utf8_delim<'t, T, C, F, S>(comb: C) -> Utf8Delim<T, C, F, S>
where
    T: Utf8Delimiter<F, S>,
    C: Combinator<'t, F, S>,
    F: Clone,
    S: Clone + MatchBytes,
{
    Utf8Delim { _t: PhantomData, comb, _f: PhantomData, _s: PhantomData }
}
