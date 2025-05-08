//  NONEMPTY.rs
//    by Lut99
//
//  Created:
//    08 May 2025, 15:50:25
//  Last edited:
//    08 May 2025, 16:20:17
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`nonempty()`]-combinator.
//

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{self, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, Spanning};
use better_derive::{Debug, Eq, PartialEq};

use super::remember;
use crate::Combinator;
use crate::result::{Result as SResult, SnackError};


/***** ERRORS *****/
/// Defines recoverable errors thrown by the [`NonEmpty`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub enum Recoverable<E, F, S> {
    /// The nested combinator failed.
    Comb(E),
    /// The nested combinator did not parse anything.
    Empty { fmt: F, span: Span<S> },
}
impl<E: Display, F: crate::ExpectsFormatter, S> Display for Recoverable<E, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Comb(err) => err.fmt(f),
            Self::Empty { fmt, .. } => write!(f, "{}", ExpectsFormatter { fmt }),
        }
    }
}
impl<'s, E: fmt::Debug + Display, F: crate::ExpectsFormatter, S: Spannable<'s>> Error for Recoverable<E, F, S> {}
impl<'s, E: Spanning<S>, F, S: Clone> Spanning<S> for Recoverable<E, F, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> {
        match self {
            Self::Comb(err) => err.span(),
            Self::Empty { span, .. } => Cow::Borrowed(span),
        }
    }

    #[inline]
    fn into_span(self) -> Span<S> {
        match self {
            Self::Comb(err) => err.into_span(),
            Self::Empty { span, .. } => span,
        }
    }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`NonEmpty`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<F> {
    /// The nested formatter
    pub fmt: F,
}
impl<F: crate::ExpectsFormatter> Display for ExpectsFormatter<F> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl<F: crate::ExpectsFormatter> crate::ExpectsFormatter for ExpectsFormatter<F> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        <F as crate::ExpectsFormatter>::expects_fmt(&self.fmt, f, indent)?;
        write!(f, " at least once")
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`nonempty()`]-combinator.
pub struct NonEmpty<C, S> {
    /// The combinator we wrap.
    comb: C,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, S> Combinator<'c, 's, S> for NonEmpty<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<C::ExpectsFormatter>;
    type Output = C::Output;
    type Recoverable = Recoverable<C::Recoverable, C::ExpectsFormatter, S>;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Parse while keeping track of a span
        let (rem, (res, span)): (Span<S>, (C::Output, Span<S>)) = match remember(&mut self.comb).parse(input.clone()) {
            Ok(res) => res,
            Err(SnackError::Recoverable(err)) => return Err(SnackError::Recoverable(Recoverable::Comb(err))),
            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(err)),
            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
        };

        // Assert that there is something there
        if span.is_empty() {
            return Err(SnackError::Recoverable(Recoverable::Empty { fmt: self.comb.expects(), span: input }));
        }

        // OK, neat
        Ok((rem, res))
    }
}





/***** LIBRARY *****/
/// A combinator that will fail if the given combinator did not parse anything.
///
/// Together with [`until()`](super::until()), this allows one to create
/// [`while1()`](crate::bytes::complete::while1())-like combinators, but then in the abstract.
///
/// # Arguments
/// - `comb`: Some other combinator to parse at least *something* of.
///
/// # Returns
/// A new combinator that will parse whatever `comb` parses.
///
/// # Fails
/// The returned combinator will fail whenever `comb` fails. In addition, it also fails if `comb`
/// did not parse anything.
///
/// # Examples
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::nonempty;
/// use ast_toolkit_snack::multi::many0;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("aaab");
/// let span2 = Span::new("b");
///
/// let mut comb = nonempty(many0(tag("a")));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(3..), vec![span1.slice(..1), span1.slice(1..2), span1.slice(2..3)]))
/// );
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(nonempty::Recoverable::Empty {
///         fmt:  many0::ExpectsFormatter { fmt: tag::ExpectsFormatter { tag: "a" } },
///         span: span2,
///     }))
/// );
/// ```
///
/// A use-case showing the propagation of recoverable error types:
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::nonempty;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("b");
///
/// let mut comb = nonempty(tag("a"));
/// assert_eq!(
///     comb.parse(span1),
///     Err(SnackError::Recoverable(nonempty::Recoverable::Comb(tag::Recoverable {
///         tag:  "a",
///         span: span1,
///     })))
/// );
/// ```
#[inline]
pub const fn nonempty<'c, 's, C, S>(comb: C) -> NonEmpty<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    NonEmpty { comb, _s: PhantomData }
}
