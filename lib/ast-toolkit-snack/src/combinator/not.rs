//  NOT.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 19:38:26
//  Last edited:
//    20 Mar 2025, 12:08:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`not()`]-combinator.
//

use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use super::recognize;
use crate::result::{Expected, Result as SResult, SnackError};
use crate::span::Parsable;
use crate::{Combinator, ExpectsFormatter as _};


/***** TYPE ALIASES *****/
/// The recoverable error returned by [`Not`].
pub type Recoverable<C, S> = Expected<ExpectsFormatter<C>, S>;





/***** FORMATTERS *****/
/// Expectsformatter for the [`Not`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<F> {
    /// The nested formatter of the thing we _didn't_ expect.
    pub fmt: F,
}
impl<F: crate::ExpectsFormatter> Display for ExpectsFormatter<F> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<F: crate::ExpectsFormatter> crate::ExpectsFormatter for ExpectsFormatter<F> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "not ")?;
        self.fmt.expects_fmt(f, indent)
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`not()`]-combinator.
pub struct Not<C, S> {
    comb: C,
    _s:   PhantomData<S>,
}
impl<'t, C, S> Combinator<'t, S> for Not<C, S>
where
    C: Combinator<'t, S>,
    S: Clone + Parsable,
{
    type ExpectsFormatter = ExpectsFormatter<C::ExpectsFormatter>;
    type Output = ();
    type Recoverable = Recoverable<C::ExpectsFormatter, S>;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match recognize(&mut self.comb).parse(input.clone()) {
            Ok((_, parsed)) => Err(SnackError::Recoverable(Recoverable { fmt: self.expects(), span: parsed })),
            Err(SnackError::Recoverable(_)) => Ok((input, ())),
            Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(err)),
            Err(SnackError::NotEnough { needed, span }) => Err(SnackError::NotEnough { needed, span }),
        }
    }
}





/***** LIBRARY *****/
/// Implements the reverse of a combinator.
///
/// Specifically, will return `Result::Ok(())` if the combinator [`Result::Fail`]s, or a [`Result::Fail`] if it [`Result::Ok`]'s.
///
/// # Arguments
/// - `comb`: The [`Combinator`] to negate.
///
/// # Returns
/// A combinator [`Not`] that will succeed (but match nothing) if the given `comb`inator fails.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator succeeds.
///
/// Note that fatal errors of the nested combinator _are_ propagated. This because they usually
/// encode explicitly incorrect states of the input, and as such, are always illegal (even when
/// parsing negatedly).
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::not;
/// use ast_toolkit_snack::error::cut;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut tag = tag("Goodbye");
/// let mut comb = not(&mut tag);
/// assert_eq!(comb.parse(span1), Ok((span1, ())));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(not::Recoverable {
///         fmt:  comb.expects(),
///         span: span2.slice(..7),
///     }))
/// );
///
/// let mut comb = not(cut(tag));
/// assert_eq!(
///     comb.parse(span1),
///     Err(SnackError::Fatal(cut::Fatal::Recoverable(tag::Recoverable {
///         tag:  "Goodbye",
///         span: span1,
///     })))
/// );
/// ```
#[inline]
pub const fn not<'t, C, S>(comb: C) -> Not<C, S>
where
    C: Combinator<'t, S>,
    S: Clone + Parsable,
{
    Not { comb, _s: PhantomData }
}
