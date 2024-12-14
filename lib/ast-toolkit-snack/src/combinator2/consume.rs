//  CONSUME.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:22:15
//  Last edited:
//    14 Dec 2024, 19:35:52
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`consume()`]-combinator.
//

use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, SpannableEq, Spanning};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator2, ExpectsFormatter};


/***** ERRORS *****/
/// Defines the errors emitted by [`Consume`].
pub enum ConsumeRecoverable<E, F, S> {
    /// The nested combinator failed.
    Comb(E),
    /// There was input left.
    RemainingInput { span: Span<F, S> },
}
impl<E: Debug, F, S> Debug for ConsumeRecoverable<E, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Comb(err) => {
                let mut fmt = f.debug_tuple("ConsumeRecoverable::Comb");
                fmt.field(err);
                fmt.finish()
            },
            Self::RemainingInput { span } => {
                let mut fmt = f.debug_struct("ConsumeRecoverable::RemainingInput");
                fmt.field("span", span);
                fmt.finish()
            },
        }
    }
}
impl<E: Display, F, S> Display for ConsumeRecoverable<E, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Comb(err) => <E as Display>::fmt(err, f),
            Self::RemainingInput { .. } => write!(f, "Not all input was successfully parsed."),
        }
    }
}
impl<E: Error, F, S> Error for ConsumeRecoverable<E, F, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Comb(err) => <E as Error>::source(err),
            Self::RemainingInput { .. } => None,
        }
    }
}
impl<E: Spanning<F, S>, F: Clone, S: Clone> Spanning<F, S> for ConsumeRecoverable<E, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::Comb(err) => err.span(),
            Self::RemainingInput { span } => span.clone(),
        }
    }

    #[inline]
    fn into_span(self) -> Span<F, S> {
        match self {
            Self::Comb(err) => err.into_span(),
            Self::RemainingInput { span } => span,
        }
    }
}
impl<E: Eq, F, S: SpannableEq> Eq for ConsumeRecoverable<E, F, S> {}
impl<E: PartialEq, F, S: SpannableEq> PartialEq for ConsumeRecoverable<E, F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Comb(lhs), Self::Comb(rhs)) => lhs == rhs,
            (Self::RemainingInput { span: lhs }, Self::RemainingInput { span: rhs }) => lhs == rhs,
            _ => false,
        }
    }
}




/***** FORMATTERS *****/
/// Expects formatter for the [`Consume`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ConsumeExpectsFormatter<F> {
    /// The nested combinator is what we're expecting.
    pub fmt: F,
}
impl<F: ExpectsFormatter> Display for ConsumeExpectsFormatter<F> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<F: ExpectsFormatter> ExpectsFormatter for ConsumeExpectsFormatter<F> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        self.fmt.expects_fmt(f, indent)?;
        write!(f, " (and nothing else)")
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`consume()`]-combinator.
#[derive(Debug)]
pub struct Consume<C, F, S> {
    comb: C,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C, F, S> Combinator2<'t, F, S> for Consume<C, F, S>
where
    C: Combinator2<'t, F, S>,
    S: Spannable,
{
    type ExpectsFormatter = ConsumeExpectsFormatter<C::ExpectsFormatter>;
    type Output = C::Output;
    type Recoverable = ConsumeRecoverable<C::Recoverable, F, S>;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ConsumeExpectsFormatter { fmt: self.comb.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        // First, parse the combinator as usual
        let (rem, res): (Span<F, S>, C::Output) = match self.comb.parse(input) {
            Ok(res) => res,
            Err(SnackError::Recoverable(err)) => return Err(SnackError::Recoverable(ConsumeRecoverable::Comb(err))),
            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(err)),
            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
        };

        // Then assert that nothing is remaining
        if rem.is_empty() { Result::Ok((rem, res)) } else { Err(SnackError::Recoverable(ConsumeRecoverable::RemainingInput { span: rem })) }
    }
}





/***** LIBRARY *****/
/// Matches the full input text.
///
/// Note that this version could be counted as part of the `complete`-suite. There is no streaming
/// counterpart, though, because consuming all input while more may be expected doesn't make too
/// much sense.
///
/// # Arguments
/// - `comb`: Some combinator to apply to the input text in order to match it.
///
/// # Returns
/// A combinator [`Consume`] that will apply `comb`.
///
/// # Fails
/// The returned combinator fails if `comb` fails, or if there is any input left after `comb` has
/// parsed from the input.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::combinator2::consume;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello");
/// let span2 = Span::new("<example>", "Hello, world!");
/// let span3 = Span::new("<example>", "Hey");
///
/// let mut comb = consume(tag("Hello"));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(consume::ConsumeRecoverable::RemainingInput {
///         span: span2.slice(5..),
///     }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(consume::ConsumeRecoverable::Comb(tag::TagRecoverable {
///         tag:  "Hello",
///         span: span3,
///     })))
/// );
/// ```
#[inline]
pub const fn consume<'t, C, F, S>(comb: C) -> Consume<C, F, S>
where
    C: Combinator2<'t, F, S>,
    S: Spannable,
{
    Consume { comb, _f: PhantomData, _s: PhantomData }
}
