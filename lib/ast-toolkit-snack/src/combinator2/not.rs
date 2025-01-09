//  NOT.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 19:38:26
//  Last edited:
//    09 Jan 2025, 20:33:20
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`not()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;
use ast_toolkit_span::range::SpanRange;

use crate::result::{Expected, Result as SResult, SnackError};
use crate::{Combinator2, ExpectsFormatter};


/***** FORMATTERS *****/
/// Expectsformatter for the [`Not`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct NotExpectsFormatter<F> {
    /// The nested formatter of the thing we _didn't_ expect.
    pub fmt: F,
}
impl<F: ExpectsFormatter> Display for NotExpectsFormatter<F> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<F: ExpectsFormatter> ExpectsFormatter for NotExpectsFormatter<F> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "not ")?;
        self.fmt.expects_fmt(f, indent)
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`not()`]-combinator.
pub struct Not<C, F, S> {
    comb: C,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C, F, S> Combinator2<'t, F, S> for Not<C, F, S>
where
    F: Clone,
    S: Clone,
    C: Combinator2<'t, F, S>,
{
    type ExpectsFormatter = NotExpectsFormatter<C::ExpectsFormatter>;
    type Output = ();
    type Recoverable = Expected<NotExpectsFormatter<C::ExpectsFormatter>, F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { NotExpectsFormatter { fmt: self.comb.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        match self.comb.parse(input.clone()) {
            Ok((rem, _)) => {
                // Before we return, inject the appropriate end into the start range
                // I.e., instead of 'start onwards', we get 'start -> end of parse'

                // Get the OG start
                let offset: usize = match input.range() {
                    SpanRange::Closed(s, _) | SpanRange::ClosedOpen(s) => s,
                    SpanRange::OpenClosed(_) | SpanRange::Open | SpanRange::Empty => 0,
                };

                // Inject the parsed end
                let span: Span<F, S> = match rem.range() {
                    SpanRange::Closed(s, _) | SpanRange::ClosedOpen(s) => {
                        Span::ranged(input.from_ref().clone(), input.source_ref().clone(), offset..s)
                    },
                    SpanRange::OpenClosed(_) | SpanRange::Open => Span::ranged(input.from_ref().clone(), input.source_ref().clone(), offset..offset),
                    SpanRange::Empty => input,
                };

                // OK, that's what we want
                Err(SnackError::Recoverable(Expected { fmt: self.expects(), span }))
            },
            Err(SnackError::Recoverable(_) | SnackError::Fatal(_)) => Ok((input, ())),
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
/// Note that fatal errors of the nested combinator are _not_ propagated. Instead, this is treated
/// as that the not-combinator succeeds.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::combinator2::not;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut tag = tag("Goodbye");
/// let mut comb = not(&mut tag);
/// assert_eq!(comb.parse(span1), Ok((span1, ())));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(not::NotRecoverable {
///         fmt:  tag.expects(),
///         span: span2.slice(..7),
///     }))
/// );
/// ```
#[inline]
pub const fn not<'t, C, F, S>(comb: C) -> Not<C, F, S>
where
    F: Clone,
    S: Clone,
    C: Combinator2<'t, F, S>,
{
    Not { comb, _f: PhantomData, _s: PhantomData }
}
