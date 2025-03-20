//  REMEMBER.rs
//    by Lut99
//
//  Created:
//    07 Mar 2025, 17:35:03
//  Last edited:
//    20 Mar 2025, 12:07:40
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`remember()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::Span;

use crate::Combinator;
use crate::result::Result as SResult;
use crate::span::Parsable;


/***** COMBINATORS *****/
/// Actual implementation of the [`remember()`]-combinator.
pub struct Remember<C, S> {
    comb: C,
    _s:   PhantomData<S>,
}
impl<'t, C, S> Combinator<'t, S> for Remember<C, S>
where
    C: Combinator<'t, S>,
    S: Clone + Parsable,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = (C::Output, Span<S>);
    type Recoverable = C::Recoverable;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Run the combinator, but we map the output to inject the relative complement between the
        // two functions (computes the "parsed" part between them)
        self.comb.parse(input.clone()).map(|(rem, res)| match input.range().relative_complement(rem.range()) {
            Some(range) => (rem, (res, Span::ranged(input.into_source(), range))),
            None => panic!(
                "Failed to compute the relative complement after parsing with {}.\n\nThis can happen if:\n - The Span returned has an early start \
                 bound than the input Span; or\n - The Span returned is out-of-bounds of the input Span.",
                std::any::type_name::<C>()
            ),
        })
    }
}





/***** LIBRARY *****/
/// Uses the given combinator to parse the input while also returning the parsed section as a span.
///
/// # Arguments
/// - `comb`: The [`Combinator`] to negate.
///
/// # Returns
/// A combinator [`Remember`] that copies the behaviour of a given `comb`inator, but returns a
/// tuple with the parsed output and the consumed [`Span`] instead.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::branch::alt;
/// use ast_toolkit_snack::combinator::{map, remember};
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::sequence::pair;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// #[derive(Debug, PartialEq)]
/// enum Greeting {
///     Hello,
///     Goodbye,
/// }
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb = remember(alt((
///     map(tag("Hello"), |_| Greeting::Hello),
///     map(tag("Goodbye"), |_| Greeting::Goodbye),
/// )));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), (Greeting::Hello, span1.slice(..5)))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(7..), (Greeting::Goodbye, span2.slice(..7)))));
/// ```
#[inline]
pub const fn remember<'t, C, S>(comb: C) -> Remember<C, S>
where
    C: Combinator<'t, S>,
    S: Clone + Parsable,
{
    Remember { comb, _s: PhantomData }
}
