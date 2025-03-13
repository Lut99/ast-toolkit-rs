//  REMEMBER.rs
//    by Lut99
//
//  Created:
//    07 Mar 2025, 17:35:03
//  Last edited:
//    07 Mar 2025, 17:42:57
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`remember()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::Span;
use ast_toolkit_span::range::SpanRange;

use crate::Combinator;
use crate::result::Result as SResult;


/***** COMBINATORS *****/
/// Actual implementation of the [`remember()`]-combinator.
pub struct Remember<C, F, S> {
    comb: C,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C, F, S> Combinator<'t, F, S> for Remember<C, F, S>
where
    C: Combinator<'t, F, S>,
    F: Clone,
    S: Clone,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = (C::Output, Span<F, S>);
    type Recoverable = C::Recoverable;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        // Get some initial span offset
        let offset: usize = match input.range() {
            SpanRange::Closed(s, _) | SpanRange::ClosedOpen(s) => s,
            SpanRange::OpenClosed(_) | SpanRange::Open | SpanRange::Empty => 0,
        };

        // Run the combinator
        self.comb.parse(input.clone()).map(|(rem, res)| match rem.range() {
            SpanRange::Closed(s, _) | SpanRange::ClosedOpen(s) => {
                (rem, (res, Span::ranged(input.from_ref().clone(), input.source_ref().clone(), offset..s)))
            },
            SpanRange::OpenClosed(_) | SpanRange::Open => {
                (rem, (res, Span::ranged(input.from_ref().clone(), input.source_ref().clone(), offset..offset)))
            },
            SpanRange::Empty => (rem, (res, input)),
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
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = remember(alt((
///     map(tag("Hello"), |_| Greeting::Hello),
///     map(tag("Goodbye"), |_| Greeting::Goodbye),
/// )));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), (Greeting::Hello, span1.slice(..5)))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(7..), (Greeting::Goodbye, span2.slice(..7)))));
/// ```
#[inline]
pub const fn remember<'t, C, F, S>(comb: C) -> Remember<C, F, S>
where
    C: Combinator<'t, F, S>,
    F: Clone,
    S: Clone,
{
    Remember { comb, _f: PhantomData, _s: PhantomData }
}
