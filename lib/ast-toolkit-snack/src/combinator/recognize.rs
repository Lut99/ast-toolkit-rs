//  RECOGNIZE.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 13:58:07
//  Last edited:
//    07 Mar 2025, 14:23:23
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`recognize()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::Span;
use ast_toolkit_span::range::SpanRange;

use crate::Combinator;
use crate::result::Result as SResult;


/***** COMBINATORS *****/
/// Actual implementation of the [`recognize()`]-combinator.
pub struct Recognize<C, F, S> {
    comb: C,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C, F, S> Combinator<'t, F, S> for Recognize<C, F, S>
where
    F: Clone,
    S: Clone,
    C: Combinator<'t, F, S>,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = Span<F, S>;
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
        self.comb.parse(input.clone()).map(|(rem, _)| match rem.range() {
            SpanRange::Closed(s, _) | SpanRange::ClosedOpen(s) => {
                (rem, Span::ranged(input.from_ref().clone(), input.source_ref().clone(), offset..s))
            },
            SpanRange::OpenClosed(_) | SpanRange::Open => (rem, Span::ranged(input.from_ref().clone(), input.source_ref().clone(), offset..offset)),
            SpanRange::Empty => (rem, input),
        })
    }
}





/***** LIBRARY *****/
/// Uses the given combinator to parse the input, but always returns the parsed input instead of
/// what the combinator returns.
///
/// This can be used to "dumb down" complex combinators that return complex types.
///
/// # Arguments
/// - `comb`: The [`Combinator`] to negate.
///
/// # Returns
/// A combinator [`Recognize`] that copies the behaviour of a given `comb`inator, but returns a
/// [`Span`] instead.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::recognize;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::sequence::pair;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = recognize(pair(tag("Hello, "), tag("world!")));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(13..), span1.slice(..13))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(pair::Recoverable::Comb0(tag::Recoverable {
///         tag:  "Hello, ",
///         span: span2,
///     })))
/// );
/// ```
#[inline]
pub const fn recognize<'t, C, F, S>(comb: C) -> Recognize<C, F, S>
where
    F: Clone,
    S: Clone,
    C: Combinator<'t, F, S>,
{
    Recognize { comb, _f: PhantomData, _s: PhantomData }
}
