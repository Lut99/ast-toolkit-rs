//  INSPECT.rs
//    by Lut99
//
//  Created:
//    01 Dec 2024, 12:11:28
//  Last edited:
//    20 Mar 2025, 12:52:00
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`inspect_after()`]-operator.
//

use std::marker::PhantomData;

use ast_toolkit_span::Span;

use crate::Combinator;
use crate::result::Result as SResult;
use crate::span::Parsable;


/***** COMBINATORS *****/
/// Actual implementation of the [`inspect_after()`]-combinator.
pub struct InspectAfter<C, P, S> {
    /// The combinator to maybe apply.
    comb: C,
    /// The closure to call after executing `comb`.
    pred: P,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<'t, C, P, S> Combinator<'t, S> for InspectAfter<C, P, S>
where
    C: Combinator<'t, S>,
    P: for<'a> FnMut(&'a SResult<C::Output, C::Recoverable, C::Fatal, S>),
    S: Clone + Parsable,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = C::Output;
    type Recoverable = C::Recoverable;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Call the combinator
        let res: SResult<C::Output, C::Recoverable, C::Fatal, S> = self.comb.parse(input);

        // Call the closure
        (self.pred)(&res);

        // OK, return
        res
    }
}





/***** LIBRARY *****/
/// Runs a given function on the output of the given combinator.
///
/// This useful when you only want to inspect output in the midst of a large combinator chain.
///
/// # Arguments
/// - `comb`: Some combinator to apply.
/// - `pred`: Some closure to call to inspect the output to `comb`.
///
/// # Returns
/// A combinator [`InspectAfter`] that will apply `comb` before running `pred`.
///
/// # Fails
/// The returned combinator fails exactly when `comb` fails.
///
/// # Exampel
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::debug::inspect_after;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb = inspect_after(tag("Hello"), |input| println!("{input:?}"));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: "Hello", span: span2 }))
/// );
/// ```
#[inline]
pub const fn inspect_after<'t, C, P, S>(comb: C, pred: P) -> InspectAfter<C, P, S>
where
    C: Combinator<'t, S>,
    P: for<'a> FnMut(&'a SResult<C::Output, C::Recoverable, C::Fatal, S>),
    S: Clone + Parsable,
{
    InspectAfter { comb, pred, _s: PhantomData }
}
