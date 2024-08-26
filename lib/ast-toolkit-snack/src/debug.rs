//  DEBUG.rs
//    by Lut99
//
//  Created:
//    26 Aug 2024, 15:05:07
//  Last edited:
//    26 Aug 2024, 15:10:15
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators useful for debugging.
//

use std::marker::PhantomData;

use ast_toolkit_span::Span;

use crate::{Combinator, Expects, Result as SResult};


/***** FUNCTIONS *****/
/// Runs the other combinator but only after some function has been called.
///
/// This useful when you only want to inspect input in the midst of a large combinator chain.
///
/// # Arguments
/// - `comb`: Some combinator to apply and then to discard the output of.
/// - `call`: Some function or closure to call to inspect the input to `comb`.
///
/// # Returns
/// A combinator [`Inspect`] that will apply `comb` after running `call`.
///
/// # Fails
/// The returned combinator fails exactly when `comb` fails.
///
/// # Exampel
/// ```rust
/// use ast_toolkit_snack::debug::inspect;
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = inspect(tag("Hello"), |input| println!("{input:?}"));
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::TagUtf8 { .. }))));
/// ```
#[inline]
pub const fn inspect<'t, F, S, C, I>(comb: C, call: I) -> Inspect<F, S, C, I>
where
    C: Combinator<'t, F, S>,
    I: for<'a> FnMut(&'a Span<F, S>),
{
    Inspect { comb, call, _f: PhantomData, _s: PhantomData }
}





/***** COMBINATORS *****/
/// The concrete combinator returned by [`inspect()`].
pub struct Inspect<F, S, C, I> {
    /// The combinator to maybe apply.
    comb: C,
    /// The closure to call before executing `comb`.
    call: I,
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f:   PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<'t, F, S, C: Expects<'t>, I> Expects<'t> for Inspect<F, S, C, I> {
    type Formatter = C::Formatter;

    #[inline]
    fn expects(&self) -> Self::Formatter { self.comb.expects() }
}
impl<'t, F, S, C, I> Combinator<'t, F, S> for Inspect<F, S, C, I>
where
    C: Combinator<'t, F, S>,
    I: for<'a> FnMut(&'a Span<F, S>),
{
    type Output = C::Output;
    type Error = C::Error;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<'t, Self::Output, F, S, Self::Error> {
        // Call inspect first
        (self.call)(&input);

        // Then call the combinator
        self.comb.parse(input)
    }
}
