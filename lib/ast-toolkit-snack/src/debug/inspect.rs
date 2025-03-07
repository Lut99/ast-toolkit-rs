//  INSPECT.rs
//    by Lut99
//
//  Created:
//    01 Dec 2024, 12:11:28
//  Last edited:
//    18 Jan 2025, 17:52:47
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`inspect()`]-operator.
//

use std::marker::PhantomData;

use ast_toolkit_span::Span;

use crate::Combinator;
use crate::result::Result as SResult;


/***** COMBINATORS *****/
/// Actual implementation of the [`inspect()`]-combinator.
pub struct Inspect<C, P, F, S> {
    /// The combinator to maybe apply.
    comb: C,
    /// The closure to call before executing `comb`.
    pred: P,
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f:   PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<'t, C, P, F, S> Combinator<'t, F, S> for Inspect<C, P, F, S>
where
    C: Combinator<'t, F, S>,
    P: for<'a> FnMut(&'a Span<F, S>),
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = C::Output;
    type Recoverable = C::Recoverable;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        // Call inspect first
        (self.pred)(&input);

        // Then call the combinator
        self.comb.parse(input)
    }
}





/***** LIBRARY *****/
/// Runs a given function on the input before passing that input to the given combinator.
///
/// This useful when you only want to inspect input in the midst of a large combinator chain.
///
/// # Arguments
/// - `comb`: Some combinator to apply.
/// - `pred`: Some closure to call to inspect the input to `comb`.
///
/// # Returns
/// A combinator [`Inspect`] that will apply `comb` after running `pred`.
///
/// # Fails
/// The returned combinator fails exactly when `comb` fails.
///
/// # Exampel
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::debug::inspect;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = inspect(tag("Hello"), |input| println!("{input:?}"));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: "Hello", span: span2 }))
/// );
/// ```
#[inline]
pub const fn inspect<'t, F, S, C, P>(comb: C, pred: P) -> Inspect<C, P, F, S>
where
    C: Combinator<'t, F, S>,
    P: for<'a> FnMut(&'a Span<F, S>),
{
    Inspect { comb, pred, _f: PhantomData, _s: PhantomData }
}
