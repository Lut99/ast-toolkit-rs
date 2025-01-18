//  MAP.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:57:10
//  Last edited:
//    03 Nov 2024, 19:27:58
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`map()`]-combinator.
//

use std::marker::PhantomData;

use crate::Combinator2;
use crate::result::Result as SResult;


/***** COMBINATORS *****/
/// Actual implementation of the [`map()`]-combinator.
pub struct Map<C, P, F, S> {
    /// The nested combinator who's result we're mapping.
    comb: C,
    /// The predicate that does the mapping.
    pred: P,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C, P, O1, O2, F, S> Combinator2<'t, F, S> for Map<C, P, F, S>
where
    C: Combinator2<'t, F, S, Output = O1>,
    P: FnMut(O1) -> O2,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = O2;
    type Recoverable = C::Recoverable;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: ast_toolkit_span::Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        match self.comb.parse(input) {
            Ok((rem, res)) => Ok((rem, (self.pred)(res))),
            Err(err) => Err(err),
        }
    }
}





/***** LIBRARY *****/
/// Maps the result of a combinator to something else.
///
/// # Arguments
/// - `comb`: Some combinator to run.
/// - `pred`: Some closure that takes the `comb`'s result and maps it to something else.
///
/// # Returns
/// A combinator [`Map`] that runs the given `comb`inator, and then maps the result using `pred`.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::combinator2::map;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::tag;
/// use ast_toolkit_span::Span;
///
/// #[derive(Debug, PartialEq)]
/// struct Hello {
///     span: Span<&'static str, &'static str>,
/// };
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = map(tag("Hello"), |parsed| Hello { span: parsed });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), Hello { span: span1.slice(..5) })));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: "Hello", span: span2 }))
/// );
/// ```
#[inline]
pub const fn map<'t, C, P, O1, O2, F, S>(comb: C, pred: P) -> Map<C, P, F, S>
where
    C: Combinator2<'t, F, S, Output = O1>,
    P: FnMut(O1) -> O2,
{
    Map { comb, pred, _f: PhantomData, _s: PhantomData }
}
