//  MAP.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:57:10
//  Last edited:
//    22 Apr 2025, 11:43:23
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`map()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::Combinator;
use crate::result::Result as SResult;
use crate::span::Parsable;


/***** COMBINATORS *****/
/// Actual implementation of the [`map()`]-combinator.
pub struct Map<C, P, S> {
    /// The nested combinator who's result we're mapping.
    comb: C,
    /// The predicate that does the mapping.
    pred: P,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, P, O1, O2, S> Combinator<'c, 's, S> for Map<C, P, S>
where
    C: Combinator<'c, 's, S, Output = O1>,
    P: FnMut(O1) -> O2,
    S: Clone + Spannable<'s>,
    S::Slice: Parsable<'s>,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = O2;
    type Recoverable = C::Recoverable;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
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
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::map;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// #[derive(Debug, PartialEq)]
/// struct Hello {
///     span: Span<&'static str>,
/// };
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb = map(tag("Hello"), |parsed| Hello { span: parsed });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), Hello { span: span1.slice(..5) })));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: "Hello", span: span2 }))
/// );
/// ```
#[inline]
pub const fn map<'c, 's, C, P, O1, O2, S>(comb: C, pred: P) -> Map<C, P, S>
where
    C: Combinator<'c, 's, S, Output = O1>,
    P: FnMut(O1) -> O2,
    S: Clone + Spannable<'s>,
    S::Slice: Parsable<'s>,
{
    Map { comb, pred, _s: PhantomData }
}
