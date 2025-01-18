//  SEPARATED PAIR.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 19:43:22
//  Last edited:
//    18 Jan 2025, 18:47:28
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`separated_pair()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::Span;

use super::tuple;
pub use super::tuple::{Error3 as Error, ExpectsFormatter3 as ExpectsFormatter, Fatal3 as Fatal, Recoverable3 as Recoverable};
use crate::Combinator2;
use crate::result::Result as SResult;


/***** LIBRARY *****/
// NOTE: Not a type alias, because this needs to change the interface (either introduce `'t` at the
// type level or else other arbitrary generics).
/// Actually implements the [`separated_pair()`]-combinator.
pub struct SeparatedPair<C1, C2, C3, F, S> {
    /// The left combinator, not to discard.
    left:   C1,
    /// The middle combinator, to discard.
    middle: C2,
    /// The right combinator, not to discard.
    right:  C3,
    _f:     PhantomData<F>,
    _s:     PhantomData<S>,
}
impl<'t, C1, C2, C3, F, S> Combinator2<'t, F, S> for SeparatedPair<C1, C2, C3, F, S>
where
    C1: Combinator2<'t, F, S>,
    C2: Combinator2<'t, F, S>,
    C3: Combinator2<'t, F, S>,
{
    type ExpectsFormatter = ExpectsFormatter<C1::ExpectsFormatter, C2::ExpectsFormatter, C3::ExpectsFormatter>;
    type Output = (C1::Output, C3::Output);
    type Recoverable = Recoverable<C1::Recoverable, C2::Recoverable, C3::Recoverable>;
    type Fatal = Fatal<C1::Fatal, C2::Fatal, C3::Fatal>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmts: (self.left.expects(), self.middle.expects(), self.right.expects()) } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        // We can rely on the `pair()`-combinator, we just discard as appropriate
        match tuple((&mut self.left, &mut self.middle, &mut self.right)).parse(input) {
            Ok((rem, (res1, _, res3))) => Ok((rem, (res1, res3))),
            Err(err) => Err(err),
        }
    }
}





/***** LIBRARY *****/
/// Applies the first combinator, then applies the second combinator, discards the latter's result,
/// and then applies the third combinator.
///
/// This is useful for parsing items separated by some third, insignificant item (e.g., `a/b`).
///
/// # Arguments
/// - `first`: The first combinator to match.
/// - `second`: The second combinator to match (and who's result to discard).
/// - `third`: The third combinator to match.
///
/// # Returns
/// A combinator [`SeparatedPair`] that will first apply the `first` combinator, then the `second` and
/// finally the `third`.
///
/// # Fails
/// The returned combinator fails if either the `first`, the `second` or the `third` combinator
/// fails.
///
/// Note that this short-circuits; e.g., the second combinator is never called if the first fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::sequence2::separated_pair;
/// use ast_toolkit_snack::utf82::complete::{digit1, tag};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello123Goodbye");
/// let span2 = Span::new("<example>", "123Goodbye");
/// let span3 = Span::new("<example>", "HelloWorld");
///
/// let mut comb = separated_pair(tag("Hello"), digit1(), tag("Goodbye"));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(15..), (span1.slice(..5), span1.slice(8..)))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(separated_pair::Recoverable::Comb0(tag::Recoverable {
///         tag:  "Hello",
///         span: span2,
///     })))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(separated_pair::Recoverable::Comb1(digit1::Recoverable {
///         fmt:  digit1::ExpectsFormatter,
///         span: span3.slice(5..),
///     })))
/// );
/// ```
#[inline]
pub const fn separated_pair<'t, C1, C2, C3, F, S>(first: C1, second: C2, third: C3) -> SeparatedPair<C1, C2, C3, F, S>
where
    C1: Combinator2<'t, F, S>,
    C2: Combinator2<'t, F, S>,
    C3: Combinator2<'t, F, S>,
{
    SeparatedPair { left: first, middle: second, right: third, _f: PhantomData, _s: PhantomData }
}
