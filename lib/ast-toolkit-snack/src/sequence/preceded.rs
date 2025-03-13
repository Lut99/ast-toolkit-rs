//  PRECEDED.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 19:43:22
//  Last edited:
//    18 Jan 2025, 18:35:43
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`preceded()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::Span;

use super::pair;
pub use super::pair::{Error, ExpectsFormatter, Fatal, Recoverable};
use crate::Combinator;
use crate::result::Result as SResult;


/***** LIBRARY *****/
// NOTE: Not a type alias, because this needs to change the interface (either introduce `'t` at the
// type level or else other arbitrary generics).
/// Actually implements the [`preceded()`]-combinator.
pub struct Preceded<C1, C2, F, S> {
    /// The left combinator, to discard.
    left:  C1,
    /// The right combinator, not to discard.
    right: C2,
    _f:    PhantomData<F>,
    _s:    PhantomData<S>,
}
impl<'t, C1, C2, F, S> Combinator<'t, F, S> for Preceded<C1, C2, F, S>
where
    C1: Combinator<'t, F, S>,
    C2: Combinator<'t, F, S>,
{
    type ExpectsFormatter = ExpectsFormatter<C1::ExpectsFormatter, C2::ExpectsFormatter>;
    type Output = C2::Output;
    type Recoverable = Recoverable<C1::Recoverable, C2::Recoverable>;
    type Fatal = Fatal<C1::Fatal, C2::Fatal>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmts: (self.left.expects(), self.right.expects()) } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        // We can rely on the `pair()`-combinator, we just discard as appropriate
        match pair(&mut self.left, &mut self.right).parse(input) {
            Ok((rem, (_, res))) => Ok((rem, res)),
            Err(err) => Err(err),
        }
    }
}





/***** LIBRARY *****/
/// Applies the first combinator, discards the result, and then applies the second.
///
/// # Arguments
/// - `first`: The first combinator to match (and who's result to discard).
/// - `second`: The second combinator to match.
///
/// # Returns
/// A combinator [`Preceded`] that will first apply the `first` combinator, and then the `second`.
///
/// # Fails
/// The returned combinator fails if either the `first` or the `second` combinator fails.
///
/// Note that this short-circuits; the second combinator is never called if the first fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::sequence::preceded;
/// use ast_toolkit_snack::utf8::complete::{digit1, tag};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello123");
/// let span2 = Span::new("<example>", "123");
/// let span3 = Span::new("<example>", "HelloWorld");
///
/// let mut comb = preceded(tag("Hello"), digit1());
/// assert_eq!(comb.parse(span1), Ok((span1.slice(8..), span1.slice(5..8))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(preceded::Recoverable::Comb0(tag::Recoverable {
///         tag:  "Hello",
///         span: span2,
///     })))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(preceded::Recoverable::Comb1(digit1::Recoverable {
///         fmt:  digit1::ExpectsFormatter,
///         span: span3.slice(5..),
///     })))
/// );
/// ```
#[inline]
pub const fn preceded<'t, C1, C2, F, S>(first: C1, second: C2) -> Preceded<C1, C2, F, S>
where
    C1: Combinator<'t, F, S>,
    C2: Combinator<'t, F, S>,
{
    Preceded { left: first, right: second, _f: PhantomData, _s: PhantomData }
}
