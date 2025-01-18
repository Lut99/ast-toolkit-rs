//  TERMINATED.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 19:43:22
//  Last edited:
//    18 Jan 2025, 18:35:52
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`terminated()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::Span;

use super::pair;
pub use super::pair::{Error, ExpectsFormatter, Fatal, Recoverable};
use crate::Combinator2;
use crate::result::Result as SResult;


/***** LIBRARY *****/
// NOTE: Not a type alias, because this needs to change the interface (either introduce `'t` at the
// type level or else other arbitrary generics).
/// Actually implements the [`terminated()`]-combinator.
pub struct Terminated<C1, C2, F, S> {
    /// The left combinator, not to discard.
    left:  C1,
    /// The right combinator, to discard.
    right: C2,
    _f:    PhantomData<F>,
    _s:    PhantomData<S>,
}
impl<'t, C1, C2, F, S> Combinator2<'t, F, S> for Terminated<C1, C2, F, S>
where
    C1: Combinator2<'t, F, S>,
    C2: Combinator2<'t, F, S>,
{
    type ExpectsFormatter = ExpectsFormatter<C1::ExpectsFormatter, C2::ExpectsFormatter>;
    type Output = C1::Output;
    type Recoverable = Recoverable<C1::Recoverable, C2::Recoverable>;
    type Fatal = Fatal<C1::Fatal, C2::Fatal>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmts: (self.left.expects(), self.right.expects()) } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        // We can rely on the `pair()`-combinator, we just discard as appropriate
        match pair(&mut self.left, &mut self.right).parse(input) {
            Ok((rem, (res, _))) => Ok((rem, res)),
            Err(err) => Err(err),
        }
    }
}





/***** LIBRARY *****/
/// Applies the first combinator, then applies the second, then discards the latter's result.
///
/// # Arguments
/// - `first`: The first combinator to match.
/// - `second`: The second combinator to match (and who's result to discard).
///
/// # Returns
/// A combinator [`Terminated`] that will first apply the `first` combinator, and then the `second`.
///
/// # Fails
/// The returned combinator fails if either the `first` or the `second` combinator fails.
///
/// Note that this short-circuits; the second combinator is never called if the first fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::sequence2::terminated;
/// use ast_toolkit_snack::utf82::complete::{digit1, tag};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello123");
/// let span2 = Span::new("<example>", "123");
/// let span3 = Span::new("<example>", "HelloWorld");
///
/// let mut comb = terminated(tag("Hello"), digit1());
/// assert_eq!(comb.parse(span1), Ok((span1.slice(8..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(terminated::Recoverable::Comb0(tag::Recoverable {
///         tag:  "Hello",
///         span: span2,
///     })))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(terminated::Recoverable::Comb1(digit1::Recoverable {
///         fmt:  digit1::ExpectsFormatter,
///         span: span3.slice(5..),
///     })))
/// );
/// ```
#[inline]
pub const fn terminated<'t, C1, C2, F, S>(first: C1, second: C2) -> Terminated<C1, C2, F, S>
where
    C1: Combinator2<'t, F, S>,
    C2: Combinator2<'t, F, S>,
{
    Terminated { left: first, right: second, _f: PhantomData, _s: PhantomData }
}
