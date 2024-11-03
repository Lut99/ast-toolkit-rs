//  PAIR.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:06:36
//  Last edited:
//    03 Nov 2024, 11:16:56
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`pair()`]-combinator.
//

pub use super::tuple::{Tuple2 as Pair, Tuple2Error as PairError, Tuple2ExpectsFormatter as PairExpectsFormatter};
use crate::Combinator2;


/***** LIBRARY *****/
/// Applies the first combinator, then applies the second.
///
/// # Arguments
/// - `first`: The first combinator to match.
/// - `second`: The second combinator to match.
///
/// # Returns
/// A combinator [`Pair`] that will first apply the `first` combinator, and then the `second`.
///
/// # Fails
/// The returned combinator fails if either the `first` or the `second` combinator fails.
///
/// Note that this short-circuits; the second combinator is never called if the first fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::sequence2::pair;
/// use ast_toolkit_snack::utf82::complete::{digit1, tag};
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello123");
/// let span2 = Span::new("<example>", "123");
/// let span3 = Span::new("<example>", "HelloWorld");
///
/// let mut comb = pair(tag("Hello"), digit1());
/// assert_eq!(comb.parse(span1), Ok((span1.slice(8..), (span1.slice(..5), span1.slice(5..8)))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(pair::PairError::Comb0(tag::TagRecoverable {
///         tag:  "Hello",
///         span: span2,
///     })))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(pair::PairError::Comb1(digit1::Digit1Recoverable {
///         span: span3.slice(5..),
///     })))
/// );
/// ```
#[inline]
pub const fn pair<'t, F, S, C1, C2>(first: C1, second: C2) -> Pair<C1, C2>
where
    C1: Combinator2<'t, F, S>,
    C2: Combinator2<'t, F, S>,
{
    Pair { combs: (first, second) }
}
