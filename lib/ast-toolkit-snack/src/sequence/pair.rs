//  PAIR.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:06:36
//  Last edited:
//    08 May 2025, 11:20:46
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`pair()`]-combinator.
//

use ast_toolkit_span::Spannable;

pub use super::tuple::{Error2 as Error, ExpectsFormatter2 as ExpectsFormatter};
use crate::Combinator;


/***** TYPE ALIASES *****/
/// Actual implementation for the [`pair()`]-combinator.
pub type Pair<C1, C2> = (C1, C2);

/// Recoverable errors emitted by [`Pair`].
pub type Recoverable<E1, E2> = Error<E1, E2>;

/// Fatal errors emitted by [`Pair`].
pub type Fatal<E1, E2> = Error<E1, E2>;





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
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::ascii::digit1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_snack::sequence::pair;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello123");
/// let span2 = Span::new("123");
/// let span3 = Span::new("HelloWorld");
///
/// let mut comb = pair(tag(b"Hello"), digit1());
/// assert_eq!(comb.parse(span1), Ok((span1.slice(8..), (span1.slice(..5), span1.slice(5..8)))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(pair::Recoverable::Comb0(tag::Recoverable {
///         tag: b"Hello",
///         is_fixable: false,
///         span: span2,
///     })))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(pair::Recoverable::Comb1(digit1::Recoverable {
///         fmt:     digit1::ExpectsFormatter { what: "digit" },
///         fixable: None,
///         span:    span3.slice(5..),
///     })))
/// );
/// ```
#[inline]
pub const fn pair<'c, 's, C1, C2, S>(first: C1, second: C2) -> Pair<C1, C2>
where
    C1: Combinator<'c, 's, S>,
    C2: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    (first, second)
}
