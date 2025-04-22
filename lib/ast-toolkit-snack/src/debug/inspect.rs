//  INSPECT.rs
//    by Lut99
//
//  Created:
//    01 Dec 2024, 12:11:28
//  Last edited:
//    22 Apr 2025, 11:54:17
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`inspect()`]-operator.
//

use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::Combinator;
use crate::result::Result as SResult;
use crate::span::Parsable;


/***** COMBINATORS *****/
/// Actual implementation of the [`inspect()`]-combinator.
pub struct Inspect<C, P, S> {
    /// The combinator to maybe apply.
    comb: C,
    /// The closure to call before executing `comb`.
    pred: P,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<'c, 's, C, P, S> Combinator<'c, 's, S> for Inspect<C, P, S>
where
    C: Combinator<'c, 's, S>,
    P: for<'a> FnMut(&'a Span<S>),
    S: Clone + Spannable<'s>,
    S::Slice: Parsable<'s>,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = C::Output;
    type Recoverable = C::Recoverable;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
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
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb = inspect(tag("Hello"), |input| println!("{input:?}"));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: "Hello", span: span2 }))
/// );
/// ```
#[inline]
pub const fn inspect<'c, 's, C, P, S>(comb: C, pred: P) -> Inspect<C, P, S>
where
    C: Combinator<'c, 's, S>,
    P: for<'a> FnMut(&'a Span<S>),
    S: Clone + Spannable<'s>,
    S::Slice: Parsable<'s>,
{
    Inspect { comb, pred, _s: PhantomData }
}
