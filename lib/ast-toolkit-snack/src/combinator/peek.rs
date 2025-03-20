//  PEEK.rs
//    by Lut99
//
//  Created:
//    07 Mar 2025, 17:19:33
//  Last edited:
//    20 Mar 2025, 12:15:33
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`peek()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::Span;

use crate::Combinator;
use crate::result::Result as SResult;
use crate::span::Parsable;


/***** COMBINATORS *****/
/// Actual implementation of the [`peek()`]-combinator.
pub struct Peek<C, S> {
    comb: C,
    _s:   PhantomData<S>,
}
impl<'t, C, S> Combinator<'t, S> for Peek<C, S>
where
    C: Combinator<'t, S>,
    S: Clone + Parsable,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = C::Output;
    type Recoverable = C::Recoverable;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Call the combinator with the input
        let (_, res): (_, C::Output) = self.comb.parse(input.clone())?;
        // Return with the original input, obfuscating anything was consumed.
        Ok((input, res))
    }
}





/***** LIBRARY *****/
/// Applies some combinator but does not advance the stream.
///
/// Specifically, calls the given combinator with a clone of the input and then returns the full
/// input as remainder stream.
///
/// # Arguments
/// - `comb`: The combinator to apply without consuming the input stream.
///
/// # Returns
/// A combinator [`Peek`] that applies the given `comb`inator without advancing the stream.
///
/// # Fails
/// The returned combinator perfectly mimics the error behaviour of the given one.
///
/// # Examples
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::peek;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
///
/// let mut comb = peek(tag("Hello"));
/// assert_eq!(
///     comb.parse(span1),
///     // Note: The remainder did not change
///     Ok((span1, span1.slice(..5)))
/// );
/// ```
#[inline]
pub const fn peek<'t, C, S>(comb: C) -> Peek<C, S>
where
    C: Combinator<'t, S>,
    S: Clone + Parsable,
{
    Peek { comb, _s: PhantomData }
}
