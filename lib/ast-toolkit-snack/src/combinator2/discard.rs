//  DISCARD.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 18:55:10
//  Last edited:
//    03 Nov 2024, 19:33:07
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`discard()`]-combinator.
//

use std::marker::PhantomData;

use crate::Combinator2;
use crate::result::Result as SResult;


/***** COMBINATORS *****/
/// Actual implementation of the [`discard()`]-combinator.
pub struct Discard<C, F, S> {
    comb: C,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C, F, S> Combinator2<'t, F, S> for Discard<C, F, S>
where
    C: Combinator2<'t, F, S>,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = ();
    type Recoverable = C::Recoverable;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: ast_toolkit_span::Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        match self.comb.parse(input) {
            Ok((rem, _)) => Ok((rem, ())),
            Err(err) => Err(err),
        }
    }
}





/***** LIBRARY *****/
/// Discards the output of another combinator.
///
/// This useful when you only want to advance the stream but not get anything out of it.
///
/// # Arguments
/// - `comb`: Some combinator to apply and then to discard the output of.
///
/// # Returns
/// A combinator [`Discard`] that will apply `comb` and discard its output.
///
/// # Fails
/// The returned combinator fails exactly when `comb` fails.
///
/// # Exampel
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::combinator2::discard;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = discard(tag("Hello"));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), ())));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::TagRecoverable { tag: "Hello", span: span2 }))
/// );
/// ```
#[inline]
pub const fn discard<'t, C, F, S>(comb: C) -> Discard<C, F, S>
where
    C: Combinator2<'t, F, S>,
{
    Discard { comb, _f: PhantomData, _s: PhantomData }
}
