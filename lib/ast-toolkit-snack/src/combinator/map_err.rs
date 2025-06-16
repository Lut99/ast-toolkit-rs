//  MAP ERROR.rs
//    by Lut99
//
//  Description:
//!   Implements the [`map_err()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ParseError};


/***** COMBINATORS *****/
/// Actual implementation of [`map_err()`].
pub struct MapErr<C, P, S> {
    /// The combinator to wrap.
    comb: C,
    /// The predicate used to map.
    map:  P,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, P, E1, E2, S> Combinator<'c, 's, S> for MapErr<C, P, S>
where
    C: Combinator<'c, 's, S>,
    P: FnMut(SnackError<C::Recoverable, C::Fatal>) -> SnackError<E1, E2>,
    E1: ParseError<S>,
    E2: ParseError<S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = C::Output;
    type Recoverable = E1;
    type Fatal = E2;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match self.comb.parse(input) {
            Ok(res) => Ok(res),
            Err(err) => Err((self.map)(err)),
        }
    }
}





/***** LIBRARY *****/
/// Combinator that, if the given combinator fails, will map its [`SnackError`] to another one.
///
/// This is a more flexible version of [`map_recoverable()`](super::map_recoverable()) or
/// [`map_fatal()`](super::map_fatal()) because it also allows translation from recoverable errors
/// to fatal ones and vice versa.
///
/// # Arguments
/// - `comb`: Some [`Combinator`] to map the result of.
/// - `map`: Some [`FnMut`] closure that maps one [`SnackError`] to another.
///
/// # Returns
/// A combinator that will behave exactly the same as the given `comb`, except that it will map the
/// errors before returning them.
///
/// # Fails
/// The returned combinator fails precisely when `comb` fails. However, _how_ it fails (e.g.,
/// recoverably or fatal) is dependent on `map`.
///
/// # Example
/// ```rust
/// use std::convert::Infallible;
///
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::map_err;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Goodbye, world!");
///
/// let mut comb = map_err(
///     tag(b"Hello, world!"),
///     |err| -> SnackError<Infallible, tag::Recoverable<'static, u8, &'static str>> {
///         match err {
///             SnackError::Recoverable(err) => SnackError::Fatal(err),
///             SnackError::Fatal(_) => unreachable!(),
///         }
///     },
/// );
/// assert_eq!(
///     comb.parse(span1),
///     Err(SnackError::Fatal(tag::Recoverable {
///         tag: b"Hello, world!",
///         is_fixable: false,
///         span: span1,
///     }))
/// );
/// ```
#[inline]
pub const fn map_err<'c, 's, C, P, E1, E2, S>(comb: C, map: P) -> MapErr<C, P, S>
where
    C: Combinator<'c, 's, S>,
    P: FnMut(SnackError<C::Recoverable, C::Fatal>) -> SnackError<E1, E2>,
    E1: ParseError<S>,
    E2: ParseError<S>,
    S: Clone + Spannable<'s>,
{
    MapErr { comb, map, _s: PhantomData }
}
