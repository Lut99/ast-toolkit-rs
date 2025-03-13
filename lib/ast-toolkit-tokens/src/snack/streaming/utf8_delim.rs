//  UTF 8 DELIM.rs
//    by Lut99
//
//  Created:
//    13 Mar 2025, 21:12:24
//  Last edited:
//    13 Mar 2025, 22:06:17
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a parser for delimiting tokens.
//

use std::marker::PhantomData;

use ast_toolkit_snack::Combinator;
use ast_toolkit_snack::result::{Result as SResult, SnackError};
use ast_toolkit_snack::span::MatchBytes;
use ast_toolkit_snack::utf8::streaming::tag;
use ast_toolkit_span::{Span, Spanning};

pub use super::super::complete::utf8_delim::{ExpectsFormatter, Fatal, Recoverable};
use crate::Utf8Delimiter;


/***** COMBINATORS *****/
/// Implements the [`utf8_delim()`]-combinator.
pub struct Utf8Delim<T, C, F, S> {
    /// The type of token to parse.
    _t:   PhantomData<T>,
    /// The combinator that parses in between the parenthesis.
    comb: C,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, T, C, F, S> Combinator<'t, F, S> for Utf8Delim<T, C, F, S>
where
    T: Utf8Delimiter<F, S>,
    C: Combinator<'t, F, S>,
    F: Clone,
    S: Clone + MatchBytes,
{
    type ExpectsFormatter = ExpectsFormatter<C::ExpectsFormatter>;
    type Output = (C::Output, T);
    type Recoverable = Recoverable<C::Recoverable, F, S>;
    type Fatal = Fatal<C::Fatal, F, S>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { left: T::OPEN_TOKEN, inner: self.comb.expects(), right: T::CLOSE_TOKEN } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        // Attempt to parse the opening delimiter
        let (rem, open): (Span<F, S>, Span<F, S>) = match tag(T::OPEN_TOKEN).parse(input) {
            Ok(res) => res,
            Err(SnackError::Recoverable(err)) => {
                return Err(SnackError::Recoverable(Recoverable::OpenKeyword { what: T::OPEN_TOKEN, span: err.into_span() }));
            },
            Err(SnackError::Fatal(_)) => unreachable!(),
            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
        };

        // Attempt to parse the middle bit
        let (rem, inner): (Span<F, S>, C::Output) = match self.comb.parse(rem) {
            Ok(res) => res,
            Err(SnackError::Recoverable(err)) => return Err(SnackError::Recoverable(Recoverable::Inner { err })),
            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Inner { err })),
            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
        };

        // Finally, parse the closing delim
        match tag(T::CLOSE_TOKEN).parse(rem) {
            Ok((rem, close)) => Ok((rem, (inner, T::from((open, close))))),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Fatal(Fatal::CloseKeyword { what: T::CLOSE_TOKEN, span: err.into_span() })),
            Err(SnackError::Fatal(_)) => unreachable!(),
            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
        }
    }
}





/***** LIBRARY *****/
/// Parses something delimited by a [`Utf8Delimiter`](crate::Utf8Delimiter).
///
/// # Generics
/// - `T`: The [`Utf8Token`](crate::Utf8Token) to parse.
/// - `C`: The combinator that is used to match the delimited contents.
/// - `F`: The type of the From-string in input [`Span`]s.
/// - `S`: The type of the Source-string in input [`Span`]s.
///
/// # Arguments
/// - `comb`: A combinator that can be used to parse the content that is delimited by `T`.
///
/// # Returns
/// A combinator [`Utf8Delim`] that parses the delimiters and the bit in between.
///
/// # Fails
/// The returned combinator fails if the opening token was not found or if the given `comb` fails.
///
/// # Errors
/// The returned combinator fails unrecoverably if it failed to parse the closing delimiter after
/// parsing the opening delimiter and `comb`; or if `comb` fails fatally.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::streaming::tag;
/// use ast_toolkit_span::Span;
/// use ast_toolkit_tokens::snack::streaming::utf8_delim;
/// use ast_toolkit_tokens::{utf8_delim, utf8_delim_snack};
///
/// // Define the tokens
/// utf8_delim!(Parens, "(", ")");
/// utf8_delim_snack!(Parens);
///
/// // Let's test some inputs
/// let span1 = Span::new("<example>", "(foo)");
/// let span2 = Span::new("<example>", "(bar)");
/// let span3 = Span::new("<example>", "foo");
/// let span4 = Span::new("<example>", "(foo");
///
/// let mut comb1 = utf8_delim::<Parens<_, _>, _, _, _>(tag("foo"));
/// assert_eq!(
///     comb1.parse(span1),
///     Ok((
///         span1.slice(5..),
///         (span1.slice(1..4), Parens { open: span1.slice(0..1), close: span1.slice(4..5) })
///     ))
/// );
/// assert_eq!(
///     comb1.parse(span2),
///     Err(SnackError::Recoverable(utf8_delim::Recoverable::Inner {
///         err: tag::Recoverable { tag: "foo", span: span2.slice(1..) },
///     }))
/// );
///
/// // Alternative way of getting the same parser
/// let mut comb2 = Parens::parser_streaming(tag("foo"));
/// assert_eq!(
///     comb2.parse(span3),
///     Err(SnackError::Recoverable(utf8_delim::Recoverable::OpenKeyword {
///         what: "(",
///         span: span3,
///     }))
/// );
/// assert_eq!(
///     comb2.parse(span4),
///     Err(SnackError::NotEnough { needed: Some(1), span: span4.slice(4..) })
/// );
/// ```
#[inline]
pub const fn utf8_delim<'t, T, C, F, S>(comb: C) -> Utf8Delim<T, C, F, S>
where
    T: Utf8Delimiter<F, S>,
    C: Combinator<'t, F, S>,
    F: Clone,
    S: Clone + MatchBytes,
{
    Utf8Delim { _t: PhantomData, comb, _f: PhantomData, _s: PhantomData }
}
