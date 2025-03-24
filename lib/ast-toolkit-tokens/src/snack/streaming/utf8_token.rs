//  UTF 8 TOKEN.rs
//    by Lut99
//
//  Created:
//    13 Mar 2025, 21:12:21
//  Last edited:
//    24 Mar 2025, 12:22:42
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`utf8_token()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_snack::Combinator;
use ast_toolkit_snack::combinator::recognize;
use ast_toolkit_snack::result::{Result as SResult, SnackError};
use ast_toolkit_snack::span::Utf8Parsable;
use ast_toolkit_snack::utf8::streaming::tag;
use ast_toolkit_span::{Span, Spanning};

pub use super::super::complete::utf8_token::{ExpectsFormatter, Recoverable};


/***** COMBINATORS *****/
/// Implements the [`utf8_token()`]-combinator.
pub struct Utf8Token<T, C, S> {
    /// The token type to parse.
    _t:  PhantomData<T>,
    /// The combinator used to parse the end-of-token at the end.
    eot: C,
    _s:  PhantomData<S>,
}
impl<'t, T, C, S> Combinator<'t, S> for Utf8Token<T, C, S>
where
    T: crate::Utf8Token<S>,
    C: Combinator<'t, S>,
    S: Clone + Utf8Parsable,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = T;
    type Recoverable = Recoverable<C::Recoverable, S>;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: T::TOKEN } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Parse the keyword first
        let (rem, token): (Span<S>, T) = match tag(T::TOKEN).parse(input) {
            Ok((rem, res)) => (rem, T::from(res)),
            Err(SnackError::Recoverable(err)) => return Err(SnackError::Recoverable(Recoverable::Keyword { what: T::TOKEN, span: err.into_span() })),
            Err(SnackError::Fatal(_)) => unreachable!(),
            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
        };

        // Then parse the end-of-token symbols.
        match recognize(&mut self.eot).parse(rem) {
            Ok((rem, _)) => Ok((rem, token)),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(Recoverable::EndOfToken { what: T::TOKEN, err })),
            Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(err)),
            Err(SnackError::NotEnough { needed, span }) => Err(SnackError::NotEnough { needed, span }),
        }
    }
}





/***** LIBRARY *****/
/// Parses a (non-delimited) token.
///
/// # Generics
/// - `T`: The [`Utf8Token`](crate::Utf8Token) to parse.
/// - `C`: The combinator that is used to match the end of the keyword.
/// - `F`: The type of the From-string in input [`Span`]s.
/// - `S`: The type of the Source-string in input [`Span`]s.
///
/// # Arguments
/// - `comb`: A combinator that can be used to match an end-of-token symbol. This is used to
///   disambiguite this keyword from other identifiers.
///
/// # Returns
/// A combinator [`Utf8Token`] that can parse the target token.
///
/// # Fails
/// The returned combinator fails if the token was not on top of the input stream OR the given
/// `comb`intor failed after the keyword was recognized (e.g., there was more identifier to parse).
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::{nop, not};
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::streaming::tag;
/// use ast_toolkit_span::Span;
/// use ast_toolkit_tokens::snack::streaming::utf8_token;
/// use ast_toolkit_tokens::{utf8_token, utf8_token_snack};
///
/// // Define the tokens
/// utf8_token!(Dot, ".");
/// utf8_token_snack!(Dot);
/// utf8_token!(Foo, "foo");
/// utf8_token_snack!(Foo);
///
/// // Let's test some inputs
/// let span1 = Span::new(".,");
/// let span2 = Span::new("foo");
/// let span3 = Span::new("foobar");
/// let span4 = Span::new("fo");
///
/// let mut comb1 = utf8_token::<Dot<_>, _, _>(nop());
/// assert_eq!(comb1.parse(span1), Ok((span1.slice(1..), Dot { span: span1.slice(..1) })));
/// assert_eq!(
///     comb1.parse(span2),
///     Err(SnackError::Recoverable(utf8_token::Recoverable::Keyword { what: ".", span: span2 }))
/// );
/// assert!(matches!(
///     comb1.parse(span3),
///     Err(SnackError::Recoverable(utf8_token::Recoverable::Keyword { what: ".", span: span3 }))
/// ));
/// assert!(matches!(
///     comb1.parse(span4),
///     Err(SnackError::Recoverable(utf8_token::Recoverable::Keyword { what: ".", span: span4 }))
/// ));
///
/// // Alternative way of getting the parser
/// let mut comb2 = Foo::parser_streaming(not(tag("bar")));
/// assert_eq!(
///     comb2.parse(span1),
///     Err(SnackError::Recoverable(utf8_token::Recoverable::Keyword { what: "foo", span: span1 }))
/// );
/// assert_eq!(
///     comb2.parse(span2),
///     Err(SnackError::NotEnough { needed: Some(3), span: span2.slice(3..) })
/// );
/// assert_eq!(
///     comb2.parse(span3),
///     Err(SnackError::Recoverable(utf8_token::Recoverable::EndOfToken {
///         what: "foo",
///         err:  not::Recoverable {
///             fmt:  not::ExpectsFormatter { fmt: tag::ExpectsFormatter { tag: "bar" } },
///             span: span3.slice(3..6),
///         },
///     }))
/// );
/// assert_eq!(
///     comb2.parse(span4),
///     Err(SnackError::NotEnough { needed: Some(1), span: span4.slice(2..) })
/// );
/// ```
#[inline]
pub const fn utf8_token<'t, T, C, S>(comb: C) -> Utf8Token<T, C, S>
where
    T: crate::Utf8Token<S>,
    C: Combinator<'t, S>,
    S: Clone + Utf8Parsable,
{
    Utf8Token { _t: PhantomData, eot: comb, _s: PhantomData }
}
