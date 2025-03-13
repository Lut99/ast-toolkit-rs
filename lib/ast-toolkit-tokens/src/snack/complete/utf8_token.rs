//  UTF 8 TOKEN.rs
//    by Lut99
//
//  Created:
//    12 Mar 2025, 13:52:52
//  Last edited:
//    13 Mar 2025, 22:06:47
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`utf8_token()`]-combinator.
//

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_snack::Combinator;
use ast_toolkit_snack::combinator::recognize;
use ast_toolkit_snack::result::{Result as SResult, SnackError};
use ast_toolkit_snack::span::MatchBytes;
use ast_toolkit_snack::utf8::complete::tag;
use ast_toolkit_span::{Span, Spanning};
use better_derive::{Debug, Eq, PartialEq};


/***** ERRORS *****/
/// Defines the recoverable error for the [`utf8_token()`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub enum Recoverable<E, F, S> {
    /// Not the token we expected.
    Keyword { what: &'static str, span: Span<F, S> },
    /// We found a remainder we shouldn't have.
    EndOfToken { what: &'static str, err: E },
}
impl<E, F, S> Display for Recoverable<E, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Keyword { what, .. } => ExpectsFormatter { what }.fmt(f),
            Self::EndOfToken { what, .. } => write!(f, "Expected token boundary after {what:?}"),
        }
    }
}
impl<E: 'static + Error, F, S> Error for Recoverable<E, F, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Keyword { .. } => None,
            Self::EndOfToken { err, .. } => Some(err),
        }
    }
}
impl<E: Spanning<F, S>, F: Clone, S: Clone> Spanning<F, S> for Recoverable<E, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::Keyword { span, .. } => span.clone(),
            Self::EndOfToken { err, .. } => err.span(),
        }
    }

    #[inline]
    fn into_span(self) -> Span<F, S>
    where
        Self: Sized,
    {
        match self {
            Self::Keyword { span, .. } => span,
            Self::EndOfToken { err, .. } => err.into_span(),
        }
    }
}





/***** EXPECTS FORMATTERS *****/
/// ExpectsFormatter for the [`Utf8Token`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter {
    pub what: &'static str,
}
impl Display for ExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as ast_toolkit_snack::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl ast_toolkit_snack::ExpectsFormatter for ExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "{}", self.what) }
}





/***** COMBINATORS *****/
/// Implements the [`utf8_token()`]-combinator.
pub struct Utf8Token<T, C, F, S> {
    /// The token type to parse.
    _t:  PhantomData<T>,
    /// The combinator used to parse the end-of-token at the end.
    eot: C,
    _f:  PhantomData<F>,
    _s:  PhantomData<S>,
}
impl<'t, T, C, F, S> Combinator<'t, F, S> for Utf8Token<T, C, F, S>
where
    T: crate::Utf8Token<F, S>,
    C: Combinator<'t, F, S>,
    F: Clone,
    S: Clone + MatchBytes,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = T;
    type Recoverable = Recoverable<C::Recoverable, F, S>;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: T::TOKEN } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        // Parse the keyword first
        let (rem, token): (Span<F, S>, T) = match tag(T::TOKEN).parse(input) {
            Ok((rem, res)) => (rem, T::from(res)),
            Err(SnackError::Recoverable(err)) => return Err(SnackError::Recoverable(Recoverable::Keyword { what: T::TOKEN, span: err.into_span() })),
            Err(SnackError::Fatal(_) | SnackError::NotEnough { .. }) => unreachable!(),
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
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
/// use ast_toolkit_tokens::snack::complete::utf8_token;
/// use ast_toolkit_tokens::{utf8_token, utf8_token_snack};
///
/// // Define the tokens
/// utf8_token!(Dot, ".");
/// utf8_token_snack!(Dot);
/// utf8_token!(Foo, "foo");
/// utf8_token_snack!(Foo);
///
/// // Let's test some inputs
/// let span1 = Span::new("<example>", ".,");
/// let span2 = Span::new("<example>", "foo");
/// let span3 = Span::new("<example>", "foobar");
/// let span4 = Span::new("<example>", "fo");
///
/// let mut comb1 = utf8_token::<Dot<_, _>, _, _, _>(nop());
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
/// let mut comb2 = Foo::parser(not(tag("bar")));
/// assert_eq!(
///     comb2.parse(span1),
///     Err(SnackError::Recoverable(utf8_token::Recoverable::Keyword { what: "foo", span: span1 }))
/// );
/// assert_eq!(comb2.parse(span2), Ok((span2.slice(3..), Foo { span: span2.slice(..3) })));
/// assert_eq!(
///     comb2.parse(span3),
///     Err(SnackError::Recoverable(utf8_token::Recoverable::EndOfToken {
///         what: "foo",
///         err:  not::Recoverable {
///             fmt:  not::ExpectsFormatter { fmt: tag::ExpectsFormatter { tag: "bar" } },
///             span: span3.slice(3..),
///         },
///     }))
/// );
/// assert_eq!(
///     comb2.parse(span4),
///     Err(SnackError::Recoverable(utf8_token::Recoverable::Keyword { what: "foo", span: span4 }))
/// );
/// ```
#[inline]
pub const fn utf8_token<'t, T, C, F, S>(comb: C) -> Utf8Token<T, C, F, S>
where
    T: crate::Utf8Token<F, S>,
    C: Combinator<'t, F, S>,
    F: Clone,
    S: Clone + MatchBytes,
{
    Utf8Token { _t: PhantomData, eot: comb, _f: PhantomData, _s: PhantomData }
}
