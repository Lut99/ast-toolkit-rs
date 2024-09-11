//  COMPLETE.rs
//    by Lut99
//
//  Created:
//    09 Sep 2024, 15:26:49
//  Last edited:
//    11 Sep 2024, 13:54:17
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines complete versions of the token parsers, i.e., ones that
//!   consider an early EOF an error instead of a failure.
//


/***** LIBRARY *****/
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_snack::error::{Common, Error, Failure};
use ast_toolkit_snack::span::MatchBytes;
use ast_toolkit_snack::utf8::complete::tag;
use ast_toolkit_snack::{Combinator, Expects, ExpectsFormatter, Result as SResult};
use ast_toolkit_span::{Span, Spanning};

use crate as ast;


/***** ERRORS *****/
/// Defines errors that occur when parsing tokens or delimiters.
#[derive(Debug)]
pub enum ParseError<'a, F, S, E> {
    // Token
    /// Failed to find the token itself.
    Utf8Token { token: &'static str, span: Span<F, S> },
    /// The postfix did not match.
    Postfix { err: Common<'a, F, S, E> },

    // Delimiter
    /// Failed to parse the opening delimiter.
    Utf8OpenToken { token: &'static str, span: Span<F, S> },
    /// Failed to parse the bit in between the delimiters.
    Inner { err: Common<'a, F, S, E> },
    /// Failed to parse the closing delimiter.
    Utf8CloseToken { token: &'static str, span: Span<F, S> },
}
impl<'t, F, S, E: Display> Display for ParseError<'t, F, S, E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use ParseError::*;
        match self {
            Utf8Token { token, .. } => write!(f, "{}", Utf8TokenExpects { what: token }),
            Postfix { err } => write!(f, "Expected token to be succeeded by {err}"),

            Utf8OpenToken { token, .. } => write!(f, "Expected opening delimiter '{token}'"),
            Inner { err } => write!(f, "Expected {err} in between delimiters"),
            Utf8CloseToken { token, .. } => write!(f, "Expected closing delimiter '{token}'"),
        }
    }
}
impl<'t, F: Debug, S: Debug, E: std::error::Error> std::error::Error for ParseError<'t, F, S, E> {}
impl<'t, F: Clone, S: Clone, E: Spanning<F, S>> Spanning<F, S> for ParseError<'t, F, S, E> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        use ParseError::*;
        match self {
            Utf8Token { span, .. } => span.clone(),
            Postfix { err } => err.span(),

            Utf8OpenToken { span, .. } => span.clone(),
            Inner { err } => err.span(),
            Utf8CloseToken { span, .. } => span.clone(),
        }
    }
}





/***** LIBRARY *****/
/// Parses a (non-delimited) token.
///
/// # Generics
/// - `F`: The type of the From-string in input [`Span`]s.
/// - `S`: The type of the Source-string in input [`Span`]s.
/// - `T`: The [`Utf8Token`](ast::Utf8Token) to parse.
/// - `C`: The combinator that is used to match the postfix.
///
/// # Arguments
/// - `comb`: A combinator that can be used to match something after the token. The token
///   is only parsed if it succeeds, but what it matches is discarded. This can be used to ensure
///   no identifier is matched that happens to begin with a keyword, for example.
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
/// use ast_toolkit_snack::combinator::{nop, not};
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
/// use ast_toolkit_tokens::snack::complete::{utf8_token, ParseError};
/// use ast_toolkit_tokens::utf8_token;
///
/// // Define the tokens
/// utf8_token!(Dot, ".");
/// utf8_token!(Foo, "foo");
///
/// // Let's test some inputs
/// let span1 = Span::new("<example>", ".,");
/// let span2 = Span::new("<example>", "foo");
/// let span3 = Span::new("<example>", "foobar");
///
/// let mut comb1 = utf8_token::<_, _, Dot<_, _>, _>(nop());
/// assert_eq!(comb1.parse(span1).unwrap(), (span1.slice(1..), Dot { span: span1.slice(..1) }));
/// assert!(matches!(
///     comb1.parse(span2),
///     SResult::Fail(Failure::Common(Common::Custom(ParseError::Utf8Token { .. })))
/// ));
/// assert!(matches!(
///     comb1.parse(span3),
///     SResult::Fail(Failure::Common(Common::Custom(ParseError::Utf8Token { .. })))
/// ));
///
/// let mut comb2 = utf8_token::<_, _, Foo<_, _>, _>(not(tag("bar")));
/// assert!(matches!(
///     comb2.parse(span1),
///     SResult::Fail(Failure::Common(Common::Custom(ParseError::Utf8Token { .. })))
/// ));
/// assert_eq!(comb2.parse(span2).unwrap(), (span2.slice(3..), Foo { span: span2.slice(..3) }));
/// assert!(matches!(
///     comb2.parse(span3),
///     SResult::Fail(Failure::Common(Common::Custom(ParseError::Postfix {
///         err: Common::Not { .. },
///     })))
/// ));
/// ```
#[inline]
pub const fn utf8_token<'t, F, S, T, C>(comb: C) -> Utf8Token<F, S, T, C>
where
    F: Clone,
    S: Clone + MatchBytes,
    T: ast::Utf8Token<F, S>,
    C: Combinator<'t, F, S>,
{
    Utf8Token { _f: PhantomData, _s: PhantomData, _t: PhantomData, comb }
}

/// Parses something delimited by a [`Utf8Delimiter`](ast::Utf8Delimiter).
///
/// # Generics
/// - `F`: The type of the From-string in input [`Span`]s.
/// - `S`: The type of the Source-string in input [`Span`]s.
/// - `T`: The [`Utf8Delimiter`](ast::Utf8Delimiter) to parse.
/// - `C`: The combinator that is used to match the bit in between the delimiters.
///
/// # Arguments
/// - `comb`: A combinator that can be used to parse the content that is delimited by `T`.
///
/// # Returns
/// A combinator [`Utf8Delimiter`] that parses the delimiters and the bit in between.
///
/// # Fails
/// The returned combinator fails if the opening token was not found or if the given `comb` fails.
///
/// # Errors
/// The returned combinator fails unrecoverably if it failed to parse the closing delimiter after
/// parsing the opening delimiter and `comb`; or if `comb` errors.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Error, Failure};
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
/// use ast_toolkit_tokens::snack::complete::{utf8_delimiter, ParseError};
/// use ast_toolkit_tokens::utf8_delimiter;
///
/// // Define the tokens
/// utf8_delimiter!(Parens, "(", ")");
///
/// // Let's test some inputs
/// let span1 = Span::new("<example>", "(foo)");
/// let span2 = Span::new("<example>", "(bar)");
/// let span3 = Span::new("<example>", "foo");
/// let span4 = Span::new("<example>", "(foo");
///
/// let mut comb1 = utf8_delimiter::<_, _, Parens<_, _>, _>(tag("foo"));
/// assert_eq!(
///     comb1.parse(span1).unwrap(),
///     (
///         span1.slice(5..),
///         (span1.slice(1..4), Parens { open: span1.slice(0..1), close: span1.slice(4..5) })
///     )
/// );
/// assert!(matches!(
///     comb1.parse(span2),
///     SResult::Fail(Failure::Common(Common::Custom(ParseError::Inner { .. })))
/// ));
/// assert!(matches!(
///     comb1.parse(span3),
///     SResult::Fail(Failure::Common(Common::Custom(ParseError::Utf8OpenToken { .. })))
/// ));
/// assert!(matches!(
///     comb1.parse(span4),
///     SResult::Error(Error::Common(Common::Custom(ParseError::Utf8CloseToken { .. })))
/// ));
/// ```
#[inline]
pub const fn utf8_delimiter<'t, F, S, T, C>(comb: C) -> Utf8Delimiter<F, S, T, C>
where
    F: Clone,
    S: Clone + MatchBytes,
    T: ast::Utf8Delimiter<F, S>,
    C: Combinator<'t, F, S>,
{
    Utf8Delimiter { _f: PhantomData, _s: PhantomData, _t: PhantomData, comb }
}





/***** EXPECTS FORMATTERS *****/
/// ExpectsFormatter for the [`Utf8Token`] combinator.
#[derive(Debug)]
pub struct Utf8TokenExpects {
    what: &'static str,
}
impl Display for Utf8TokenExpects {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for Utf8TokenExpects {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "{}", self.what) }
}

/// ExpectsFormatter for the [`Utf8Delimiter`] combinator.
#[derive(Debug)]
pub struct Utf8DelimiterExpects<E> {
    /// The opening character.
    left:  &'static str,
    /// The middle expects.
    inner: E,
    /// The closing character.
    right: &'static str,
}
impl<C: ExpectsFormatter> Display for Utf8DelimiterExpects<C> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<C: ExpectsFormatter> ExpectsFormatter for Utf8DelimiterExpects<C> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        self.inner.expects_fmt(f, indent)?;
        write!(f, " delimited by '{}{}'", self.left, self.right)
    }
}





/***** COMBINATORS *****/
/// Combinator returned by any of the keyword-token combinators.
pub struct Utf8Token<F, S, T, C> {
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
    _t:   PhantomData<T>,
    comb: C,
}
impl<'t, F, S, T: ast::Utf8Token<F, S>, C> Expects<'t> for Utf8Token<F, S, T, C> {
    type Formatter = Utf8TokenExpects;

    #[inline]
    fn expects(&self) -> Self::Formatter { Utf8TokenExpects { what: T::TOKEN } }
}
impl<'t, F: Clone, S: Clone + MatchBytes, T: ast::Utf8Token<F, S>, C: Combinator<'t, F, S>> Combinator<'t, F, S> for Utf8Token<F, S, T, C> {
    type Output = T;
    type Error = ParseError<'t, F, S, C::Error>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<'t, Self::Output, F, S, Self::Error> {
        // Parse the keyword itself, first
        let (rem, res) = match tag(T::TOKEN).parse(input.clone()) {
            SResult::Ok(rem, res) => (rem, res),
            SResult::Fail(fail) => {
                return SResult::Fail(Failure::Common(Common::Custom(ParseError::Utf8Token { token: T::TOKEN, span: fail.span() })));
            },
            SResult::Error(_) => unreachable!(),
        };

        // Make sure the remainder is OK
        match self.comb.parse(rem.clone()) {
            SResult::Ok(_, _) => SResult::Ok(rem, T::from(res)),
            SResult::Fail(Failure::NotEnough { needed, span }) => SResult::Fail(Failure::NotEnough { needed, span }),
            SResult::Fail(fail) => SResult::Fail(Failure::Common(Common::Custom(ParseError::Postfix { err: fail.try_into().unwrap() }))),
            SResult::Error(Error::Context { context, span }) => SResult::Error(Error::Context { context, span }),
            SResult::Error(err) => SResult::Error(Error::Common(Common::Custom(ParseError::Postfix { err: err.try_into().unwrap() }))),
        }
    }
}

/// Combinator returned by any of the delimited tokens.
pub struct Utf8Delimiter<F, S, T, C> {
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
    _t:   PhantomData<T>,
    /// The nested combinator that parses the middle bit.
    comb: C,
}
impl<'t, F, S, T, C> Expects<'t> for Utf8Delimiter<F, S, T, C>
where
    T: ast::Utf8Delimiter<F, S>,
    C: Expects<'t>,
{
    type Formatter = Utf8DelimiterExpects<C::Formatter>;

    #[inline]
    fn expects(&self) -> Self::Formatter { Utf8DelimiterExpects { left: T::OPEN_TOKEN, inner: self.comb.expects(), right: T::CLOSE_TOKEN } }
}
impl<'t, F, S, T, C> Combinator<'t, F, S> for Utf8Delimiter<F, S, T, C>
where
    F: Clone,
    S: Clone + MatchBytes,
    T: ast::Utf8Delimiter<F, S>,
    C: Combinator<'t, F, S>,
{
    type Output = (C::Output, T);
    type Error = ParseError<'t, F, S, C::Error>;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<'t, Self::Output, F, S, Self::Error> {
        // Parse the left itself, first
        let (rem, left): (Span<F, S>, Span<F, S>) = match tag(T::OPEN_TOKEN).parse(input) {
            SResult::Ok(rem, res) => (rem, res),
            SResult::Fail(fail) => {
                return SResult::Fail(Failure::Common(Common::Custom(ParseError::Utf8OpenToken { token: T::OPEN_TOKEN, span: fail.span() })));
            },
            SResult::Error(_) => unreachable!(),
        };

        // Parse the middle bit
        let (rem, res): (Span<F, S>, C::Output) = match self.comb.parse(rem) {
            SResult::Ok(rem, res) => (rem, res),
            SResult::Fail(Failure::NotEnough { needed, span }) => return SResult::Fail(Failure::NotEnough { needed, span }),
            SResult::Fail(fail) => return SResult::Fail(Failure::Common(Common::Custom(ParseError::Inner { err: fail.try_into().unwrap() }))),
            SResult::Error(Error::Context { context, span }) => return SResult::Error(Error::Context { context, span }),
            SResult::Error(err) => return SResult::Error(Error::Common(Common::Custom(ParseError::Inner { err: err.try_into().unwrap() }))),
        };

        // Then parse the right bit
        match tag(T::CLOSE_TOKEN).parse(rem) {
            SResult::Ok(rem, right) => SResult::Ok(rem, (res, T::from((left, right)))),
            SResult::Fail(fail) => {
                SResult::Error(Error::Common(Common::Custom(ParseError::Utf8CloseToken { token: T::CLOSE_TOKEN, span: fail.span() })))
            },
            SResult::Error(err) => {
                SResult::Error(Error::Common(Common::Custom(ParseError::Utf8CloseToken { token: T::CLOSE_TOKEN, span: err.span() })))
            },
        }
    }
}
