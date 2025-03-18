//  //  ADD.rs
//    by John Smith
//
//  Created:
//    07 Mar 2025, 15:10:20
//  Last edited:
//    07 Mar 2025, 15:10:20
//  Auto updated?
//    No
//
//  Description:
//!   Example showing the usage of the `#[comb(...)]`-macro.
//

use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};

use ast_toolkit_snack::result::SnackError;
use ast_toolkit_snack::span::{BytesParsable as _, Utf8Parsable};
use ast_toolkit_snack::utf8::complete::{digit1, tag};
use ast_toolkit_snack::{Combinator as _, comb};
use ast_toolkit_span::Span;


/***** CONSTANTS *****/
/// Whatever we call "an expression"
const EXPRESSION_NAME: &str = "expression";





/***** ERRORS *****/
/// Defines custom errors during parsing.
#[derive(Debug)]
enum ParseError {
    /// Integer overflow!
    Overflow,
}
impl Display for ParseError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Overflow => write!(f, "Integer overflow"),
        }
    }
}
impl Error for ParseError {}
impl ast_toolkit_span::Spanning<&'static str> for ParseError {
    #[inline]
    fn span(&self) -> std::borrow::Cow<Span<&'static str>> { todo!() }
    #[inline]
    fn into_span(self) -> Span<&'static str> { todo!() }
}





/***** AST *****/
#[derive(Debug)]
enum Expr {
    Add(Box<Self>, Box<Self>),
    Lit(u64),
}
impl Expr {
    /// Computes the result of the expression.
    ///
    /// # Returns
    /// A number carrying this expression's result.
    #[inline]
    fn compute(&self) -> u64 {
        match self {
            Self::Add(lhs, rhs) => lhs.compute() + rhs.compute(),
            Self::Lit(v) => *v,
        }
    }
}





/***** PARSERS *****/
/// Parses an arbitrary expression.
///
/// Luckily for us, our combinator needn't deal with precedence and associativity and all that
/// jazz.
///
/// # Arguments
/// - `input`: The input span to parse.
///
/// # Returns
/// The result, as an [`SResult`].
///
/// # Fails
/// This combinator does not fail.
///
/// # Errors
/// This combinator errors if the input is not a valid expression, or if any of the digits in
/// either expression would overflow a 64-bit unsigned integer.
///
/// Erroring means that this combinator successfully recognizes the input, but it was invalid.
#[inline]
#[comb(
    // Note: needed because we access the library directly. Usually we'd do it through
    // `ast_toolkit::snack`.
    prefix = ast_toolkit_snack,
    expected = ("An {EXPRESSION_NAME}"),
    Combinator = ExprComb,
    Output = Expr,
    Recoverable = digit1::Recoverable<S>,
    Fatal = ParseError
)]
fn expr<S>(input: Span<S>) -> _
where
    S: Clone + Utf8Parsable,
    for<'a> S::Slice<'a>: Debug,
{
    // Always parse a number first
    let (rem, val) = lit().parse(input)?;

    // Then recursively parse the rest if there's an addition
    match tag("+").parse(rem.clone()) {
        Ok((rem, _)) => expr().parse(rem).map(|(rem, expr)| (rem, Expr::Add(Box::new(Expr::Lit(val)), Box::new(expr)))),
        // If there's no addition, we are still OK
        Err(SnackError::Recoverable(_)) => Ok((rem, Expr::Lit(val))),
        Err(SnackError::Fatal(err)) => unreachable!(),
        Err(SnackError::NotEnough { needed, span }) => Err(SnackError::NotEnough { needed, span }),
    }
}

/// Parses a literal number.
///
/// This combinator is a little more complex, because it also parses the resulting string as an
/// actual number.
///
/// # Arguments
/// - `input`: The input span to parse.
///
/// # Returns
/// The result, as an [`SResult`].
///
/// # Fails
/// This combinator fails if the input does not start with at least one digit.
///
/// Failure means that another combinator may try to parse the input instead.
///
/// # Errors
/// This combinator errors if the digit at the head of the input would overflow a 64-bit unsigned
/// integer.
///
/// Erroring means that this combinator successfully recognizes the input, but it was invalid.
#[comb(
    // Note: needed because we access the library directly. Usually we'd do it through
    // `ast_toolkit::snack`.
    prefix = ast_toolkit_snack,
    expected = "A literal number",
    Output = u64,
    Recoverable = digit1::Recoverable<S>,
    Fatal = ParseError
)]
fn lit<S>(input: Span<S>) -> _
where
    S: Clone + Utf8Parsable,
    for<'a> S::Slice<'a>: Debug,
{
    // Parse the characters
    let (rem, val): (Span<S>, Span<S>) = match digit1().parse(input) {
        Ok(res) => res,
        Err(SnackError::Recoverable(err)) => return Err(SnackError::Recoverable(err)),
        Err(SnackError::Fatal(_) | SnackError::NotEnough { .. }) => unreachable!(),
    };

    // Convert to an integer
    let mut value: u64 = 0;
    for b in val.bytes() {
        if *b >= b'0' && *b <= b'9' {
            let i: u64 = (*b - b'0') as u64;
            if value > (u64::MAX - i) / 10 {
                return Err(SnackError::Fatal(ParseError::Overflow));
            }
            value *= 10;
            value += i;
        } else {
            unreachable!();
        }
    }

    // Ok!
    Ok((rem, value))
}





/***** ENTRYPOINT *****/
fn main() {
    let span1 = Span::new("5");
    let (_, exp) = expr().parse(span1).unwrap();
    assert_eq!(exp.compute(), 5);

    let span2 = Span::new("5+5");
    let (_, exp) = expr().parse(span2).unwrap();
    assert_eq!(exp.compute(), 10);

    let span3 = Span::new("42+0+33+5");
    let (_, exp) = expr().parse(span3).unwrap();
    assert_eq!(exp.compute(), 80);
}
