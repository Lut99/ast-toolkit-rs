//  ADD.rs
//    by Lut99
//
//  Created:
//    07 Aug 2024, 22:04:09
//  Last edited:
//    23 Aug 2024, 11:57:47
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines an example parser for a language that consists of arbitrary
//!   additions.
//

use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};

use ast_toolkit_snack::error::{Common as SCommon, Error as SError};
use ast_toolkit_snack::span::{MatchBytes, WhileUtf8};
use ast_toolkit_snack::utf8::complete::{digit1, tag};
use ast_toolkit_snack::{comb, Combinator as _, Result as SResult};
use ast_toolkit_span::{Span, SpannableAsBytes};


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
#[comb(Combinator = ExprComb, expected = ("An {}", EXPRESSION_NAME), Output = Expr, Error = ParseError)]
fn expr<F, S>(input: Span<F, S>) -> _
where
    F: Clone,
    S: Clone + MatchBytes + SpannableAsBytes + WhileUtf8,
{
    // Always parse a number first
    let (rem, val) = match lit().parse(input) {
        SResult::Ok(rem, val) => (rem, Expr::Lit(val)),
        SResult::Fail(fail) => return SResult::Fail(fail),
        SResult::Error(err) => return SResult::Error(err),
    };

    // Then recursively parse the rest if there's an addition
    match tag("+").parse(rem.clone()) {
        SResult::Ok(rem, _) => expr().parse(rem).map(|expr| Expr::Add(Box::new(val), Box::new(expr))),
        // If there's no addition, we are still OK
        SResult::Fail(_) => SResult::Ok(rem, val),
        SResult::Error(err) => SResult::Error(err.transmute()),
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
#[comb(expected = "A literal number", Output = u64, Error = ParseError)]
fn lit<F, S>(input: Span<F, S>) -> _
where
    F: Clone,
    S: Clone + SpannableAsBytes + WhileUtf8,
{
    // Parse the characters
    let (rem, val): (Span<F, S>, Span<F, S>) = match digit1().parse(input) {
        SResult::Ok(rem, val) => (rem, val),
        SResult::Fail(fail) => return SResult::Fail(fail.transmute()),
        SResult::Error(err) => return SResult::Error(err.transmute()),
    };

    // Convert to an integer
    let mut value: u64 = 0;
    for b in val.as_bytes() {
        if *b >= b'0' && *b <= b'9' {
            let i: u64 = (*b - b'0') as u64;
            if value > (u64::MAX - i) / 10 {
                return SResult::Error(SError::Common(SCommon::Custom(ParseError::Overflow)));
            }
            value *= 10;
            value += i;
        } else {
            unreachable!();
        }
    }

    // Ok!
    SResult::Ok(rem, value)
}





/***** ENTRYPOINT *****/
fn main() {
    let span1 = Span::new("<example>", "5");
    let (_, exp) = expr().parse(span1).unwrap();
    assert_eq!(exp.compute(), 5);

    let span2 = Span::new("<example>", "5+5");
    let (_, exp) = expr().parse(span2).unwrap();
    assert_eq!(exp.compute(), 10);

    let span3 = Span::new("<example>", "42+0+33+5");
    let (_, exp) = expr().parse(span3).unwrap();
    assert_eq!(exp.compute(), 80);
}
