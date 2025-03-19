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

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};

use ast_toolkit_snack::combinator::closure;
use ast_toolkit_snack::result::SnackError;
use ast_toolkit_snack::span::{BytesParsable as _, Utf8Parsable};
use ast_toolkit_snack::utf8::complete::{digit1, tag};
use ast_toolkit_snack::{Combinator, branch};
use ast_toolkit_span::{Span, Spannable};
use better_derive::{Debug, Eq, PartialEq};


/***** ERRORS *****/
/// Defines custom errors during parsing.
#[derive(Debug, Eq, PartialEq)]
enum Fatal<S> {
    /// Integer overflow!
    Overflow { span: Span<S> },
}
impl<S> Display for Fatal<S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Overflow { .. } => write!(f, "Integer overflow"),
        }
    }
}
impl<S: Spannable> Error for Fatal<S> {}
impl<S: Clone> ast_toolkit_span::Spanning<S> for Fatal<S> {
    #[inline]
    fn span(&self) -> std::borrow::Cow<Span<S>> {
        match self {
            Self::Overflow { span } => Cow::Borrowed(span),
        }
    }
    #[inline]
    fn into_span(self) -> Span<S> {
        match self {
            Self::Overflow { span } => span,
        }
    }
}





/***** AST *****/
#[derive(Debug)]
enum Expr {
    Add(Box<Self>, Box<Self>),
    Lit(Lit),
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
            Self::Lit(v) => v.compute(),
        }
    }
}

#[derive(Debug)]
enum Lit {
    Int(u64),
}
impl Lit {
    /// Computes the value of the literal.
    ///
    /// # Returns
    /// A number carrying this literal's value.
    #[inline]
    fn compute(&self) -> u64 {
        match self {
            Self::Int(val) => *val,
        }
    }
}





/***** PARSERS *****/
// The [`expr()`] is rather complicated due to recursion, so we'll create a fully-fledged
// combinator out of it.
mod expr {
    use std::marker::PhantomData;

    use ast_toolkit_snack::result::{BoxedParseError, Result as SResult};

    use super::*;

    /// The combinator implementation itself.
    pub(super) struct Expr<S> {
        pub(super) _s: PhantomData<S>,
    }
    impl<'s, 't, S> Combinator<'t, S> for Expr<S>
    where
        's: 't,
        S: 's + Clone + Utf8Parsable,
    {
        type ExpectsFormatter = &'static str;
        type Output = super::Expr;
        type Recoverable = BoxedParseError<'t, S>;
        type Fatal = BoxedParseError<'t, S>;

        #[inline]
        fn expects(&self) -> Self::ExpectsFormatter { "Expected an expression" }

        #[inline]
        fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
            // Always parse a number first
            let (rem, val) = match lit().parse(input) {
                Ok(res) => res,
                Err(SnackError::Recoverable(err)) => return Err(SnackError::Recoverable(BoxedParseError::new(err))),
                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(BoxedParseError::new(err))),
                Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
            };

            // Then recursively parse the rest if there's an addition
            match tag("+").parse(rem.clone()) {
                Ok((rem, _)) => expr().parse(rem).map(|(rem, expr)| (rem, super::Expr::Add(Box::new(super::Expr::Lit(val)), Box::new(expr)))),
                // If there's no addition, we are still OK
                Err(SnackError::Recoverable(_)) => Ok((rem, super::Expr::Lit(val))),
                Err(SnackError::Fatal(_)) => unreachable!(),
                Err(SnackError::NotEnough { needed, span }) => Err(SnackError::NotEnough { needed, span }),
            }
        }
    }
}
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
const fn expr<S>() -> expr::Expr<S>
where
    S: Clone + Utf8Parsable,
{
    expr::Expr { _s: std::marker::PhantomData }
}

/// Parses any literal.
///
/// For now, only [numbers](LitInt) are supported.
///
/// # Returns
/// A combinator capable of parsing literals.
///
/// # Fails
/// The combinator will fail recoverably if the input does not start with a literal (i.e., at least
/// one digit).
///
/// It fails fatally if it _did_ start with a digit, but it overflows for our internal
/// representation.
const fn lit<'s, S>() -> impl Combinator<'s, S, Output = Lit>
where
    S: 's + Clone + Utf8Parsable,
{
    closure("A literal", |input| branch::alt((lit_int(),)).parse(input))
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
const fn lit_int<'s, S>() -> impl Combinator<'s, S, Output = Lit>
where
    S: 's + Clone + Utf8Parsable,
{
    closure("An integer literal", |input: Span<S>| {
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
                    return Err(SnackError::Fatal(Fatal::Overflow { span: val.clone() }));
                }
                value *= 10;
                value += i;
            } else {
                unreachable!();
            }
        }

        // Ok!
        Ok((rem, Lit::Int(value)))
    })
}





/***** ENTRYPOINT *****/
fn main() {
    let source: String = "5".into();
    let span1 = Span::new(source.as_str());
    let (_, exp): (_, Expr) = expr().parse(span1).unwrap();
    assert_eq!(exp.compute(), 5);

    let span2 = Span::new("5+5");
    let (_, exp): (_, Expr) = expr().parse(span2).unwrap();
    assert_eq!(exp.compute(), 10);

    let span3 = Span::new("42+0+33+5");
    let (_, exp): (_, Expr) = expr().parse(span3).unwrap();
    assert_eq!(exp.compute(), 80);
}
