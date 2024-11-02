//  WHILE 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:40:18
//  Last edited:
//    02 Nov 2024, 12:14:29
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`while1()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::{Span, Spannable};

pub use super::super::complete::{While1ExpectsFormatter, While1Recoverable};
use crate::result::{Result as SResult, SnackError};
use crate::span::WhileUtf8;
use crate::{Combinator2, Expects};


/***** COMBINATORS *****/
/// Actual combinator implementing [`While1()`].
#[derive(Debug)]
pub struct While1<P, F, S> {
    predicate: P,
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<P, F, S> Expects<'static> for While1<P, F, S> {
    type Formatter = While1ExpectsFormatter;

    #[inline]
    fn expects(&self) -> Self::Formatter { While1ExpectsFormatter }
}
impl<P, F, S> Combinator2<'static, F, S> for While1<P, F, S>
where
    P: for<'a> FnMut(&'a str) -> bool,
    F: Clone,
    S: Clone + Spannable + WhileUtf8,
{
    type Output = Span<F, S>;
    type Recoverable = While1Recoverable<F, S>;
    type Fatal = Infallible;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        // Check first if there's *any* input to parse.
        if input.is_empty() {
            return Err(SnackError::NotEnough { needed: Some(1), span: input });
        }

        // Otherwise, continue as usual
        let match_point: usize = input.while_utf8(SpanRange::Open, &mut self.predicate);
        if match_point > 0 {
            Ok((input.slice(match_point..), input.slice(..match_point)))
        } else {
            Err(SnackError::Recoverable(While1Recoverable { span: input }))
        }
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many characters from the start of a span as possible, as long as those
/// characters match a given predicate.
///
/// This version does _not_ accept matching none of them. See [`while0()`](super::while0()) to also
/// allow finding none.
///
/// # Arguments
/// - `predicate`: A closure that returns true for matching characters, and false for non-matching
///   characters. All characters that are matched are returned up to the first for which
///   `predicate` returns false (if any).
///
/// # Returns
/// A combinator [`While1`] that will match the prefix of input as long as those characters match
/// the given `predicate`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one character. If this match failed
/// because end-of-input was reached, then this fails with a [`SnackError::NotEnough`] instead.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::{While1Recoverable, while1};
/// use ast_toolkit_snack::{Combinator2 as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "abcdefg");
/// let span2 = Span::new("<example>", "cdefghi");
/// let span3 = Span::new("<example>", "abÿcdef");
/// let span4 = Span::new("<example>", "hijklmn");
/// let span5 = Span::new("<example>", "");
///
/// let mut comb = while1(|c: &str| -> bool { c == "a" || c == "b" || c == "c" || c == "ÿ" });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(comb.parse(span4), Err(SnackError::Recoverable(While1Recoverable { span: span4 })));
/// assert_eq!(comb.parse(span5), Err(SnackError::Recoverable(While1Recoverable { span: span5 })));
/// ```
#[inline]
pub const fn while1<P, F, S>(predicate: P) -> While1<P, F, S>
where
    P: for<'a> FnMut(&'a str) -> bool,
    F: Clone,
    S: Clone + Spannable + WhileUtf8,
{
    While1 { predicate, _f: PhantomData, _s: PhantomData }
}
