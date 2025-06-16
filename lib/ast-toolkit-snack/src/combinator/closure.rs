//  CLOSURE.rs
//    by Lut99
//
//  Created:
//    18 Mar 2025, 16:14:07
//  Last edited:
//    08 May 2025, 11:18:45
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a combinator wrapping some arbitrary combinator function.
//

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, Spanning};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ParseError};


/***** ERRORS *****/
#[derive(Debug, Eq, PartialEq)]
pub struct Recoverable<F, E> {
    pub expected: F,
    pub err:      E,
}
impl<F: Debug + Display, E> Display for Recoverable<F, E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", ExpectsFormatter { expected: &self.expected }) }
}
impl<F: Debug + Display, E: Debug> Error for Recoverable<F, E> {}
impl<F, E: Spanning<S>, S: Clone> Spanning<S> for Recoverable<F, E> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { self.err.span() }

    #[inline]
    fn into_span(self) -> Span<S> { self.err.into_span() }
}
impl<F: Debug + Display, E: ParseError<S>, S: Clone> ParseError<S> for Recoverable<F, E> {
    #[inline]
    #[track_caller]
    fn more_might_fix(&self) -> bool { self.err.more_might_fix() }

    #[inline]
    #[track_caller]
    fn needed_to_fix(&self) -> Option<usize> { self.err.needed_to_fix() }
}





/***** FORMATTERS *****/
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<F> {
    expected: F,
}
impl<F: Debug + Display> Display for ExpectsFormatter<F> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl<F: Debug + Display> crate::ExpectsFormatter for ExpectsFormatter<F> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter<'_>, _indent: usize) -> FResult { <F as Display>::fmt(&self.expected, f) }
}





/***** COMBINATORS *****/
pub struct Closure<F, C, O, E1, E2, S> {
    expected: F,
    closure: C,
    _output: PhantomData<O>,
    _recoverable: PhantomData<E1>,
    _fatal: PhantomData<E2>,
    _s: PhantomData<S>,
}
impl<'a, 'b, 'c, 's, F, C, O, E1, E2, S> Combinator<'c, 's, S> for Closure<F, C, O, E1, E2, S>
where
    'c: 'a,
    'c: 'b,
    F: 'a + Clone + Debug + Display,
    C: 'b + FnMut(Span<S>) -> SResult<O, E1, E2, S>,
    E1: ParseError<S>,
    E2: ParseError<S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<F>;
    type Output = O;
    type Recoverable = Recoverable<F, E1>;
    type Fatal = E2;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { expected: self.expected.clone() } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match (self.closure)(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(Recoverable { expected: self.expected.clone(), err })),
            Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(err)),
        }
    }
}





/***** LIBRARY *****/
#[inline]
pub const fn closure<'s, F, C, O, E1, E2, S>(expected: F, closure: C) -> Closure<F, C, O, E1, E2, S>
where
    F: Clone + Debug + Display,
    C: FnMut(Span<S>) -> SResult<O, E1, E2, S>,
    E1: ParseError<S>,
    E2: ParseError<S>,
    S: Clone + Spannable<'s>,
{
    Closure { expected, closure, _output: PhantomData, _recoverable: PhantomData, _fatal: PhantomData, _s: PhantomData }
}
