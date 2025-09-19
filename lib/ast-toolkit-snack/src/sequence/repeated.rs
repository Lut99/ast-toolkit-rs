//  REPEATED.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 18:14:44
//  Last edited:
//    08 May 2025, 11:20:44
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`repeated()`]-combinator.
//

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, Spanning, SpanningInf, SpanningMut, SpanningRef};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ExpectsFormatter as _, ParseError};


/***** ERRORS *****/
/// Defines the recoverable error thrown by [`Repeated`].
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'s, C, E, S>, bound = (C: r#trait, E: r#trait, S: Spannable<'s>))]
pub struct Recoverable<C, E, S> {
    /// What we're expected.
    pub fmt:  C,
    /// How many times we've seen it.
    pub got:  usize,
    /// How many times we expect it.
    pub n:    usize,
    /// The span where we expected the problem.
    pub span: Span<S>,
    /// The nested error for trace purposes.
    pub err:  E,
}
impl<C: crate::ExpectsFormatter, E, S> Display for Recoverable<C, E, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", ExpectsFormatter { fmt: &self.fmt, n: self.n }) }
}
impl<'s, C: crate::ExpectsFormatter, E: Error, S: Spannable<'s>> Error for Recoverable<C, E, S> {}
impl<C, E, S: Clone> Spanning<S> for Recoverable<C, E, S> {
    #[inline]
    fn get_span(&self) -> Option<Cow<'_, Span<S>>> { Some(Cow::Borrowed(&self.span)) }

    #[inline]
    fn take_span(self) -> Option<Span<S>> { Some(self.span) }
}
impl<C, E, S: Clone> SpanningInf<S> for Recoverable<C, E, S> {
    #[inline]
    fn span(&self) -> Cow<'_, Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}
impl<C, E, S: Clone> SpanningRef<S> for Recoverable<C, E, S> {
    #[inline]
    fn span_ref(&self) -> &Span<S> { &self.span }
}
impl<C, E, S: Clone> SpanningMut<S> for Recoverable<C, E, S> {
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> { &mut self.span }
}
impl<'s, C: crate::ExpectsFormatter, E: ParseError<S>, S: Clone + Spannable<'s>> ParseError<S> for Recoverable<C, E, S> {
    #[inline]
    #[track_caller]
    fn more_might_fix(&self) -> bool { self.err.more_might_fix() }

    #[inline]
    #[track_caller]
    fn needed_to_fix(&self) -> Option<usize> { self.err.needed_to_fix() }
}





/***** FORMATTERS *****/
/// Expects formatter for the [`Repeated`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<F> {
    /// The formatter of the nested combinator.
    pub fmt: F,
    /// The number of times to apply it.
    pub n:   usize,
}
impl<F: crate::ExpectsFormatter> Display for ExpectsFormatter<F> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<F: crate::ExpectsFormatter> crate::ExpectsFormatter for ExpectsFormatter<F> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "exactly {} repetitions of ", self.n)?;
        self.fmt.expects_fmt(f, indent)
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`repeated()`]-combinator.
pub struct Repeated<C, S> {
    /// The nested combinator to repeat.
    comb: C,
    /// The number of times to apply it.
    n:    usize,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, S> Combinator<'c, 's, S> for Repeated<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<C::ExpectsFormatter>;
    type Output = Vec<C::Output>;
    type Recoverable = Recoverable<C::ExpectsFormatter, C::Recoverable, S>;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects(), n: self.n } }

    #[inline]
    fn parse(&mut self, mut input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        let mut results: Vec<C::Output> = Vec::with_capacity(self.n);
        for i in 0..self.n {
            // Attempt to parse this try
            match self.comb.parse(input.clone()) {
                Ok((rem, res)) => {
                    input = rem;
                    results.push(res);
                },
                Err(SnackError::Recoverable(err)) => {
                    return Err(SnackError::Recoverable(Recoverable { fmt: self.comb.expects(), got: i, n: self.n, span: input, err }));
                },
                // TODO: Also wrap this in an error for nicer tracing, but wait until `Diagnostics` are done so we know how that looks.
                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(err)),
            }
        }
        Ok((input, results))
    }
}





/***** LIBRARY *****/
/// Applies the given combinator exactly the given amount of times.
///
/// At face value, this combinator may seem similar to [`many0()`](crate::multi::many0()) or
/// [`many1()`](crate::multi::many1()). However, those combinators are greedy, in that they parse
/// as many matches as possible; this combinator, however, expects at least _and at most_ the
/// given number of values.
///
/// # Arguments
/// - `n`: The number of times to apply `comb`.
/// - `comb`: The combinator to apply `n` times.
///
/// # Returns
/// A combinator that will apply `comb` exactly `n` times, or fails trying.
///
/// # Fails
/// The returned combinator fails recoverably if there were not at least `n` occurrences of
/// whatever `comb` parses on the head of the input. Additionally, if `comb` fails fatally, that
/// error is propagated fatally as well.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_snack::sequence::repeated;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("hellohellohello");
/// let span2 = Span::new("hellohellohellohello");
/// let span3 = Span::new("hellohello");
/// let span4 = Span::new("hellohellohel");
///
/// let mut comb = repeated(3, tag(b"hello"));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(15..), vec![span1.slice(..5), span1.slice(5..10), span1.slice(10..15)]))
/// );
/// assert_eq!(
///     comb.parse(span2),
///     Ok((span2.slice(15..), vec![span2.slice(..5), span2.slice(5..10), span2.slice(10..15)]))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(repeated::Recoverable {
///         fmt:  tag::ExpectsFormatter { tag: b"hello" },
///         got:  2,
///         n:    3,
///         span: span3.slice(10..),
///         err:  tag::Recoverable { tag: b"hello", is_fixable: true, span: span3.slice(10..) },
///     }))
/// );
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(repeated::Recoverable {
///         fmt:  tag::ExpectsFormatter { tag: b"hello" },
///         got:  2,
///         n:    3,
///         span: span4.slice(10..),
///         err:  tag::Recoverable { tag: b"hello", is_fixable: true, span: span4.slice(10..) },
///     }))
/// );
/// ```
#[inline]
pub const fn repeated<'c, 's, C, S>(n: usize, comb: C) -> Repeated<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    Repeated { comb, n, _s: PhantomData }
}
