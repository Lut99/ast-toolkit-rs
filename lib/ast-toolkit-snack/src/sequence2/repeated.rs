//  REPEATED.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 18:14:44
//  Last edited:
//    18 Jan 2025, 18:04:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`repeated()`]-combinator.
//

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning};
use better_derive::{Debug, Eq, PartialEq};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator2, ExpectsFormatter as _};


/***** ERRORS *****/
/// Defines the recoverable error thrown by [`Repeated`].
#[derive(Debug, Eq, PartialEq)]
pub struct Recoverable<C, E, F, S> {
    /// What we're expected.
    pub fmt:  C,
    /// How many times we've seen it.
    pub got:  usize,
    /// How many times we expect it.
    pub n:    usize,
    /// The span where we expected the problem.
    pub span: Span<F, S>,
    /// The nested error for trace purposes.
    pub err:  E,
}
impl<C: crate::ExpectsFormatter, E, F, S> Display for Recoverable<C, E, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", ExpectsFormatter { fmt: &self.fmt, n: self.n }) }
}
impl<C: crate::ExpectsFormatter, E: 'static + Error, F, S> Error for Recoverable<C, E, F, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> { Some(&self.err) }
}
impl<C, E, F: Clone, S: Clone> Spanning<F, S> for Recoverable<C, E, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.span.clone() }

    #[inline]
    fn into_span(self) -> Span<F, S> { self.span }
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
pub struct Repeated<C, F, S> {
    /// The nested combinator to repeat.
    comb: C,
    /// The number of times to apply it.
    n:    usize,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C, F, S> Combinator2<'t, F, S> for Repeated<C, F, S>
where
    C: Combinator2<'t, F, S>,
    F: Clone,
    S: Clone,
{
    type ExpectsFormatter = ExpectsFormatter<C::ExpectsFormatter>;
    type Output = Vec<C::Output>;
    type Recoverable = Recoverable<C::ExpectsFormatter, C::Recoverable, F, S>;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects(), n: self.n } }

    #[inline]
    fn parse(&mut self, mut input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
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
                Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
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
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::sequence2::repeated;
/// use ast_toolkit_snack::utf82::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hellohellohello");
/// let span2 = Span::new("<example>", "hellohellohellohello");
/// let span3 = Span::new("<example>", "hellohello");
/// let span4 = Span::new("<example>", "hellohellohel");
///
/// let mut comb = repeated(3, tag("hello"));
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
///         fmt:  tag::ExpectsFormatter { tag: "hello" },
///         got:  2,
///         n:    3,
///         span: span3.slice(10..),
///         err:  tag::Recoverable { tag: "hello", span: span3.slice(10..) },
///     }))
/// );
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(repeated::Recoverable {
///         fmt:  tag::ExpectsFormatter { tag: "hello" },
///         got:  2,
///         n:    3,
///         span: span4.slice(10..),
///         err:  tag::Recoverable { tag: "hello", span: span4.slice(10..) },
///     }))
/// );
/// ```
#[inline]
pub const fn repeated<'t, C, F, S>(n: usize, comb: C) -> Repeated<C, F, S>
where
    C: Combinator2<'t, F, S>,
{
    Repeated { comb, n, _f: PhantomData, _s: PhantomData }
}
