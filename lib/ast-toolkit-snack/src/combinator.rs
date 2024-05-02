//  COMB.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 18:01:57
//  Last edited:
//    02 May 2024, 10:56:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Some miscellaneous combinators that operate on other combinators.
//

use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning};

use crate::error::{Common, Failure};
use crate::{Combinator, Expects, ExpectsFormatter, Result};


/***** LIBRARY FUNCTIONS *****/
/// Implements the reverse of a combinator.
///
/// Specifically, will return `Result::Ok(())` if the combinator [`Result::Fail`]s, or a [`Result::Fail`] if it [`Result::Ok`]'s.
///
/// # Arguments
/// - `comb`: The [`Combinator`] to negate.
///
/// # Returns
/// A combinator [`Not`] that will succeed (but match nothing) if the given `comb`inator fails.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator succeeds.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::combinator::not;
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = not(tag("Goodbye"));
/// assert_eq!(comb.parse(span1).unwrap(), (span1, ()));
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::Not { .. }))));
/// ```
#[inline]
pub fn not<'c, F, S, C>(comb: C) -> Not<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'c, F, S>,
    C::Output: Spanning<F, S>,
{
    Not { comb, _f: Default::default(), _s: Default::default() }
}



/// Maps the result of a combinator to something else.
///
/// # Arguments
/// - `comb`: Some combinator to run.
/// - `func`: Some closure that takes the `comb`'s result and maps it to something else - including some kind of map error.
///
/// # Returns
/// A combinator [`Map`] that runs the given `comb`inator, and then maps the result using `func`.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::combinator::map;
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// #[derive(Debug, PartialEq)]
/// struct Hello;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = map(tag("Hello"), |_parsed| Hello);
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), Hello));
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::TagUtf8 { .. }))));
/// ```
#[inline]
pub fn map<'c, F, S, R1, R2, C, M>(comb: C, func: M) -> Map<F, S, C, M>
where
    C: Combinator<'c, F, S, Output = R1>,
    M: FnMut(R1) -> R2,
{
    Map { comb, func, _f: Default::default(), _s: Default::default() }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Not`] combinator.
#[derive(Debug)]
pub struct NotExpects<'t> {
    /// The thing we _don't_ expect.
    fmt: Box<dyn 't + ExpectsFormatter>,
}
impl<'t> Display for NotExpects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for NotExpects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "not ")?;
        self.fmt.expects_fmt(f, indent)
    }
}

/// ExpectsFormatter for the [`Map`] combinator.
#[derive(Debug)]
pub struct MapExpects<'t> {
    /// The thing we expect.
    fmt: Box<dyn 't + ExpectsFormatter>,
}
impl<'t> Display for MapExpects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for MapExpects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult { self.fmt.expects_fmt(f, indent) }
}





/***** LIBRARY *****/
/// The concrete type returned by [`not()`].
pub struct Not<F, S, C> {
    /// The combinator to negate.
    comb: C,
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f:   PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<'t, F, S, C: Expects<'t>> Expects<'t> for Not<F, S, C> {
    type Formatter = NotExpects<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter { NotExpects { fmt: Box::new(self.comb.expects()) } }
}
impl<'c, F, S, C> Combinator<'c, F, S> for Not<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'c, F, S>,
    C::Output: Spanning<F, S>,
{
    type Output = ();

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        match self.comb.parse(input.clone()) {
            Result::Ok(_, res) => Result::Fail(Failure::Common(Common::Not { expects: Box::new(self.comb.expects()), span: res.span() })),
            Result::Fail(_) => Result::Ok(input, ()),
            Result::Error(err) => Result::Error(err),
        }
    }
}



/// The concrete type returned by [`map()`].
pub struct Map<F, S, C, M> {
    /// The combinator who's output to negate.
    comb: C,
    /// The mapping closure.
    func: M,
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f:   PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<'t, F, S, C: Expects<'t>, M> Expects<'t> for Map<F, S, C, M> {
    type Formatter = MapExpects<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter { MapExpects { fmt: Box::new(self.comb.expects()) } }
}
impl<'c, F, S, R1, R2, C, M> Combinator<'c, F, S> for Map<F, S, C, M>
where
    C: Combinator<'c, F, S, Output = R1>,
    M: FnMut(R1) -> R2,
{
    type Output = R2;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        match self.comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, (self.func)(res)),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}
