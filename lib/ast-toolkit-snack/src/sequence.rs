//  SEQUENCE.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:35:22
//  Last edited:
//    02 May 2024, 14:40:45
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators that apply other combinators in a particular order.
//

use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use crate::{Combinator, Expects, ExpectsFormatter, Result};


/***** LIBRARY FUNCTIONS *****/
/// Applies the first combinator, then applies the second.
///
/// # Arguments
/// - `first`: The first combinator to match.
/// - `second`: The second combinator to match.
///
/// # Returns
/// A combinator [`Tuple`] that will first apply the `first combinator, and then the `second`.
///
/// # Fails
/// The returned combinator fails if either the `first` or the `second` combinator fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::sequence::pair;
/// use ast_toolkit_snack::utf8::complete::{digit1, tag};
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello123");
/// let span2 = Span::new("<example>", "123");
/// let span3 = Span::new("<example>", "HelloWorld");
///
/// let mut comb = pair(tag("Hello"), digit1());
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(8..), (span1.slice(..5), span1.slice(5..8)))
/// );
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::TagUtf8 { .. }))));
/// assert!(matches!(comb.parse(span3), SResult::Fail(Failure::Common(Common::Digit1 { .. }))));
/// ```
#[inline]
pub fn pair<'c, F, S, C1, C2>(first: C1, second: C2) -> Tuple<F, S, (C1, C2)>
where
    C1: Combinator<'c, F, S>,
    C2: Combinator<'c, F, S>,
{
    Tuple { tuple: (first, second), _f: Default::default(), _s: Default::default() }
}

/// Applies a tuple of combinators, in-order.
///
/// This combinator will try the given ones one-by-one. It either returns the first non-OK [`Result`], or a same-sized tuple of all the combinator's results.
///
/// # Note
/// Actually, this combinator is really a no-op, and simply returns the given combinator. This is possible because tuples simply implement [`Combinator`].  
/// Only tuples up to a size of 16 is implemented. For more, consider nesting tuples within each other.
///
/// # Arguments
/// - `combs`: The tuple of combinators to apply.
///
/// # Returns
/// A combinator [`Tuple`] that will first apply the given combinators in-order.
///
/// # Fails
/// The returned combinator fails if any of the given `combs`inators fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::sequence::tuple;
/// use ast_toolkit_snack::utf8::complete::{digit1, tag};
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello123");
/// let span2 = Span::new("<example>", "123");
/// let span3 = Span::new("<example>", "HelloWorld");
///
/// let mut comb = tuple((tag("Hello"), digit1()));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(8..), (span1.slice(..5), span1.slice(5..8)))
/// );
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::TagUtf8 { .. }))));
/// assert!(matches!(comb.parse(span3), SResult::Fail(Failure::Common(Common::Digit1 { .. }))));
/// ```
#[inline]
pub fn tuple<'c, F, S, C>(combs: C) -> Tuple<F, S, C>
where
    C: Combinator<'c, F, S>,
{
    Tuple { tuple: combs, _f: Default::default(), _s: Default::default() }
}

/// Applies the first combinator, then applies the second and discards the result.
///
/// This is useful to parse trailing comma's, for example, where you don't care about the second but do need to parse it to enforce syntax.
///
/// # Arguments
/// - `first`: The first combinator to match.
/// - `sep`: The second combinator to match and then who's result to discard.
///
/// # Returns
/// A combinator [`Terminated`] that will first apply the `first` combinator, then the `sep` and discards its result.
///
/// # Fails
/// The returned combinator fails if either `first` or `sep` fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::sequence::terminated;
/// use ast_toolkit_snack::utf8::complete::{digit1, tag};
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello123");
/// let span2 = Span::new("<example>", "123");
/// let span3 = Span::new("<example>", "HelloWorld");
///
/// let mut comb = terminated(tag("Hello"), digit1());
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(8..), span1.slice(..5)));
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::TagUtf8 { .. }))));
/// assert!(matches!(comb.parse(span3), SResult::Fail(Failure::Common(Common::Digit1 { .. }))));
/// ```
#[inline]
pub fn terminated<'c, F, S, C1, C2>(first: C1, sep: C2) -> Terminated<F, S, C1, C2>
where
    C1: Combinator<'c, F, S>,
    C2: Combinator<'c, F, S>,
{
    Terminated { first, sep, _f: Default::default(), _s: Default::default() }
}

/// Applies the first combinator and discards the result, then applies the second.
///
/// This is useful to parse preceding colons, for example, where you don't care about the first but do need to parse it to enforce syntax.
///
/// # Arguments
/// - `sep`: The first combinator to match and then who's result to discard.
/// - `second`: The second combinator to match.
///
/// # Returns
/// A combinator [`Preceded`] that will first apply the `sep` combinator, discarding its result, and then the `second` combinator.
///
/// # Fails
/// The returned combinator fails if either `sep` or `second` fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::sequence::preceded;
/// use ast_toolkit_snack::utf8::complete::{digit1, tag};
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello123");
/// let span2 = Span::new("<example>", "123");
/// let span3 = Span::new("<example>", "HelloWorld");
///
/// let mut comb = preceded(tag("Hello"), digit1());
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(8..), span1.slice(5..8)));
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::TagUtf8 { .. }))));
/// assert!(matches!(comb.parse(span3), SResult::Fail(Failure::Common(Common::Digit1 { .. }))));
/// ```
#[inline]
pub fn preceded<'c, F, S, C1, C2>(sep: C1, second: C2) -> Preceded<F, S, C1, C2>
where
    C1: Combinator<'c, F, S>,
    C2: Combinator<'c, F, S>,
{
    Preceded { sep, second, _f: Default::default(), _s: Default::default() }
}

/// Parses two values separated by some combinator who's value we don't care about.
///
/// This is useful to parse some separated values.
///
/// # Arguments
/// - `first`: The first combinator to match.
/// - `sep`: The second combinator to match and then discard the value of.
/// - `third`: The third combinator to match.
///
/// # Returns
/// A combinator [`SeparatedPair`] that will first apply the `first` combinator, then the `sep` combinator, discarding its result, and finally the `third` combinator.
///
/// # Fails
/// The returned combinator fails if either `first`, `sep`, or `third` fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::sequence::separated_pair;
/// use ast_toolkit_snack::utf8::complete::{digit1, tag};
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello123Goodbye");
/// let span2 = Span::new("<example>", "123Goodbye");
/// let span3 = Span::new("<example>", "HelloWorldGoodbye");
/// let span4 = Span::new("<example>", "Hello123Hello");
///
/// let mut comb = separated_pair(tag("Hello"), digit1(), tag("Goodbye"));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(15..), (span1.slice(..5), span1.slice(8..)))
/// );
/// assert!(matches!(
///     comb.parse(span2),
///     SResult::Fail(Failure::Common(Common::TagUtf8 { tag: "Hello", .. }))
/// ));
/// assert!(matches!(comb.parse(span3), SResult::Fail(Failure::Common(Common::Digit1 { .. }))));
/// assert!(matches!(
///     comb.parse(span4),
///     SResult::Fail(Failure::Common(Common::TagUtf8 { tag: "Goodbye", .. }))
/// ));
/// ```
#[inline]
pub fn separated_pair<'c, F, S, C1, C2, C3>(first: C1, sep: C2, third: C3) -> SeparatedPair<F, S, C1, C2, C3>
where
    C1: Combinator<'c, F, S>,
    C2: Combinator<'c, F, S>,
    C3: Combinator<'c, F, S>,
{
    SeparatedPair { first, sep, third, _f: Default::default(), _s: Default::default() }
}

/// Parses one value surrounded by a preceding and a terminating combinator we don't care about.
///
/// This is useful to parse some parenthesis, where we only care about the middle value but not anything else.
///
/// # Arguments
/// - `sep1`: The first combinator to match and then discard the value of.
/// - `second`: The second combinator to match.
/// - `sep3`: The third combinator to match and then discard the value of.
///
/// # Returns
/// A combinator [`Delimited`] that will first apply the `sep1` combinator without storing its result, then the `second` combinator, and finally the `sep3` combinator, discarding it result as well.
///
/// # Fails
/// The returned combinator fails if either `sep1`, `second`, or `sep3` fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::sequence::delimited;
/// use ast_toolkit_snack::utf8::complete::{digit1, tag};
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello123Goodbye");
/// let span2 = Span::new("<example>", "123Goodbye");
/// let span3 = Span::new("<example>", "HelloWorldGoodbye");
/// let span4 = Span::new("<example>", "Hello123Hello");
///
/// let mut comb = delimited(tag("Hello"), digit1(), tag("Goodbye"));
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(15..), span1.slice(5..8)));
/// assert!(matches!(
///     comb.parse(span2),
///     SResult::Fail(Failure::Common(Common::TagUtf8 { tag: "Hello", .. }))
/// ));
/// assert!(matches!(comb.parse(span3), SResult::Fail(Failure::Common(Common::Digit1 { .. }))));
/// assert!(matches!(
///     comb.parse(span4),
///     SResult::Fail(Failure::Common(Common::TagUtf8 { tag: "Goodbye", .. }))
/// ));
/// ```
#[inline]
pub fn delimited<'c, F, S, C1, C2, C3>(sep1: C1, second: C2, sep3: C3) -> Delimited<F, S, C1, C2, C3>
where
    C1: Combinator<'c, F, S>,
    C2: Combinator<'c, F, S>,
    C3: Combinator<'c, F, S>,
{
    Delimited { sep1, second, sep3, _f: Default::default(), _s: Default::default() }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for two consequtive other formatters.
#[derive(Debug)]
pub struct PairExpects<'t> {
    /// The first thing we expect.
    first_fmt:  Box<dyn 't + ExpectsFormatter>,
    /// The second thing we expect.
    second_fmt: Box<dyn 't + ExpectsFormatter>,
}
impl<'t> Display for PairExpects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for PairExpects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        self.first_fmt.expects_fmt(f, indent)?;
        write!(f, ", then ")?;
        self.second_fmt.expects_fmt(f, indent)
    }
}

/// ExpectsFormatter for three consequtive other formatters.
#[derive(Debug)]
pub struct TripletExpects<'t> {
    /// The first thing we expect.
    first_fmt:  Box<dyn 't + ExpectsFormatter>,
    /// The second thing we expect.
    second_fmt: Box<dyn 't + ExpectsFormatter>,
    /// The third thing we expect.
    third_fmt:  Box<dyn 't + ExpectsFormatter>,
}
impl<'t> Display for TripletExpects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for TripletExpects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        self.first_fmt.expects_fmt(f, indent)?;
        write!(f, ", ")?;
        self.second_fmt.expects_fmt(f, indent)?;
        write!(f, ", then ")?;
        self.third_fmt.expects_fmt(f, indent)
    }
}





/***** LIBRARY COMBINATORS *****/
/// Combinator returned by [`pair()`] and [`tuple()`].
pub struct Tuple<F, S, T> {
    /// The tuple combinator to wrap.
    tuple: T,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:    PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:    PhantomData<S>,
}
impl<'t, F, S, T: Expects<'t>> Expects<'t> for Tuple<F, S, T> {
    type Formatter = T::Formatter;

    #[inline]
    fn expects(&self) -> Self::Formatter { self.tuple.expects() }
}
impl<'c, F, S, T: Combinator<'c, F, S>> Combinator<'c, F, S> for Tuple<F, S, T> {
    type Output = T::Output;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> { self.tuple.parse(input) }
}

/// Combinator returned by [`terminated()`].
pub struct Terminated<F, S, C1, C2> {
    /// The first combinator, which we parse.
    first: C1,
    /// The second combinator, which we parse but discard.
    sep:   C2,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:    PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:    PhantomData<S>,
}
impl<'t, F, S, C1: Expects<'t>, C2: Expects<'t>> Expects<'t> for Terminated<F, S, C1, C2> {
    type Formatter = PairExpects<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter { PairExpects { first_fmt: Box::new(self.first.expects()), second_fmt: Box::new(self.sep.expects()) } }
}
impl<'c, F, S, C1: Combinator<'c, F, S>, C2: Combinator<'c, F, S>> Combinator<'c, F, S> for Terminated<F, S, C1, C2> {
    type Output = C1::Output;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // Parse the first first
        let (rem, res): (Span<F, S>, C1::Output) = match self.first.parse(input) {
            Result::Ok(rem, res) => (rem, res),
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Parse the second
        match self.sep.parse(rem) {
            Result::Ok(rem, _) => Result::Ok(rem, res),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// Combinator returned by [`preceded()`].
pub struct Preceded<F, S, C1, C2> {
    /// The first combinator, which we parse but discard.
    sep:    C1,
    /// The second combinator, which we parse.
    second: C2,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:     PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:     PhantomData<S>,
}
impl<'t, F, S, C1: Expects<'t>, C2: Expects<'t>> Expects<'t> for Preceded<F, S, C1, C2> {
    type Formatter = PairExpects<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter { PairExpects { first_fmt: Box::new(self.sep.expects()), second_fmt: Box::new(self.second.expects()) } }
}
impl<'c, F, S, C1: Combinator<'c, F, S>, C2: Combinator<'c, F, S>> Combinator<'c, F, S> for Preceded<F, S, C1, C2> {
    type Output = C2::Output;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // Parse the first first
        let rem: Span<F, S> = match self.sep.parse(input) {
            Result::Ok(rem, _) => rem,
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Parse the second
        match self.second.parse(rem) {
            Result::Ok(rem, res) => Result::Ok(rem, res),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// Combinator returned by [`separated_pair()`].
pub struct SeparatedPair<F, S, C1, C2, C3> {
    /// The first combinator, which we parse.
    first: C1,
    /// The second combinator, which we parse but discard.
    sep:   C2,
    /// The third combinator, which we parse.
    third: C3,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:    PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:    PhantomData<S>,
}
impl<'t, F, S, C1: Expects<'t>, C2: Expects<'t>, C3: Expects<'t>> Expects<'t> for SeparatedPair<F, S, C1, C2, C3> {
    type Formatter = TripletExpects<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter {
        TripletExpects {
            first_fmt:  Box::new(self.first.expects()),
            second_fmt: Box::new(self.sep.expects()),
            third_fmt:  Box::new(self.third.expects()),
        }
    }
}
impl<'c, F, S, C1: Combinator<'c, F, S>, C2: Combinator<'c, F, S>, C3: Combinator<'c, F, S>> Combinator<'c, F, S>
    for SeparatedPair<F, S, C1, C2, C3>
{
    type Output = (C1::Output, C3::Output);

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // Parse the first first
        let (rem, res1): (Span<F, S>, C1::Output) = match self.first.parse(input) {
            Result::Ok(rem, res) => (rem, res),
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Parse the second, discarding the result
        let rem: Span<F, S> = match self.sep.parse(rem) {
            Result::Ok(rem, _) => rem,
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Finally, parse the third
        match self.third.parse(rem) {
            Result::Ok(rem, res3) => Result::Ok(rem, (res1, res3)),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// Combinator returned by [`delimited()`].
pub struct Delimited<F, S, C1, C2, C3> {
    /// The first combinator, which we parse but discard.
    sep1:   C1,
    /// The second combinator, which we parse.
    second: C2,
    /// The third combinator, which we parse but discard.
    sep3:   C3,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:     PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:     PhantomData<S>,
}
impl<'t, F, S, C1: Expects<'t>, C2: Expects<'t>, C3: Expects<'t>> Expects<'t> for Delimited<F, S, C1, C2, C3> {
    type Formatter = TripletExpects<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter {
        TripletExpects {
            first_fmt:  Box::new(self.sep1.expects()),
            second_fmt: Box::new(self.second.expects()),
            third_fmt:  Box::new(self.sep3.expects()),
        }
    }
}
impl<'c, F, S, C1: Combinator<'c, F, S>, C2: Combinator<'c, F, S>, C3: Combinator<'c, F, S>> Combinator<'c, F, S> for Delimited<F, S, C1, C2, C3> {
    type Output = C2::Output;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // Parse the first first, discarding the result
        let rem: Span<F, S> = match self.sep1.parse(input) {
            Result::Ok(rem, _) => rem,
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Parse the second
        let (rem, res): (Span<F, S>, C2::Output) = match self.second.parse(rem) {
            Result::Ok(rem, res) => (rem, res),
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Finally, parse the third and discard its result also
        match self.sep3.parse(rem) {
            Result::Ok(rem, _) => Result::Ok(rem, res),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}