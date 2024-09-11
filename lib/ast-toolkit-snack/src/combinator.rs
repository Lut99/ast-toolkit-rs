//  COMB.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 18:01:57
//  Last edited:
//    11 Sep 2024, 16:47:14
//  Auto updated?
//    Yes
//
//  Description:
//!   Some miscellaneous combinators that operate on other combinators.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::{Span, Spannable};

use crate::error::{Common, Failure};
use crate::span::LenBytes;
use crate::{Combinator, Expects, ExpectsFormatter, Result};


/***** LIBRARY FUNCTIONS *****/
/// Matches the full input text.
///
/// # Arguments
/// - `comb`: Some combinator to apply to the input text in order to match it.
///
/// # Returns
/// A combinator [`All`] that will apply `comb`, and then fail if there is input left.
///
/// # Fails
/// The returned combinator fails if `comb` fails, or if `comb` did not consume all input.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::combinator::all;
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello");
/// let span2 = Span::new("<example>", "Hello, world!");
///
/// let mut comb = all(tag("Hello"));
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::All { .. }))));
/// ```
#[inline]
pub const fn all<'t, F, S, C>(comb: C) -> All<F, S, C>
where
    C: Combinator<'t, F, S>,
{
    All { comb, _f: PhantomData, _s: PhantomData }
}

/// Discards the output of another combinator.
///
/// This useful when you only want to advance the stream but not get anything out of it.
///
/// # Arguments
/// - `comb`: Some combinator to apply and then to discard the output of.
///
/// # Returns
/// A combinator [`Discard`] that will apply `comb` and discard its input.
///
/// # Fails
/// The returned combinator fails exactly when `comb` fails.
///
/// # Exampel
/// ```rust
/// use ast_toolkit_snack::combinator::discard;
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = discard(tag("Hello"));
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), ()));
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::TagUtf8 { .. }))));
/// ```
#[inline]
pub const fn discard<'t, F, S, C>(comb: C) -> Discard<F, S, C>
where
    C: Combinator<'t, F, S>,
{
    Discard { comb, _f: PhantomData, _s: PhantomData }
}

/// Maps the result of a combinator to something else.
///
/// # Arguments
/// - `comb`: Some combinator to run.
/// - `func`: Some closure that takes the `comb`'s result and maps it to something else.
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
pub const fn map<'c, F, S, R1, R2, C, M>(comb: C, func: M) -> Map<F, S, C, M>
where
    C: Combinator<'c, F, S, Output = R1>,
    M: FnMut(R1) -> R2,
{
    Map { comb, func, _f: PhantomData, _s: PhantomData }
}

/// Maps the custom error in a result of a combinator to something else.
///
/// # Arguments
/// - `comb`: Some combinator to run.
/// - `func`: Some closure that takes potential custom errors emitted by `comb` and maps it to something else.
///
/// # Returns
/// A combinator [`MapErr`] that runs the given `comb`inator, and then maps the result using `func`.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::combinator::map_err;
/// use ast_toolkit_snack::error::{fail, Common, Error, Failure};
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
/// use ast_toolkit_snack::span::MatchBytes;
///
/// struct HelloWorldError;
///
/// // Pretend this is has `Expects` and `Combinator` impls
/// fn hello_world<F: Clone, S: Clone + MatchBytes>(input: Span<F, S>) -> SResult<'static, (), F, S, &'static str> {
///     match tag("Hello, world!").parse(input) {
///         SResult::Ok(rem, _) => SResult::Ok(rem, ()),
///         SResult::Fail(_) => SResult::Fail(Failure::Common(Common::Custom("that's not hello world!"))),
///         SResult::Error(_) => SResult::Error(Error::Common(Common::Custom("that's not hello world!"))),
///     }
/// }
/// # struct HelloWorld;
/// # impl ast_toolkit_snack::Expects<'static> for HelloWorld {
/// #     type Formatter = ast_toolkit_snack::utf8::Digit0Expects;
/// #     fn expects(&self) -> Self::Formatter { ast_toolkit_snack::utf8::Digit0Expects }
/// # }
/// # impl ast_toolkit_snack::Combinator<'static, &'static str, &'static str> for HelloWorld {
/// #     type Output = ();
/// #     type Error = &'static str;
/// #     fn parse(&mut self, input: Span<&'static str, &'static str>) -> SResult<'static, Self::Output, &'static str, &'static str, Self::Error> {
/// #         match tag("Hello, world!").parse(input) {
/// #             SResult::Ok(rem, _) => SResult::Ok(rem, ()),
/// #             SResult::Fail(_) => SResult::Fail(Failure::Common(Common::Custom("that's not hello world!"))),
/// #             SResult::Error(_) => SResult::Error(Error::Common(Common::Custom("that's not hello world!"))),
/// #         }
/// #     }
/// # }
/// # let hello_world = HelloWorld;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = map_err(hello_world, |_| HelloWorldError);
///
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(13..), ()));
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::Custom(HelloWorldError)))));
/// ```
#[inline]
pub const fn map_err<'c, F, S, R, E1, E2, C, M>(comb: C, func: M) -> MapErr<F, S, C, M>
where
    C: Combinator<'c, F, S, Output = R, Error = E1>,
    M: FnMut(E1) -> E2,
{
    MapErr { comb, func, _f: PhantomData, _s: PhantomData }
}

/// Implements a no-op combinator that doesn't consume anything.
///
/// This is useful in case you're working with more general combinators that you don't want to use all features of. A common case is parsing parenthesis with nothing in between them.
///
/// # Returns
/// A combinator [`Nop`] that does not consume anything but always just returns `()`.
///
/// # Fails
/// The returned combinator never fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::combinator::nop;
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = nop();
/// assert_eq!(comb.parse(span1).unwrap(), (span1, ()));
/// assert_eq!(comb.parse(span2).unwrap(), (span2, ()));
/// ```
#[inline]
pub const fn nop<F, S>() -> Nop<F, S> { Nop { _f: PhantomData, _s: PhantomData } }

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
pub const fn not<'t, F, S, C>(comb: C) -> Not<F, S, C>
where
    F: Clone,
    S: Clone + LenBytes,
    C: Combinator<'t, F, S>,
{
    Not { comb, _f: PhantomData, _s: PhantomData }
}

/// Makes parsing a combinator optional.
///
/// In other words, aside from just parsing whatever it is, it is also OK if that combinator fails. [`None`] is returned if that's the case.
///
/// # Arguments
/// - `comb`: The [`Combinator`] to make optional.
///
/// # Returns
/// A combinator [`Opt`] that will succeed pretty much regardless of whether `comb` succeeds.
///
/// # Fails
/// The returned combinator only fails if `comb` is streaming and throws [`Failure::NotEnough`].
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::combinator::opt;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = opt(tag("Hello"));
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), Some(span1.slice(..5))));
/// assert_eq!(comb.parse(span2).unwrap(), (span2, None));
/// ```
#[inline]
pub const fn opt<'t, F, S, C>(comb: C) -> Opt<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'t, F, S>,
{
    Opt { comb, _f: PhantomData, _s: PhantomData }
}

/// Turns the output of a combinator into a [`Span`].
///
/// This is useful when you're doing regex-like matching using many combinators. Applying this
/// combinator to the outer one will ensure the result is simply the matched input.
///
/// # Arguments
/// - `comb`: The [`Combinator`] who's output to turn into a [`Span`].
///
/// # Returns
/// A combinator [`Recognize`] that will copy the behaviour of `comb` but returns the matched
/// input instead of `comb`'s output.
///
/// # Fails
/// This combinator fails exactly when `comb` fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::combinator::recognize;
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::multi::many1;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello");
/// let span2 = Span::new("<example>", "Hello, world!");
/// let span3 = Span::new("<example>", "SUPER Hello, world!");
/// let span4 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = recognize(many1(tag("Hello")));
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1));
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(5..), span2.slice(..5)));
/// assert_eq!(comb.parse(span3.slice(6..)).unwrap(), (span3.slice(11..), span3.slice(6..11)));
/// assert!(matches!(comb.parse(span4), SResult::Fail(Failure::Common(Common::Many1 { .. }))));
/// ```
#[inline]
pub const fn recognize<'t, F, S, C>(comb: C) -> Recognize<F, S, C>
where
    C: Combinator<'t, F, S>,
{
    Recognize { comb, _f: PhantomData, _s: PhantomData }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Nop`] combinator.
#[derive(Debug)]
pub struct NopExpects;
impl Display for NopExpects {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for NopExpects {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "nothing") }
}

/// ExpectsFormatter for the [`Not`] combinator.
#[derive(Debug)]
pub struct NotExpects<E> {
    /// The thing we _don't_ expect.
    pub(crate) fmt: E,
}
impl<E: ExpectsFormatter> Display for NotExpects<E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<E: ExpectsFormatter> ExpectsFormatter for NotExpects<E> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "not ")?;
        self.fmt.expects_fmt(f, indent)
    }
}

/// ExpectsFormatter for the [`Opt`] combinator.
#[derive(Debug)]
pub struct OptExpects<E> {
    /// The thing we maybe expect.
    pub(crate) fmt: E,
}
impl<E: ExpectsFormatter> Display for OptExpects<E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<E: ExpectsFormatter> ExpectsFormatter for OptExpects<E> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "optionally ")?;
        self.fmt.expects_fmt(f, indent)
    }
}





/***** LIBRARY *****/
/// The concrete type returned by [`all()`].
pub struct All<F, S, C> {
    /// The combinator that is supposed to consume all input.
    comb: C,
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f:   PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<'t, F, S, C: Expects<'t>> Expects<'t> for All<F, S, C> {
    type Formatter = C::Formatter;

    #[inline]
    fn expects(&self) -> Self::Formatter { self.comb.expects() }
}
impl<'t, F, S, C> Combinator<'t, F, S> for All<F, S, C>
where
    S: Spannable,
    C: Combinator<'t, F, S>,
{
    type Output = C::Output;
    type Error = C::Error;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S, Self::Error> {
        // First, parse the combinator as usual
        let (rem, res): (Span<F, S>, C::Output) = match self.comb.parse(input) {
            Result::Ok(rem, res) => (rem, res),
            Result::Fail(fail) => return Result::Fail(fail),
            Result::Error(err) => return Result::Error(err),
        };

        // Then assert that nothing is remaining
        if rem.is_empty() { Result::Ok(rem, res) } else { Result::Fail(Failure::Common(Common::All { span: rem })) }
    }
}

/// The concrete combinator returned by [`discard()`].
pub struct Discard<F, S, C> {
    /// The combinator to maybe apply.
    comb: C,
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f:   PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<'t, F, S, C: Expects<'t>> Expects<'t> for Discard<F, S, C> {
    type Formatter = C::Formatter;

    #[inline]
    fn expects(&self) -> Self::Formatter { self.comb.expects() }
}
impl<'t, F, S, C> Combinator<'t, F, S> for Discard<F, S, C>
where
    C: Combinator<'t, F, S>,
{
    type Output = ();
    type Error = C::Error;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S, Self::Error> {
        match self.comb.parse(input) {
            Result::Ok(rem, _) => Result::Ok(rem, ()),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// The concrete type returned by [`map()`].
pub struct Map<F, S, C, M> {
    /// The combinator who's output to map.
    comb: C,
    /// The mapping closure.
    func: M,
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f:   PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<'t, F, S, C: Expects<'t>, M> Expects<'t> for Map<F, S, C, M> {
    type Formatter = C::Formatter;

    #[inline]
    fn expects(&self) -> Self::Formatter { self.comb.expects() }
}
impl<'c, F, S, R1, R2, C, M> Combinator<'c, F, S> for Map<F, S, C, M>
where
    C: Combinator<'c, F, S, Output = R1>,
    M: FnMut(R1) -> R2,
{
    type Output = R2;
    type Error = C::Error;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S, Self::Error> {
        match self.comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, (self.func)(res)),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// The concrete type returned by [`map_err()`].
pub struct MapErr<F, S, C, M> {
    /// The combinator who's custom error to change.
    comb: C,
    /// The mapping closure.
    func: M,
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f:   PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<'t, F, S, C: Expects<'t>, M> Expects<'t> for MapErr<F, S, C, M> {
    type Formatter = C::Formatter;

    #[inline]
    fn expects(&self) -> Self::Formatter { self.comb.expects() }
}
impl<'c, F, S, E1, E2, C, M> Combinator<'c, F, S> for MapErr<F, S, C, M>
where
    C: Combinator<'c, F, S, Error = E1>,
    M: FnMut(E1) -> E2,
{
    type Output = C::Output;
    type Error = E2;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S, Self::Error> {
        match self.comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, res),
            Result::Fail(fail) => Result::Fail(fail.map_custom(&mut self.func)),
            Result::Error(err) => Result::Error(err.map_custom(&mut self.func)),
        }
    }
}

/// The combinator returned by [`nop()`].
pub struct Nop<F, S> {
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f: PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s: PhantomData<S>,
}
impl<F, S> Expects<'static> for Nop<F, S> {
    type Formatter = NopExpects;

    #[inline]
    fn expects(&self) -> Self::Formatter { NopExpects }
}
impl<F, S> Combinator<'static, F, S> for Nop<F, S> {
    type Output = ();
    type Error = Infallible;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'static, Self::Output, F, S, Self::Error> { Result::Ok(input, ()) }
}

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
    type Formatter = NotExpects<C::Formatter>;

    #[inline]
    fn expects(&self) -> Self::Formatter { NotExpects { fmt: self.comb.expects() } }
}
impl<'c, F, S, C> Combinator<'c, F, S> for Not<F, S, C>
where
    F: Clone,
    S: Clone + LenBytes,
    C: Combinator<'c, F, S>,
{
    type Output = ();
    type Error = C::Error;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S, Self::Error> {
        // Run the combinator
        match self.comb.parse(input.clone()) {
            Result::Ok(rem, _) => {
                // Get some initial span offset
                let offset: usize = match input.range() {
                    SpanRange::Closed(s, _) | SpanRange::ClosedOpen(s) => s,
                    SpanRange::OpenClosed(_) | SpanRange::Open | SpanRange::Empty => 0,
                };

                // Construct the span up to the remainder
                let span: Span<F, S> = match rem.range() {
                    SpanRange::Closed(s, _) | SpanRange::ClosedOpen(s) => {
                        Span::ranged(input.from_ref().clone(), input.source_ref().clone(), offset..s)
                    },
                    SpanRange::OpenClosed(_) | SpanRange::Open => Span::ranged(input.from_ref().clone(), input.source_ref().clone(), offset..offset),
                    SpanRange::Empty => input,
                };

                // Return the failure
                Result::Fail(Failure::Common(Common::Not { nested_fmt: Box::new(self.expects()), span }))
            },
            Result::Fail(_) => Result::Ok(input, ()),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// The concrete combinator returned by [`opt()`].
pub struct Opt<F, S, C> {
    /// The combinator to maybe apply.
    comb: C,
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f:   PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<'t, F, S, C: Expects<'t>> Expects<'t> for Opt<F, S, C> {
    type Formatter = OptExpects<C::Formatter>;

    #[inline]
    fn expects(&self) -> Self::Formatter { OptExpects { fmt: self.comb.expects() } }
}
impl<'t, F, S, C> Combinator<'t, F, S> for Opt<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'t, F, S>,
{
    type Output = Option<C::Output>;
    type Error = C::Error;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S, Self::Error> {
        match self.comb.parse(input.clone()) {
            Result::Ok(rem, res) => Result::Ok(rem, Some(res)),
            Result::Fail(Failure::NotEnough { needed, span }) => Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(_) => Result::Ok(input, None),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// The concrete combinator returned by [`recognize()`].
pub struct Recognize<F, S, C> {
    /// The combinator to maybe apply.
    comb: C,
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f:   PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<'t, F, S, C: Expects<'t>> Expects<'t> for Recognize<F, S, C> {
    type Formatter = C::Formatter;

    #[inline]
    fn expects(&self) -> Self::Formatter { self.comb.expects() }
}
impl<'t, F, S, C> Combinator<'t, F, S> for Recognize<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'t, F, S>,
{
    type Output = Span<F, S>;
    type Error = C::Error;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S, Self::Error> {
        // Get some initial span offset
        let offset: usize = match input.range() {
            SpanRange::Closed(s, _) | SpanRange::ClosedOpen(s) => s,
            SpanRange::OpenClosed(_) | SpanRange::Open | SpanRange::Empty => 0,
        };

        // Run the combinator
        match self.comb.parse(input.clone()) {
            Result::Ok(rem, _) => match rem.range() {
                SpanRange::Closed(s, _) | SpanRange::ClosedOpen(s) => {
                    Result::Ok(rem, Span::ranged(input.from_ref().clone(), input.source_ref().clone(), offset..s))
                },
                SpanRange::OpenClosed(_) | SpanRange::Open => {
                    Result::Ok(rem, Span::ranged(input.from_ref().clone(), input.source_ref().clone(), offset..offset))
                },
                SpanRange::Empty => Result::Ok(rem, input),
            },
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}
