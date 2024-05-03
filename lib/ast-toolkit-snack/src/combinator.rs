//  COMB.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 18:01:57
//  Last edited:
//    03 May 2024, 16:37:59
//  Auto updated?
//    Yes
//
//  Description:
//!   Some miscellaneous combinators that operate on other combinators.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning};

use crate::error::{Common, Failure};
use crate::{Combinator, Expects, ExpectsFormatter, Result};


/***** LIBRARY FUNCTIONS *****/
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
pub const fn not<'c, F, S, C>(comb: C) -> Not<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'c, F, S>,
    C::Output: Spanning<F, S>,
{
    Not { comb, _f: PhantomData, _s: PhantomData }
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
/// A combinator [`Map`] that runs the given `comb`inator, and then maps the result using `func`.
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
///         SResult::Fail(_) => SResult::Fail(Failure::Common(Common::Custom { err: "that's not hello world!" })),
///         SResult::Error(_) => SResult::Error(Error::Common(Common::Custom { err: "that's not hello world!" })),
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
/// #             SResult::Fail(_) => SResult::Fail(Failure::Common(Common::Custom { err: "that's not hello world!" })),
/// #             SResult::Error(_) => SResult::Error(Error::Common(Common::Custom { err: "that's not hello world!" })),
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
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::Custom { err: HelloWorldError }))));
/// ```
#[inline]
pub const fn map_err<'c, F, S, R, E1, E2, C, M>(comb: C, func: M) -> MapErr<F, S, C, M>
where
    C: Combinator<'c, F, S, Output = R, Error = E1>,
    M: FnMut(E1) -> E2,
{
    MapErr { comb, func, _f: PhantomData, _s: PhantomData }
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

/// ExpectsFormatter for the [`Map`] combinator.
#[derive(Debug)]
pub struct MapExpects<E> {
    /// The thing we expect.
    pub(crate) fmt: E,
}
impl<E: ExpectsFormatter> Display for MapExpects<E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<E: ExpectsFormatter> ExpectsFormatter for MapExpects<E> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult { self.fmt.expects_fmt(f, indent) }
}





/***** LIBRARY *****/
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
    S: Clone,
    C: Combinator<'c, F, S>,
    C::Output: Spanning<F, S>,
{
    type Output = ();
    type Error = C::Error;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S, Self::Error> {
        match self.comb.parse(input.clone()) {
            Result::Ok(_, res) => Result::Fail(Failure::Common(Common::Not { nested_fmt: Box::new(self.expects()), span: res.span() })),
            Result::Fail(_) => Result::Ok(input, ()),
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
    type Formatter = MapExpects<C::Formatter>;

    #[inline]
    fn expects(&self) -> Self::Formatter { MapExpects { fmt: self.comb.expects() } }
}
impl<'c, F, S, R1, R2, E, C, M> Combinator<'c, F, S> for Map<F, S, C, M>
where
    C: Combinator<'c, F, S, Output = R1, Error = E>,
    M: FnMut(R1) -> R2,
{
    type Output = R2;
    type Error = E;

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
    type Formatter = MapExpects<C::Formatter>;

    #[inline]
    fn expects(&self) -> Self::Formatter { MapExpects { fmt: self.comb.expects() } }
}
impl<'c, F, S, R, E1, E2, C, M> Combinator<'c, F, S> for MapErr<F, S, C, M>
where
    C: Combinator<'c, F, S, Output = R, Error = E1>,
    M: FnMut(E1) -> E2,
{
    type Output = R;
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
