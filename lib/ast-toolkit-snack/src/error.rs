//  ERROR.rs
//    by Lut99
//
//  Created:
//    07 Apr 2024, 17:58:35
//  Last edited:
//    08 May 2024, 11:53:38
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines problems raised by parsing with `snack`.
//!   
//!   There two types of problems defined by the crate:
//!   - Recoverable problems, called [`Failure`]s; and
//!   - Unrecoverable problems, called [`Error`]s.
//!   
//!   Note, however, that they share quite some overlap, because the [`commit()`]
//!   combinator allows promoting (almost) all recoverable [`Failure`]s into
//!   non-recoverable [`Error`]s. The common set that can do this is called
//!   [`Common`].
//

use std::convert::Infallible;
use std::error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning};
use enum_debug::EnumDebug;

use crate::bytes::complete::{OneOf1Expects as OneOf1BytesExpects, TagExpects as TagBytesExpects, While1Expects as While1BytesExpects};
use crate::combinator::NotExpects;
use crate::multi::Many1Expects;
use crate::sequence::DelimExpects;
use crate::utf8::complete::{
    Digit1Expects, OneOf1Expects as OneOf1Utf8Expects, TagExpects as TagUtf8Expects, While1Expects as While1Utf8Expects, Whitespace1Expects,
};
use crate::{Combinator, Expects, ExpectsFormatter, Result};


/***** HELPER MACROS *****/
/// Makes it easier to propagate enum variants.
macro_rules! propagate {
    (match $self:ident {
        $($source:ident::$variants:ident { $($fields:ident),* $(,)? } => $target:ident,)*
        !special {
            $($ssource:ident::$svariants:ident { $($sfields:ident),* $(,)? $(..)? } => { $($scode:tt)* },)*
        }
        $(,)?
    }) => {
        match $self {
            $($source::$variants { $($fields,)* } => $target::$variants { $($fields,)* },)*
            $($ssource::$svariants { $($sfields,)* .. } => { $($scode)* },)*
        }
    };
}





/***** ERRORS *****/
/// Defines an error that may occur when casting [`Commons`]s to [`Failure`]s or [`Error`]s.
#[derive(Debug)]
pub struct TryFromCommonError(String, &'static str);
impl Display for TryFromCommonError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Cannot convert Common::{} to {} because there is no equivalent in Error", self.0, self.1)
    }
}
impl error::Error for TryFromCommonError {}

/// Defines an error that may occur when casting [`Failure`]s to [`Error`]s.
#[derive(Debug)]
pub struct TryFromFailureError(String);
impl Display for TryFromFailureError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Cannot convert Failure::{} to an Error because there is no equivalent in Error", self.0)
    }
}
impl error::Error for TryFromFailureError {}





/***** LIBRARY COMBINATORS *****/
/// A combinator that always emits a custom error of the given type.
///
/// This combinator returns recoverable errors, i.e., [`branch::alt()`](crate::branch::alt) will still try other branches.
///
/// # Arguments
/// - `err`: The custom error to return.
///
/// # Returns
/// A combinator [`Fail`] that emits the given `err` as a [`Result::Fail`].
///
/// # Fails
/// This combinator _always_ fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{fail, Common, Failure};
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span = Span::new("<example>", "Hello, world!");
///
/// let mut comb = fail("Hello there!");
/// assert!(matches!(
///     comb.parse(span),
///     SResult::Fail(Failure::Common(Common::Custom("Hello there!")))
/// ));
/// ```
pub const fn fail<F, S, E>(err: E) -> Fail<F, S, E>
where
    E: Clone,
{
    Fail { err, _f: PhantomData, _s: PhantomData }
}

/// A combinator that always emits a custom error of the given type.
///
/// This combinator returns non-recoverable errors, i.e., [`branch::alt()`](crate::branch::alt) will _not_ try other branches.
///
/// # Arguments
/// - `err`: The custom error to return.
///
/// # Returns
/// A combinator [`Err`] that emits the given `err` as a [`Result::Error`].
///
/// # Fails
/// This combinator _always_ fails, but as an an [`Result::Error`].
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{err, Common, Error};
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span = Span::new("<example>", "Hello, world!");
///
/// let mut comb = err("Hello there!");
/// assert!(matches!(
///     comb.parse(span),
///     SResult::Error(Error::Common(Common::Custom("Hello there!")))
/// ));
/// ```
pub const fn err<F, S, E>(err: E) -> Err<F, S, E>
where
    E: Clone,
{
    Err { err, _f: PhantomData, _s: PhantomData }
}



/// A very powerful combinator that transforms a recoverable [`Failure`] into a non-recoverable [`Error`].
///
/// This can be used to provide more useful error messages, as it "commits" the parser to this particular branch. Consider the two examples below as illustration.
///
/// # Arguments
/// - `comb`: Some other [`Combinator`] who's failures to transform into errors.
///
/// # Returns
/// A combinator [`Cut`] that parses the same as `comb`, but returns [`Result::Error`]s instead of [`Result::Failure`]s.
///
/// # Fails
/// The returned combinator only fails if `comb` returned [`Failure::NotEnough`]. Otherwise, the failures are returned as [`Error`]s.
///
/// # Examples
/// The following example illustrates the default behaviour of branching:
/// ```rust
/// use ast_toolkit_snack::branch::alt;
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::sequence::pair;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// // Build a complex combinator
/// let mut comb = alt((
///     pair(tag("Hello, world"), tag("!")),
///     pair(tag("Goodbye, world"), tag("!")),
///     pair(tag("Cheerio, world"), tag("!")),
///     pair(tag("Fancy meeting you here, world"), tag("!")),
///     pair(tag("Top o' t' morning to ya, world"), tag("!")),
/// ));
///
/// // We forget the exclaimation mark... easy mistake
/// let span = Span::new("<example>", "Hello, world");
///
/// // Parsing now reports that it expected either one of the "X world!"s
/// assert!(matches!(comb.parse(span), SResult::Fail(Failure::Common(Common::Alt { .. }))));
/// ```
///
/// However, we can improve upon the error message by cutting:
/// ```rust
/// use ast_toolkit_snack::branch::alt;
/// use ast_toolkit_snack::sequence::pair;
/// use ast_toolkit_snack::error::{cut, Common, Error};
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// // Build a complex combinator
/// let mut comb = alt((
///     // We essentially match on something unique per branch, and use that to commit to that branch
///     pair(tag("H"), cut(pair(tag("ello, world"), tag("!")))),
///     pair(tag("G"), cut(pair(tag("oodbye, world"), tag("!")))),
///     pair(tag("C"), cut(pair(tag("heerio, world"), tag("!")))),
///     pair(tag("F"), cut(pair(tag("ancy meeting you here, world"), tag("!")))),
///     pair(tag("T"), cut(pair(tag("op o' t' morning to ya, world"), tag("!")))),
/// ));
///
/// // We forget the exclaimation mark again... how embarrassing!
/// let span = Span::new("<example>", "Hello, world");
///
/// // Parsing now reports a much more useful error
/// assert!(matches!(comb.parse(span), SResult::Error(Error::Common(Common::TagUtf8 { tag: "!", .. }))));
/// ```
#[inline]
pub const fn cut<'t, F, S, C>(comb: C) -> Cut<F, S, C>
where
    C: Combinator<'t, F, S>,
{
    Cut { comb, _f: PhantomData, _s: PhantomData }
}

/// Maps the custom error in a result of a combinator to something else, without actually mapping it.
///
/// When working with combinators, it is often the case that you need to merge the custom errors of combinators that will never throw it. Then you can use this combinator to essentially "transmute" the never-used error type to whatever you like.
///
/// # Arguments
/// - `comb`: Some [`Combinator`] to run.
///
/// # Returns
/// A combinator [`Transmute`] that runs the given `comb`inator, translating the errors behind the screen.
///
/// Note that, by design, the returned combinator panics if `comb` _does_ throw a [`Common::Custom`] somehow.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{fail, transmute, Common, Error, Failure};
/// use ast_toolkit_snack::sequence::pair;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
/// use ast_toolkit_snack::span::MatchBytes;
///
/// struct HelloWorldError;
///
/// // Pretend this is has `Expects` and `Combinator` impls
/// fn hello_world<F: Clone, S: Clone + MatchBytes>(input: Span<F, S>) -> SResult<'static, (), F, S, &'static str> {
///     match tag("Hello, world").parse(input) {
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
/// #         match tag("Hello, world").parse(input) {
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
/// let mut comb = pair(hello_world, transmute(tag("!")));
///
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(13..), ((), span1.slice(12..13))));
/// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::Custom("that's not hello world!")))));
/// ```
#[inline]
pub const fn transmute<'t, F, S, C, E2>(comb: C) -> Transmute<F, S, C, E2>
where
    C: Combinator<'t, F, S>,
{
    Transmute { comb, _f: PhantomData, _s: PhantomData, _e: PhantomData }
}





/***** FORMATTERS *****/
/// Formats that nothing is expected whatsoever.
#[derive(Debug)]
pub struct NothingExpects;
impl Display for NothingExpects {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for NothingExpects {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "nothing") }
}





/***** LIBRARY COMBINATORS *****/
/// Combinator returned by [`fail()`].
pub struct Fail<F, S, E> {
    /// The error to return as failure.
    err: E,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:  PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:  PhantomData<S>,
}
impl<'e, F, S, E: 'e> Expects<'e> for Fail<F, S, E> {
    type Formatter = NothingExpects;

    #[inline]
    fn expects(&self) -> Self::Formatter { NothingExpects }
}
impl<'e, F, S, E: 'e + Clone> Combinator<'e, F, S> for Fail<F, S, E> {
    type Output = ();
    type Error = E;

    #[inline]
    fn parse(&mut self, _input: Span<F, S>) -> Result<'e, Self::Output, F, S, Self::Error> {
        Result::Fail(Failure::Common(Common::Custom(self.err.clone())))
    }
}

/// Combinator returned by [`err()`].
pub struct Err<F, S, E> {
    /// The error to return as error.
    err: E,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:  PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:  PhantomData<S>,
}
impl<'e, F, S, E: 'e> Expects<'e> for Err<F, S, E> {
    type Formatter = NothingExpects;

    #[inline]
    fn expects(&self) -> Self::Formatter { NothingExpects }
}
impl<'e, F, S, E: 'e + Clone> Combinator<'e, F, S> for Err<F, S, E> {
    type Output = ();
    type Error = E;

    #[inline]
    fn parse(&mut self, _input: Span<F, S>) -> Result<'e, Self::Output, F, S, Self::Error> {
        Result::Error(Error::Common(Common::Custom(self.err.clone())))
    }
}



/// Combinator returned by [`cut()`].
pub struct Cut<F, S, C> {
    /// The combinator we're cutting.
    comb: C,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:   PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:   PhantomData<S>,
}
impl<'t, F, S, C: Expects<'t>> Expects<'t> for Cut<F, S, C> {
    type Formatter = C::Formatter;

    #[inline]
    fn expects(&self) -> Self::Formatter { self.comb.expects() }
}
impl<'t, F, S, C: Combinator<'t, F, S>> Combinator<'t, F, S> for Cut<F, S, C> {
    type Output = C::Output;
    type Error = C::Error;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S, Self::Error> {
        match self.comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, res),
            Result::Fail(Failure::NotEnough { needed, span }) => Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(fail) => Result::Error(fail.try_into().unwrap()),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// The concrete type returned by [`transmute()`].
pub struct Transmute<F, S, C, E2> {
    /// The combinator who's custom error to change.
    comb: C,
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f:   PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
    /// The type of the custom error to magically cast to.
    _e:   PhantomData<E2>,
}
impl<'t, F, S, C: Expects<'t>, E2> Expects<'t> for Transmute<F, S, C, E2> {
    type Formatter = C::Formatter;

    #[inline]
    fn expects(&self) -> Self::Formatter { self.comb.expects() }
}
impl<'c, F, S, C, E2> Combinator<'c, F, S> for Transmute<F, S, C, E2>
where
    C: Combinator<'c, F, S>,
{
    type Output = C::Output;
    type Error = E2;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S, Self::Error> {
        match self.comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, res),
            Result::Fail(fail) => Result::Fail(fail.transmute()),
            Result::Error(err) => Result::Error(err.transmute()),
        }
    }
}





/***** LIBRARY *****/
/// Defines a common set of problems raised by snack combinators.
///
/// These are usually emitted as recoverable [`Failure`]s, but can be turned
/// into unrecoverable [`Error`]s by usage of the [`commit()`]-combinator. Both
/// enums also define a small set that cannot do this change (i.e., that cannot
/// be made unrecoverable or that isn't recoverable in the first place).
#[derive(Debug, EnumDebug)]
pub enum Common<'a, F, S, E = Infallible> {
    /// Not all input was parsed where we expected it.
    All { span: Span<F, S> },
    /// All possible branches in an [`alt()`](crate::branch::alt())-combinator have failed.
    ///
    /// Note that, if there is only one possible branch, Alt acts more like a pass-through in terms of expecting.
    Alt { branches: Vec<Self>, fmt: Box<dyn 'a + ExpectsFormatter>, span: Span<F, S> },
    /// Some non-library combinator failed.
    Custom(E),
    /// Failed to match the delimited part of a [`delim()`](crate::sequence::delim()).
    Delim {
        fail:      Box<Self>,
        open_fmt:  Box<dyn 'a + ExpectsFormatter>,
        comb_fmt:  Box<dyn 'a + ExpectsFormatter>,
        close_fmt: Box<dyn 'a + ExpectsFormatter>,
    },
    /// Failed to match the closing delimiter of a [`delim()`](crate::sequence::delim()).
    DelimClose { fail: Box<Self>, close_fmt: Box<dyn 'a + ExpectsFormatter> },
    /// Failed to match the opening delimiter of a [`delim()`](crate::sequence::delim()).
    DelimOpen { fail: Box<Self>, open_fmt: Box<dyn 'a + ExpectsFormatter> },
    /// Failed to match at least one digit.
    Digit1 { span: Span<F, S> },
    /// Failed to match a combinator at least once.
    Many1 { fail: Box<Self>, nested_fmt: Box<dyn 'a + ExpectsFormatter> },
    /// Failed to match a combinator exactly N times.
    ManyN { n: usize, i: usize, fail: Box<Self>, nested_fmt: Box<dyn 'a + ExpectsFormatter> },
    /// Failed to _not_ apply a combinator.
    Not { nested_fmt: Box<dyn 'a + ExpectsFormatter>, span: Span<F, S> },
    /// Expected at least one of the following bytes.
    OneOf1Bytes { byteset: &'a [u8], span: Span<F, S> },
    /// Expected at least one of the following characters.
    OneOf1Utf8 { charset: &'a [&'a str], span: Span<F, S> },
    /// Failed to parse at least one value in a punctuated list of sorts.
    PunctuatedList1 { value_fail: Box<Self>, value_fmt: Box<dyn 'a + ExpectsFormatter>, punct_fmt: Box<dyn 'a + ExpectsFormatter> },
    /// Failed to match a combinator exactly N times, separated by some other thing, where the punctuation was what we failed to parse.
    PunctuatedListNPunct {
        n: usize,
        i: usize,
        punct_fail: Box<Self>,
        value_fmt: Box<dyn 'a + ExpectsFormatter>,
        punct_fmt: Box<dyn 'a + ExpectsFormatter>,
    },
    /// Failed to match a combinator exactly N times, separated by some other thing, where the value was what we failed to parse.
    PunctuatedListNValue {
        n: usize,
        i: usize,
        value_fail: Box<Self>,
        value_fmt: Box<dyn 'a + ExpectsFormatter>,
        punct_fmt: Box<dyn 'a + ExpectsFormatter>,
    },
    /// Failed to match something particular with byte version of the [`tag()`](crate::bytes::complete::tag())-combinator.
    TagBytes { tag: &'a [u8], span: Span<F, S> },
    /// Failed to match something particular with UTF-8 version of the [`tag()`](crate::utf8::complete::tag())-combinator.
    TagUtf8 { tag: &'a str, span: Span<F, S> },
    /// Failed to match something matching a predicate with the byte-version of [`while1()`](crate::bytes::complete::while1())-combinator.
    While1Bytes { span: Span<F, S> },
    /// Failed to match something matching a predicate with the UTF-8-version of [`while1()`](crate::utf8::complete::while1())-combinator.
    While1Utf8 { span: Span<F, S> },
    /// Failed to match at least one whitespace.
    Whitespace1 { span: Span<F, S> },
}
impl<'a, F, S, E> Common<'a, F, S, E> {
    /// "Transmutes" this Common from one custom error type to another.
    ///
    /// This is only possible if this is _not_ [`Common::Custom`], because the point is that we don't really care about the type. If you _do_ care about it, see [`Common::map_custom()`] instead.
    ///
    /// # Generics
    /// - `E2`: The new custom error type to transmute to.
    ///
    /// # Returns
    /// A new Common that is the same variant, but with another custom error type.
    ///
    /// # Panics
    /// This function panics if we're [`Common::Custom`].
    #[inline]
    #[track_caller]
    pub fn transmute<E2>(self) -> Common<'a, F, S, E2> {
        propagate! {
            match self {
                Self::All { span } => Common,
                Self::Digit1 { span } => Common,
                Self::Not { nested_fmt, span } => Common,
                Self::OneOf1Bytes { byteset, span } => Common,
                Self::OneOf1Utf8 { charset, span } => Common,
                Self::TagBytes { tag, span } => Common,
                Self::TagUtf8 { tag, span } => Common,
                Self::While1Bytes { span } => Common,
                Self::While1Utf8 { span } => Common,
                Self::Whitespace1 { span } => Common,

                !special {
                    Self::Alt { branches, fmt, span } => {
                        Common::Alt {
                            branches: branches.into_iter().map(|c| c.transmute()).collect(),
                            fmt,
                            span,
                        }
                    },
                    Self::Custom { .. } => { panic!("Cannot transmute Common::Custom"); },
                    Self::Delim { fail, open_fmt, comb_fmt, close_fmt } => { Common::Delim { fail: Box::new((*fail).transmute()), open_fmt, comb_fmt, close_fmt } },
                    Self::DelimClose { fail, close_fmt } => { Common::DelimClose { fail: Box::new((*fail).transmute()), close_fmt } },
                    Self::DelimOpen { fail, open_fmt } => { Common::DelimOpen { fail: Box::new((*fail).transmute()), open_fmt } },
                    Self::Many1 { fail, nested_fmt } => { Common::Many1 { fail: Box::new((*fail).transmute()), nested_fmt } },
                    Self::ManyN { n, i, fail, nested_fmt } => { Common::ManyN { n, i, fail: Box::new((*fail).transmute()), nested_fmt } },
                    Self::PunctuatedList1 { value_fail, value_fmt, punct_fmt } => { Common::PunctuatedList1 { value_fail: Box::new((*value_fail).transmute()), value_fmt, punct_fmt } },
                    Self::PunctuatedListNPunct { n, i, punct_fail, value_fmt, punct_fmt } => { Common::PunctuatedListNPunct { n, i, punct_fail: Box::new((*punct_fail).transmute()), value_fmt, punct_fmt } },
                    Self::PunctuatedListNValue { n, i, value_fail, value_fmt, punct_fmt } => { Common::PunctuatedListNValue { n, i, value_fail: Box::new((*value_fail).transmute()), value_fmt, punct_fmt } },
                },
            }
        }
    }

    /// Maps the custom type of this Common.
    ///
    /// This applies the given closure if we are [`Common::Custom`], but else passes values as-is.
    ///
    /// You can use [`Common::transmute()`] if you're certain that [`Common::Custom`] never occurs.
    ///
    /// # Generics
    /// - `E2`: The new custom error type to transmute to.
    ///
    /// # Arguments
    /// - `map`: The mapping closure that translates from one custom error type to another.
    ///
    /// # Returns
    /// A new Common that is the same variant, but with another custom error type.
    #[inline]
    #[track_caller]
    pub fn map_custom<E2>(self, map: &mut impl FnMut(E) -> E2) -> Common<'a, F, S, E2> {
        match self {
            Self::All { span } => Common::All { span },
            Self::Alt { branches, fmt, span } => Common::Alt { branches: branches.into_iter().map(|c| c.map_custom(map)).collect(), fmt, span },
            Self::Custom(err) => Common::Custom(map(err)),
            Self::Delim { fail, open_fmt, comb_fmt, close_fmt } => {
                Common::Delim { fail: Box::new((*fail).map_custom(map)), open_fmt, comb_fmt, close_fmt }
            },
            Self::DelimClose { fail, close_fmt } => Common::DelimClose { fail: Box::new((*fail).map_custom(map)), close_fmt },
            Self::DelimOpen { fail, open_fmt } => Common::DelimOpen { fail: Box::new((*fail).map_custom(map)), open_fmt },
            Self::Digit1 { span } => Common::Digit1 { span },
            Self::Many1 { fail, nested_fmt } => Common::Many1 { fail: Box::new((*fail).map_custom(map)), nested_fmt },
            Self::ManyN { n, i, fail, nested_fmt } => Common::ManyN { n, i, fail: Box::new((*fail).map_custom(map)), nested_fmt },
            Self::Not { nested_fmt, span } => Common::Not { nested_fmt, span },
            Self::OneOf1Bytes { byteset, span } => Common::OneOf1Bytes { byteset, span },
            Self::OneOf1Utf8 { charset, span } => Common::OneOf1Utf8 { charset, span },
            Self::PunctuatedList1 { value_fail, value_fmt, punct_fmt } => {
                Common::PunctuatedList1 { value_fail: Box::new((*value_fail).map_custom(map)), value_fmt, punct_fmt }
            },
            Self::PunctuatedListNPunct { n, i, punct_fail, value_fmt, punct_fmt } => {
                Common::PunctuatedListNPunct { n, i, punct_fail: Box::new((*punct_fail).map_custom(map)), value_fmt, punct_fmt }
            },
            Self::PunctuatedListNValue { n, i, value_fail, value_fmt, punct_fmt } => {
                Common::PunctuatedListNValue { n, i, value_fail: Box::new((*value_fail).map_custom(map)), value_fmt, punct_fmt }
            },
            Self::TagBytes { tag, span } => Common::TagBytes { tag, span },
            Self::TagUtf8 { tag, span } => Common::TagUtf8 { tag, span },
            Self::While1Bytes { span } => Common::While1Bytes { span },
            Self::While1Utf8 { span } => Common::While1Utf8 { span },
            Self::Whitespace1 { span } => Common::Whitespace1 { span },
        }
    }
}

impl<'a, F, S, E: Display> Display for Common<'a, F, S, E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::All { .. } => write!(f, "Unexpected additional input"),
            Self::Alt { fmt, .. } => write!(f, "{fmt}"),
            Self::Custom(err) => write!(f, "{err}"),
            Self::DelimClose { close_fmt, .. } => {
                write!(f, "Expected closing ")?;
                close_fmt.expects_fmt(f, 0)
            },
            Self::Delim { open_fmt, comb_fmt, close_fmt, .. } => write!(f, "{}", DelimExpects { open_fmt, comb_fmt, close_fmt }),
            Self::DelimOpen { open_fmt, .. } => {
                write!(f, "Expected opening ")?;
                open_fmt.expects_fmt(f, 0)
            },
            Self::Digit1 { .. } => write!(f, "{}", Digit1Expects),
            Self::Many1 { nested_fmt, .. } => write!(f, "{}", Many1Expects { fmt: nested_fmt }),
            Self::ManyN { i, n, nested_fmt, .. } => {
                write!(f, "Expected at least {} more repetitions of ", *n - *i)?;
                nested_fmt.expects_fmt(f, 0)
            },
            Self::Not { nested_fmt, .. } => write!(f, "{}", NotExpects { fmt: nested_fmt }),
            Self::OneOf1Bytes { byteset, .. } => write!(f, "{}", OneOf1BytesExpects { byteset }),
            Self::OneOf1Utf8 { charset, .. } => write!(f, "{}", OneOf1Utf8Expects { charset }),
            Self::PunctuatedList1 { value_fmt, punct_fmt, .. } => {
                write!(f, "Expected ")?;
                value_fmt.expects_fmt(f, 0)?;
                write!(f, " as first entry in ")?;
                punct_fmt.expects_fmt(f, 0)?;
                write!(f, "-separated list")
            },
            Self::PunctuatedListNPunct { n, value_fmt, punct_fmt, .. } => {
                write!(f, "Expected ")?;
                value_fmt.expects_fmt(f, 0)?;
                write!(f, " to separate elements in ")?;
                punct_fmt.expects_fmt(f, 0)?;
                write!(f, "-list of {n} elements")
            },
            Self::PunctuatedListNValue { n, i, value_fmt, punct_fmt, .. } => {
                write!(f, "Expected ")?;
                value_fmt.expects_fmt(f, 0)?;
                write!(f, " as entry {} in ", i + 1)?;
                punct_fmt.expects_fmt(f, 0)?;
                write!(f, "-separated list of {n} elements")
            },
            Self::TagBytes { tag, .. } => write!(f, "{}", TagBytesExpects { tag }),
            Self::TagUtf8 { tag, .. } => write!(f, "{}", TagUtf8Expects { tag }),
            Self::While1Bytes { .. } => write!(f, "{}", While1BytesExpects),
            Self::While1Utf8 { .. } => write!(f, "{}", While1Utf8Expects),
            Self::Whitespace1 { .. } => write!(f, "{}", Whitespace1Expects),
        }
    }
}
impl<'a, F: Clone, S: Clone, E: Spanning<F, S>> Spanning<F, S> for Common<'a, F, S, E> {
    fn span(&self) -> Span<F, S> {
        match self {
            Self::All { span } => span.clone(),
            Self::Alt { span, .. } => span.clone(),
            Self::Custom(err) => err.span(),
            Self::DelimClose { fail, .. } => fail.span(),
            Self::Delim { fail, .. } => fail.span(),
            Self::DelimOpen { fail, .. } => fail.span(),
            Self::Digit1 { span } => span.clone(),
            Self::Many1 { fail, .. } => fail.span(),
            Self::ManyN { fail, .. } => fail.span(),
            Self::Not { span, .. } => span.clone(),
            Self::OneOf1Bytes { span, .. } => span.clone(),
            Self::OneOf1Utf8 { span, .. } => span.clone(),
            Self::PunctuatedList1 { value_fail, .. } => value_fail.span(),
            Self::PunctuatedListNPunct { punct_fail, .. } => punct_fail.span(),
            Self::PunctuatedListNValue { value_fail, .. } => value_fail.span(),
            Self::TagBytes { span, .. } => span.clone(),
            Self::TagUtf8 { span, .. } => span.clone(),
            Self::While1Bytes { span } => span.clone(),
            Self::While1Utf8 { span } => span.clone(),
            Self::Whitespace1 { span } => span.clone(),
        }
    }
}



/// Defines a problems emitted by snack combinators that are recoverable.
#[derive(Debug, EnumDebug)]
pub enum Failure<'a, F, S, E = Infallible> {
    /// There wasn't enough input data and the combinator was a streaming combinator.
    ///
    /// The `needed` is an optional hint provided by the combinator to guess how many more bytes would be needed.
    NotEnough { needed: Option<usize>, span: Span<F, S> },

    /// It's a type of failure that can be made a non-recoverable [`Error`].
    Common(Common<'a, F, S, E>),
}
impl<'a, F, S, E> Failure<'a, F, S, E> {
    /// "Transmutes" this Failure from one custom error type to another.
    ///
    /// This is only possible if this is _not_ [`Failure::Common(Common::Custom)`](Custom::Common), because the point is that we don't really care about the type. If you _do_ care about it, see [`Failure::map_custom()`] instead.
    ///
    /// # Generics
    /// - `E2`: The new custom error type to transmute to.
    ///
    /// # Returns
    /// A new Failure that is the same variant, but with another custom error type.
    ///
    /// # Panics
    /// This function panics if we're [`Failure::Common(Common::Custom)`](Custom::Common).
    #[inline]
    #[track_caller]
    pub fn transmute<E2>(self) -> Failure<'a, F, S, E2> {
        match self {
            Self::NotEnough { needed, span } => Failure::NotEnough { needed, span },
            Self::Common(c) => Failure::Common(c.transmute()),
        }
    }

    /// Maps the custom type of this Failure.
    ///
    /// This applies the given closure if we are [`Failure::Common(Common::Custom)`](Common::Custom), but else passes values as-is.
    ///
    /// You can use [`Failure::transmute()`] if you're certain that [`Common::Custom`] never occurs.
    ///
    /// # Generics
    /// - `E2`: The new custom error type to transmute to.
    ///
    /// # Arguments
    /// - `map`: The mapping closure that translates from one custom error type to another.
    ///
    /// # Returns
    /// A new Failure that is the same variant, but with another custom error type.
    #[inline]
    #[track_caller]
    pub fn map_custom<E2>(self, mut map: impl FnMut(E) -> E2) -> Failure<'a, F, S, E2> {
        match self {
            Self::NotEnough { needed, span } => Failure::NotEnough { needed, span },
            Self::Common(c) => Failure::Common(c.map_custom(&mut map)),
        }
    }
}

impl<'a, F, S, E: Display> Display for Failure<'a, F, S, E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult {
        match self {
            Self::NotEnough { needed, .. } => {
                if let Some(needed) = needed {
                    write!(f, "{needed} more input bytes")
                } else {
                    write!(f, "more input")
                }
            },

            Self::Common(p) => <Common<'a, F, S, E> as Display>::fmt(p, f),
        }
    }
}
impl<'a, F: Debug + Display, S: Debug + Display, E: error::Error> error::Error for Failure<'a, F, S, E> {}
impl<'a, F: Clone, S: Clone, E: Spanning<F, S>> Spanning<F, S> for Failure<'a, F, S, E> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::NotEnough { span, .. } => span.clone(),

            Self::Common(p) => p.span(),
        }
    }
}

impl<'a, F, S, E> From<Common<'a, F, S, E>> for Failure<'a, F, S, E> {
    #[inline]
    fn from(value: Common<'a, F, S, E>) -> Self { Self::Common(value) }
}
impl<'a, F, S, E> TryFrom<Failure<'a, F, S, E>> for Common<'a, F, S, E> {
    type Error = TryFromCommonError;

    #[inline]
    fn try_from(value: Failure<'a, F, S, E>) -> std::result::Result<Self, Self::Error> {
        match value {
            Failure::Common(c) => Ok(c),
            Failure::NotEnough { .. } => Err(TryFromCommonError(value.variant().to_string(), "a Failure")),
        }
    }
}



/// Defines a problems emitted by snack combinators that are non-recoverable.
#[derive(Debug, EnumDebug)]
pub enum Error<'a, F, S, E = Infallible> {
    /// There is a specific context in which the parsing failed.
    ///
    /// This is usually used through the [`context()`]-combinator, and allows
    /// one to hint to the user that something larger was being parsed (e.g., expressions).
    Context { context: &'static str, span: Span<F, S> },

    /// It's a type of error that can come from recoverable [`Failure`]s.
    Common(Common<'a, F, S, E>),
}
impl<'a, F, S, E> Error<'a, F, S, E> {
    /// "Transmutes" this Error from one custom error type to another.
    ///
    /// This is only possible if this is _not_ [`Error::Common(Common::Custom)`](Custom::Common), because the point is that we don't really care about the type. If you _do_ care about it, see [`Error::map_custom()`] instead.
    ///
    /// # Generics
    /// - `E2`: The new custom error type to transmute to.
    ///
    /// # Returns
    /// A new Error that is the same variant, but with another custom error type.
    ///
    /// # Panics
    /// This function panics if we're [`Error::Common(Common::Custom)`](Custom::Common).
    #[inline]
    #[track_caller]
    pub fn transmute<E2>(self) -> Error<'a, F, S, E2> {
        match self {
            Self::Context { context, span } => Error::Context { context, span },
            Self::Common(c) => Error::Common(c.transmute()),
        }
    }

    /// Maps the custom type of this Error.
    ///
    /// This applies the given closure if we are [`Error::Common(Common::Custom)`](Common::Custom), but else passes values as-is.
    ///
    /// You can use [`Error::transmute()`] if you're certain that [`Common::Custom`] never occurs.
    ///
    /// # Generics
    /// - `E2`: The new custom error type to transmute to.
    ///
    /// # Arguments
    /// - `map`: The mapping closure that translates from one custom error type to another.
    ///
    /// # Returns
    /// A new Error that is the same variant, but with another custom error type.
    #[inline]
    #[track_caller]
    pub fn map_custom<E2>(self, mut map: impl FnMut(E) -> E2) -> Error<'a, F, S, E2> {
        match self {
            Self::Context { context, span } => Error::Context { context, span },
            Self::Common(c) => Error::Common(c.map_custom(&mut map)),
        }
    }
}

impl<'a, F, S, E: Display> Display for Error<'a, F, S, E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult {
        match self {
            Self::Context { context, .. } => {
                write!(f, "{context}")
            },

            Self::Common(p) => <Common<'a, F, S, E> as Display>::fmt(p, f),
        }
    }
}
impl<'a, F: Debug + Display, S: Debug + Display, E: error::Error> error::Error for Error<'a, F, S, E> {}
impl<'a, F: Clone, S: Clone, E: Spanning<F, S>> Spanning<F, S> for Error<'a, F, S, E> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::Context { span, .. } => span.clone(),

            Self::Common(p) => p.span(),
        }
    }
}

impl<'a, F, S, E> From<Common<'a, F, S, E>> for Error<'a, F, S, E> {
    #[inline]
    fn from(value: Common<'a, F, S, E>) -> Self { Self::Common(value) }
}
impl<'a, F, S, E> TryFrom<Error<'a, F, S, E>> for Common<'a, F, S, E> {
    type Error = TryFromCommonError;

    #[inline]
    fn try_from(value: Error<'a, F, S, E>) -> std::result::Result<Self, Self::Error> {
        match value {
            Error::Common(c) => Ok(c),
            Error::Context { .. } => Err(TryFromCommonError(value.variant().to_string(), "an Error")),
        }
    }
}
impl<'a, F, S, E> TryFrom<Failure<'a, F, S, E>> for Error<'a, F, S, E> {
    type Error = TryFromFailureError;

    #[inline]
    fn try_from(value: Failure<'a, F, S, E>) -> std::result::Result<Self, Self::Error> {
        match value {
            Failure::Common(p) => Ok(Self::Common(p)),
            other => Err(TryFromFailureError(other.variant().to_string())),
        }
    }
}
