//  MULTI.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:46:57
//  Last edited:
//    30 Apr 2024, 16:37:38
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators that repeat other combinators multiple times. A
//!   _variable_ number of times.
//

// Imports
use std::fmt::{Formatter, Result as FResult};
use std::marker::PhantomData;

#[cfg(feature = "punctuated")]
use ast_toolkit_punctuated::{Punctuated, PunctuatedTrailing};
use ast_toolkit_span::Span;

use crate::error::{Common, Failure};
use crate::{Combinator, Expects, ExpectsExt as _, ExpectsFormatter, Result};


/***** EXPECTS FUNCTIONS *****/
/// Defines what we expect from a [`Many1`].
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `expects`: Some nested [`ExpectsFormatter`] of the combinator that is being applied `n` times.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[inline]
pub(crate) fn expects_many1(f: &mut Formatter, indent: usize, expects: ExpectsFormatter) -> FResult {
    write!(f, "at least one time ")?;
    <ExpectsFormatter as Expects>::fmt(&expects, f, indent)
}

/// Defines what we expect from a [`ManyN`].
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `n`: The number of times to apply the combinator.
/// - `expects`: Some nested [`ExpectsFormatter`] of the combinator that is being applied `n` times.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[inline]
pub(crate) fn expects_many_n(f: &mut Formatter, indent: usize, n: usize, expects: ExpectsFormatter) -> FResult {
    write!(f, "{n} repetitions of ")?;
    <ExpectsFormatter as Expects>::fmt(&expects, f, indent)
}



/// Defines what we expect from a [`Punctuated1`].
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `values`: Some nested [`ExpectsFormatter`] of the values combinator that is being applied `n` times.
/// - `puncts`: Some nested [`ExpectsFormatter`] of the punctuation combinator.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[cfg(feature = "punctuated")]
#[inline]
pub(crate) fn expects_punctuated1(f: &mut Formatter, indent: usize, values: ExpectsFormatter, puncts: ExpectsFormatter) -> FResult {
    write!(f, "a list of at least one ")?;
    values.fmt(f, indent)?;
    write!(f, " separated by ")?;
    puncts.fmt(f, indent)
}



/// Defines what we expect from a [`PunctuatedTrailing1`].
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `values`: Some nested [`ExpectsFormatter`] of the values combinator that is being applied `n` times.
/// - `puncts`: Some nested [`ExpectsFormatter`] of the punctuation combinator.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[cfg(feature = "punctuated")]
#[inline]
pub(crate) fn expects_punctuated_trailing1(f: &mut Formatter, indent: usize, values: ExpectsFormatter, puncts: ExpectsFormatter) -> FResult {
    write!(f, "a list of at least one time ")?;
    values.fmt(f, indent)?;
    write!(f, " separated by ")?;
    puncts.fmt(f, indent)?;
    write!(f, ", with optional trailing punctuation")
}

/// Defines what we expect from a [`PunctuatedTrailing1`] when it fails.
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `fail`: Some nested [`ExpectsFormatter`] of the value combinator that actually failed.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[cfg(feature = "punctuated")]
#[inline]
pub(crate) fn expects_punctuated_trailing1_fail(f: &mut Formatter, indent: usize, fail: ExpectsFormatter) -> FResult {
    write!(f, "at least one time ")?;
    fail.fmt(f, indent)
}

/// Defines what we expect from a [`PunctuatedTrailingN`].
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `n`: The number of times to apply the combinator.
/// - `values`: Some nested [`ExpectsFormatter`] of the values combinator that is being applied `n` times.
/// - `puncts`: Some nested [`ExpectsFormatter`] of the punctuation combinator.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[cfg(feature = "punctuated")]
#[inline]
pub(crate) fn expects_punctuated_trailing_n(
    f: &mut Formatter,
    indent: usize,
    n: usize,
    values: ExpectsFormatter,
    puncts: ExpectsFormatter,
) -> FResult {
    write!(f, "a list of {n} times ")?;
    values.fmt(f, indent)?;
    write!(f, " separated by ")?;
    puncts.fmt(f, indent)?;
    write!(f, ", with optional trailing punctuation")
}

/// Defines what we expect from a [`PunctuatedTrailingN`] when it fails to parse a value.
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `n`: The number of times to apply the combinator.
/// - `values`: Some nested [`ExpectsFormatter`] of the values combinator that is being applied `n` times.
/// - `puncts`: Some nested [`ExpectsFormatter`] of the punctuation combinator.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[cfg(feature = "punctuated")]
#[inline]
pub(crate) fn expects_punctuated_trailing_n_value(
    f: &mut Formatter,
    indent: usize,
    n: usize,
    values: ExpectsFormatter,
    puncts: ExpectsFormatter,
) -> FResult {
    write!(f, "the next value in a list of {n} times ")?;
    values.fmt(f, indent)?;
    write!(f, " separated by ")?;
    puncts.fmt(f, indent)?;
    write!(f, ", with optional trailing punctuation")
}

/// Defines what we expect from a [`PunctuatedTrailingN`] when it fails to parse a punctuation.
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `n`: The number of times to apply the combinator.
/// - `values`: Some nested [`ExpectsFormatter`] of the values combinator that is being applied `n` times.
/// - `puncts`: Some nested [`ExpectsFormatter`] of the punctuation combinator.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[cfg(feature = "punctuated")]
#[inline]
pub(crate) fn expects_punctuated_trailing_n_punct(
    f: &mut Formatter,
    indent: usize,
    n: usize,
    values: ExpectsFormatter,
    puncts: ExpectsFormatter,
) -> FResult {
    write!(f, "the next punctuation in a list of {n} times ")?;
    values.fmt(f, indent)?;
    write!(f, " separated by ")?;
    puncts.fmt(f, indent)?;
    write!(f, ", with optional trailing punctuation")
}



/// Defines what we expect from a [`SeparatedList1`].
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `values`: Some nested [`ExpectsFormatter`] of the values combinator that is being applied `n` times.
/// - `puncts`: Some nested [`ExpectsFormatter`] of the punctuation combinator.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[inline]
pub(crate) fn expects_separated_list1(f: &mut Formatter, indent: usize, values: ExpectsFormatter, puncts: ExpectsFormatter) -> FResult {
    write!(f, "a list of at least one time ")?;
    values.fmt(f, indent)?;
    write!(f, " separated by ")?;
    puncts.fmt(f, indent)
}

/// Defines what we expect from a [`SeparatedList1`] when it fails.
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `fail`: Some nested [`ExpectsFormatter`] of the value combinator that actually failed.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[inline]
pub(crate) fn expects_separated_list1_fail(f: &mut Formatter, indent: usize, fail: ExpectsFormatter) -> FResult {
    write!(f, "at least one time ")?;
    fail.fmt(f, indent)
}

/// Defines what we expect from a [`SeparatedListN`].
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `n`: The number of times to apply the combinator.
/// - `values`: Some nested [`ExpectsFormatter`] of the values combinator that is being applied `n` times.
/// - `puncts`: Some nested [`ExpectsFormatter`] of the punctuation combinator.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[inline]
pub(crate) fn expects_separated_list_n(f: &mut Formatter, indent: usize, n: usize, values: ExpectsFormatter, puncts: ExpectsFormatter) -> FResult {
    write!(f, "a list of {n} times ")?;
    values.fmt(f, indent)?;
    write!(f, " separated by ")?;
    puncts.fmt(f, indent)
}

/// Defines what we expect from a [`SeparatedListN`] when it fails to parse a value.
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `n`: The number of times to apply the combinator.
/// - `values`: Some nested [`ExpectsFormatter`] of the values combinator that is being applied `n` times.
/// - `puncts`: Some nested [`ExpectsFormatter`] of the punctuation combinator.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[inline]
pub(crate) fn expects_separated_list_n_value(
    f: &mut Formatter,
    indent: usize,
    n: usize,
    values: ExpectsFormatter,
    puncts: ExpectsFormatter,
) -> FResult {
    write!(f, "the next value in a list of {n} times ")?;
    values.fmt(f, indent)?;
    write!(f, " separated by ")?;
    puncts.fmt(f, indent)
}

/// Defines what we expect from a [`SeparatedListN`] when it fails to parse a punctuation.
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Any indentation to apply should the nested expects write new lines.
/// - `n`: The number of times to apply the combinator.
/// - `values`: Some nested [`ExpectsFormatter`] of the values combinator that is being applied `n` times.
/// - `puncts`: Some nested [`ExpectsFormatter`] of the punctuation combinator.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
#[inline]
pub(crate) fn expects_separated_list_n_punct(
    f: &mut Formatter,
    indent: usize,
    n: usize,
    values: ExpectsFormatter,
    puncts: ExpectsFormatter,
) -> FResult {
    write!(f, "the next punctuation in a list of {n} times ")?;
    values.fmt(f, indent)?;
    write!(f, " separated by ")?;
    puncts.fmt(f, indent)
}





/***** LIBRARY FUNCTIONS *****/
/// Attempts to apply the given combinator as many times as possible.
///
/// All the parsed values are put into a [`Vec`].
///
/// Note that this combinator is OK with matching no input, and can therefore not fail.
/// If you want at least one, see [`many1()`] instead.
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `comb`inator as much as humanly possible.
#[inline]
pub fn many0<'c, F, S, C>(comb: C) -> Many0<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'c, F, S>,
{
    Many0 { comb, _f: Default::default(), _s: Default::default() }
}

/// Attempts to apply the given combinator as many times as possible, but at least once.
///
/// All the parsed values are put into a [`Vec`].
///
/// Note that this combinator is NOT okay with matching no input, and therefore fails if nothing could be matched.
/// See [`many0()`] for a version that is more lenient in this.
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `comb`inator as much as humanly possible.
///
/// # Fails
/// This function returns the failure returned by `value` if not at least one of those was parsed.
#[inline]
pub fn many1<'c, F, S, C>(comb: C) -> Many1<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'c, F, S>,
{
    Many1 { comb, _f: Default::default(), _s: Default::default() }
}

/// Attempts to apply the given combinator exactly `N` times.
///
/// All the parsed values are put into a [`Vec`] of `N` elements.
///
/// # Arguments
/// - `n`: The number of times to apply the combinator.
/// - `comb`: The combinator to apply `N` times.
///
/// # Returns
/// A new combinator that will apply the given `comb`inator `N` times.
///
/// # Fails
/// This function returns a [`Failure::ManyN`] if it failed to apply `comb` exactly `N` times.
#[inline]
pub fn many_n<'c, F, S, C>(n: usize, comb: C) -> ManyN<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'c, F, S>,
{
    ManyN { comb, n, _f: Default::default(), _s: Default::default() }
}



/// Attempts to parse a list of things separated by other things, discarding the parsed other things as we go.
///
/// This is useful for parsing lists where we don't care about the separator.
///
/// All the parsed values are put into a [`Vec`].
///
/// Note that this combinator is OK with matching no input, and can therefore not fail.
/// If you want at least one, see [`separated_list1()`] instead.
///
/// # Arguments
/// - `values`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `puncts`: The combinator to repeatedly apply to parse punctuation, discarding them, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator as much as humanly possible.
///
/// Note that any trailing punctuation is not parsed by this combinator.
#[inline]
pub fn separated_list0<'c, F, S, CV, CP>(values: CV, puncts: CP) -> SeparatedList0<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    SeparatedList0 { values, puncts, _f: Default::default(), _s: Default::default() }
}

/// Attempts to parse a list of things separated by other things, discarding the parsed other things as we go.
///
/// This is useful for parsing lists where we don't care about the separator.
///
/// All the parsed values are put into a [`Vec`].
///
/// Note that this combinator is NOT okay with matching no input, and therefore fails if nothing could be matched.
/// See [`separated_list0()`] for a version that is more lenient in this.
///
/// # Arguments
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation, discarding them, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator as much as humanly possible.
///
/// Note that any trailing punctuation is not parsed by this combinator.
///
/// # Fails
/// This function returns the failure returned by `value` if not at least one of those was parsed.
#[inline]
pub fn separated_list1<'c, F, S, CV, CP>(values: CV, puncts: CP) -> SeparatedList1<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    SeparatedList1 { values, puncts, _f: Default::default(), _s: Default::default() }
}

/// Attempts to apply the given combinator exactly `N` times, parsing separators in between.
///
/// All the parsed values are put into a [`Vec`] of `N` elements.
///
/// # Arguments
/// - `n`: The number of times to apply the `value` combinator.
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation, discarding them, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator `N` times.
///
/// Note that any trailing punctuation is not parsed by this list.
///
/// # Fails
/// This function returns a [`Failure::ManyN`] if it failed to apply `value` exactly `N` times.
#[inline]
pub fn separated_list_n<'c, F, S, CV, CP>(n: usize, values: CV, puncts: CP) -> SeparatedListN<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    SeparatedListN { values, puncts, n, _f: PhantomData::default(), _s: PhantomData::default() }
}



/// Attempts to parse a list of things separated by other things, keeping the parsed other things as we go.
///
/// This is useful for parsing lists where we care about the separator.
///
/// All the parsed values are put into a [`Punctuated`].
///
/// Note that this combinator is OK with matching no input, and can therefore not fail.
/// If you want at least one, see [`punctuated1()`] instead.
///
/// # Arguments
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation,until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator as much as humanly possible.
///
/// Note that any trailing punctuation is not parsed by this combinator.
#[cfg(feature = "punctuated")]
#[inline]
pub fn punctuated0<'c, F, S, CV, CP>(values: CV, puncts: CP) -> Punctuated0<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    Punctuated0 { values, puncts, _f: Default::default(), _s: Default::default() }
}

/// Attempts to parse a list of things separated by other things, keeping the parsed other things as we go.
///
/// This is useful for parsing lists where we care about the separator.
///
/// All the parsed values are put into a [`Punctuated`].
///
/// Note that this combinator is NOT okay with matching no input, and therefore fails if nothing could be matched.
/// See [`punctuated0()`] for a version that is more lenient in this.
///
/// # Arguments
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation,until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator as much as humanly possible.
///
/// Note that any trailing punctuation is not parsed by this combinator.
///
/// # Fails
/// This function returns the failure returned by `value` if not at least one of those was parsed.
#[cfg(feature = "punctuated")]
#[inline]
pub fn punctuated1<'c, F, S, CV, CP>(values: CV, puncts: CP) -> Punctuated1<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    Punctuated1 { values, puncts, _f: Default::default(), _s: Default::default() }
}

/// Attempts to apply the given combinator exactly `N` times, parsing separators in between.
///
/// All the parsed values are put into a [`Punctuated`] of `N` elements.
///
/// # Arguments
/// - `n`: The number of times to apply the `value` combinator.
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator `N` times.
///
/// Note that any trailing punctuation is not parsed by this list.
///
/// # Fails
/// This function returns a [`Failure::PunctuatedN`] if it failed to apply `value` exactly `N` times.
#[cfg(feature = "punctuated")]
#[inline]
pub fn punctuated_n<'c, F, S, CV, CP>(n: usize, values: CV, puncts: CP) -> PunctuatedN<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    PunctuatedN { values, puncts, n, _f: Default::default(), _s: Default::default() }
}



/// Attempts to parse a list of things separated by other things, keeping the parsed other things as we go.
///
/// This is useful for parsing lists where we care about the separator.
///
/// All the parsed values are put into a [`PunctuatedTrailing`].
///
/// Note that this combinator is OK with matching no input, and can therefore not fail.
/// If you want at least one, see [`punctuated_trailing1()`] instead.
///
/// # Arguments
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation,until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator as much as humanly possible.
///
/// Note that any trailing punctuation is parsed by this combinator and added to the returned result.
#[cfg(feature = "punctuated")]
#[inline]
pub fn punctuated_trailing0<'c, F, S, CV, CP>(values: CV, puncts: CP) -> PunctuatedTrailing0<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    PunctuatedTrailing0 { values, puncts, _f: Default::default(), _s: Default::default() }
}

/// Attempts to parse a list of things separated by other things, keeping the parsed other things as we go.
///
/// This is useful for parsing lists where we care about the separator.
///
/// All the parsed values are put into a [`PunctuatedTrailing`].
///
/// Note that this combinator is NOT okay with matching no input, and therefore fails if nothing could be matched.
/// See [`punctuated_trailing0()`] for a version that is more lenient in this.
///
/// # Arguments
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator as much as humanly possible.
///
/// Note that any trailing punctuation is parsed by this combinator and added to the returned result.
///
/// # Fails
/// This function returns the failure returned by `value` if not at least one of those was parsed.
#[cfg(feature = "punctuated")]
#[inline]
pub fn punctuated_trailing1<'c, F, S, CV, CP>(values: CV, puncts: CP) -> PunctuatedTrailing1<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    PunctuatedTrailing1 { values, puncts, _f: Default::default(), _s: Default::default() }
}

/// Attempts to apply the given combinator exactly `N` times, parsing separators in between.
///
/// All the parsed values are put into a [`PunctuatedTrailing`] of `N` elements.
///
/// # Arguments
/// - `n`: The number of times to apply the `value` combinator.
/// - `value`: The combinator to repeatedly apply to parse values, until it returns a [`Result::Fail`].
/// - `punct`: The combinator to repeatedly apply to parse punctuation, until it returns a [`Result::Fail`].
///
/// # Returns
/// A new combinator that will apply the given `value` combinator `N` times.
///
/// Note that any trailing punctuation is parsed by this combinator and added to the returned result.
///
/// # Fails
/// This function returns a [`Failure::PunctuatedTrailingN`] if it failed to apply `value` exactly `N` times.
#[cfg(feature = "punctuated")]
#[inline]
pub fn punctuated_trailing_n<'c, F, S, CV, CP>(n: usize, values: CV, puncts: CP) -> PunctuatedTrailingN<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    PunctuatedTrailingN { values, puncts, n, _f: Default::default(), _s: Default::default() }
}





/***** LIBRARY *****/
/// The combinator returned by [`many0()`].
pub struct Many0<F, S, C> {
    /// Some nested combinator to repeat.
    comb: C,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:   PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:   PhantomData<S>,
}
impl<F, S, C: Expects> Expects for Many0<F, S, C> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "multiple times ")?;
        self.comb.fmt(f, indent)
    }
}
impl<'c, F, S, C> Combinator<'c, F, S> for Many0<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'c, F, S>,
{
    type Output = Vec<C::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        let mut rem: Span<F, S> = input;
        let mut res: Vec<C::Output> = Vec::with_capacity(1);
        loop {
            match self.comb.parse(rem.clone()) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(new_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}

/// The combinator returned by [`many1()`].
pub struct Many1<F, S, C> {
    /// Some nested combinator to repeat at least once.
    comb: C,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:   PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:   PhantomData<S>,
}
impl<F, S, C: Expects> Expects for Many1<F, S, C> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult { expects_many1(f, indent, self.comb.expects()) }
}
impl<'c, F, S, C> Combinator<'c, F, S> for Many1<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'c, F, S>,
{
    type Output = Vec<C::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // Run the combinator at least once
        let (mut rem, mut res): (Span<F, S>, Vec<C::Output>) = match self.comb.parse(input) {
            Result::Ok(rem, res) => (rem, vec![res]),
            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(fail) => return Result::Fail(Failure::Common(Common::Many1 { fail: Box::new(fail.try_into().unwrap()) })),
            Result::Error(err) => return Result::Error(err),
        };

        // Then run it as much as we can get away with
        loop {
            match self.comb.parse(rem.clone()) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(new_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}

/// The combinator returned by [`many_n()`].
pub struct ManyN<F, S, C> {
    /// Some nested combinator to repeat a specific amount of times.
    comb: C,
    /// The amount of times to repeat it.
    n:    usize,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:   PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:   PhantomData<S>,
}
impl<F, S, C: Expects> Expects for ManyN<F, S, C> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult { expects_many_n(f, indent, self.n, self.comb.expects()) }
}
impl<'c, F, S, C> Combinator<'c, F, S> for ManyN<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'c, F, S>,
{
    type Output = Vec<C::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        let mut rem = input;
        let mut res = Vec::with_capacity(self.n);
        for i in 0..self.n {
            match self.comb.parse(rem.clone()) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(new_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(fail) => {
                    return Result::Fail(Failure::Common(Common::ManyN { n: self.n, i, fail: Box::new(fail.try_into().unwrap()) }));
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
        #[cfg(debug_assertions)]
        assert_eq!(res.len(), self.n);
        Result::Ok(rem, res)
    }
}



/// The combinator returned by [`separated_list0()`].
pub struct SeparatedList0<F, S, CV, CP> {
    /// Some nested combinator for the values.
    values: CV,
    /// Some nested combinator for the punctuation.
    puncts: CP,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:     PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:     PhantomData<S>,
}
impl<F, S, CV: Expects, CP: Expects> Expects for SeparatedList0<F, S, CV, CP> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "a list of ")?;
        self.values.fmt(f, indent)?;
        write!(f, " separated by ")?;
        self.puncts.fmt(f, indent)
    }
}
impl<'c, F, S, CV, CP> Combinator<'c, F, S> for SeparatedList0<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    type Output = Vec<CV::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // First parse a possible first value
        let (mut rem, mut res): (Span<F, S>, Vec<CV::Output>) = match self.values.parse(input.clone()) {
            Result::Ok(rem, res) => (rem, vec![res]),
            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(_) => return Result::Ok(input, vec![]),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        loop {
            // Only parse punctuation after the first value has been parsed
            let punct_rem: Span<F, S> = match self.puncts.parse(rem.clone()) {
                Result::Ok(rem, _) => rem,
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                // If we fail, that's the end of the list
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match self.values.parse(punct_rem) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(new_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(_) => {
                    // We don't return the punctuated parsed but only the OG one to not consume it
                    return Result::Ok(rem, res);
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}

/// The combinator returned by [`separated_list1()`].
pub struct SeparatedList1<F, S, CV, CP> {
    /// Some nested combinator for the values.
    values: CV,
    /// Some nested combinator for the punctuation.
    puncts: CP,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:     PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:     PhantomData<S>,
}
impl<F, S, CV: Expects, CP: Expects> Expects for SeparatedList1<F, S, CV, CP> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult { expects_separated_list1(f, indent, self.values.expects(), self.puncts.expects()) }
}
impl<'c, F, S, CV, CP> Combinator<'c, F, S> for SeparatedList1<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    type Output = Vec<CV::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // First parse a possible first value
        let (mut rem, mut res): (Span<F, S>, Vec<CV::Output>) = match self.values.parse(input) {
            Result::Ok(rem, res) => (rem, vec![res]),
            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(fail) => return Result::Fail(Failure::Common(Common::SeparatedList1 { fail: Box::new(fail.try_into().unwrap()) })),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        loop {
            // Only parse punctuation after the first value has been parsed
            let punct_rem: Span<F, S> = match self.puncts.parse(rem.clone()) {
                Result::Ok(rem, _) => rem,
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                // If we fail, that's the end of the list
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match self.values.parse(punct_rem) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(new_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(_) => {
                    // We don't return the punctuated parsed but only the OG one to not consume it
                    return Result::Ok(rem, res);
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}

/// The combinator returned by [`separated_list_n()`].
pub struct SeparatedListN<F, S, CV, CP> {
    /// Some nested combinator for the values.
    values: CV,
    /// Some nested combinator for the punctuation.
    puncts: CP,
    /// The hard number of values to parse.
    n:      usize,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:     PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:     PhantomData<S>,
}
impl<F, S, CV: Expects, CP: Expects> Expects for SeparatedListN<F, S, CV, CP> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        expects_separated_list_n(f, indent, self.n, self.values.expects(), self.puncts.expects())
    }
}
impl<'c, F, S, CV, CP> Combinator<'c, F, S> for SeparatedListN<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    type Output = Vec<CV::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // Do nothing if n == 0
        if self.n == 0 {
            return Result::Ok(input, Vec::new());
        }

        // First parse a possible first value
        let (mut rem, mut res): (Span<F, S>, Vec<CV::Output>) = match self.values.parse(input.clone()) {
            Result::Ok(rem, res) => (rem, vec![res]),
            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(fail) => {
                return Result::Fail(Failure::Common(Common::SeparatedListNValue {
                    n:      self.n,
                    i:      0,
                    values: Box::new(fail.try_into().unwrap()),
                    puncts: self.puncts.expects(),
                }));
            },
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        for i in 1..self.n {
            // Only parse punctuation after the first value has been parsed
            let punct_rem: Span<F, S> = match self.puncts.parse(rem.clone()) {
                Result::Ok(rem, _) => rem,
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                // If we fail, that's the end of the list
                Result::Fail(fail) => {
                    return Result::Fail(Failure::Common(Common::SeparatedListNPunct {
                        n: self.n,
                        i,
                        values: self.values.expects(),
                        puncts: Box::new(fail.try_into().unwrap()),
                    }));
                },
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match self.values.parse(punct_rem) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(new_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(fail) => {
                    return Result::Fail(Failure::Common(Common::SeparatedListNValue {
                        n: self.n,
                        i,
                        values: Box::new(fail.try_into().unwrap()),
                        puncts: self.puncts.expects(),
                    }));
                },
                Result::Error(err) => return Result::Error(err),
            }
        }

        // OK, we now made it this far
        #[cfg(debug_assertions)]
        assert_eq!(res.len(), self.n);
        Result::Ok(rem, res)
    }
}



/// The combinator returned by [`punctuated0()`].
#[cfg(feature = "punctuated")]
pub struct Punctuated0<F, S, CV, CP> {
    /// Some nested combinator for the values.
    values: CV,
    /// Some nested combinator for the punctuation.
    puncts: CP,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:     PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:     PhantomData<S>,
}
#[cfg(feature = "punctuated")]
impl<F, S, CV: Expects, CP: Expects> Expects for Punctuated0<F, S, CV, CP> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "a list of ")?;
        self.values.fmt(f, indent)?;
        write!(f, " separated by ")?;
        self.puncts.fmt(f, indent)
    }
}
#[cfg(feature = "punctuated")]
impl<'c, F, S, CV, CP> Combinator<'c, F, S> for Punctuated0<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    type Output = Punctuated<CV::Output, CP::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        let mut res: Punctuated<CV::Output, CP::Output> = Punctuated::new();

        // First parse a possible first value
        let mut rem: Span<F, S> = match self.values.parse(input.clone()) {
            Result::Ok(new_rem, new_res) => {
                res.push_first(new_res);
                new_rem
            },
            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(_) => return Result::Ok(input, res),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        loop {
            // Only parse punctuation after the first value has been parsed
            let (punct_rem, punct_res): (Span<F, S>, CP::Output) = match self.puncts.parse(rem.clone()) {
                Result::Ok(rem, res) => (rem, res),
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                // If we fail, that's the end of the list
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match self.values.parse(punct_rem) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(punct_res, new_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(_) => {
                    // We don't return the punctuated parsed but only the OG one to not consume it
                    return Result::Ok(rem, res);
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}

/// The combinator returned by [`punctuated1()`].
#[cfg(feature = "punctuated")]
pub struct Punctuated1<F, S, CV, CP> {
    /// Some nested combinator for the values.
    values: CV,
    /// Some nested combinator for the punctuation.
    puncts: CP,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:     PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:     PhantomData<S>,
}
#[cfg(feature = "punctuated")]
impl<F, S, CV: Expects, CP: Expects> Expects for Punctuated1<F, S, CV, CP> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult { expects_punctuated1(f, indent, self.values.expects(), self.puncts.expects()) }
}
#[cfg(feature = "punctuated")]
impl<'c, F, S, CV, CP> Combinator<'c, F, S> for Punctuated1<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    type Output = Punctuated<CV::Output, CP::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        let mut res: Punctuated<CV::Output, CP::Output> = Punctuated::new();

        // First parse a possible first value
        let mut rem: Span<F, S> = match self.values.parse(input) {
            Result::Ok(new_rem, new_res) => {
                res.push_first(new_res);
                new_rem
            },
            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(fail) => return Result::Fail(Failure::Common(Common::Punctuated1 { fail: Box::new(fail.try_into().unwrap()) })),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        loop {
            // Only parse punctuation after the first value has been parsed
            let (punct_rem, punct_res): (Span<F, S>, CP::Output) = match self.puncts.parse(rem.clone()) {
                Result::Ok(rem, res) => (rem, res),
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                // If we fail, that's the end of the list
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match self.values.parse(punct_rem) {
                Result::Ok(new_rem, new_res) => {
                    rem = new_rem;
                    res.push(punct_res, new_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(_) => {
                    // We don't return the punctuated parsed but only the OG one to not consume it
                    return Result::Ok(rem, res);
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}

/// The combinator returned by [`punctuated1()`].
#[cfg(feature = "punctuated")]
pub struct PunctuatedN<F, S, CV, CP> {
    /// Some nested combinator for the values.
    values: CV,
    /// Some nested combinator for the punctuation.
    puncts: CP,
    /// The amount of values to parse.
    n:      usize,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:     PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:     PhantomData<S>,
}
#[cfg(feature = "punctuated")]
impl<F, S, CV: Expects, CP: Expects> Expects for PunctuatedN<F, S, CV, CP> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        expects_separated_list_n(f, indent, self.n, self.values.expects(), self.puncts.expects())
    }
}
#[cfg(feature = "punctuated")]
impl<'c, F, S, CV, CP> Combinator<'c, F, S> for PunctuatedN<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    type Output = Punctuated<CV::Output, CP::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // Do nothing if n == 0
        if self.n == 0 {
            return Result::Ok(input, Punctuated::new());
        }

        // First parse a possible first value
        let mut res: Punctuated<CV::Output, CP::Output> = Punctuated::new();
        let mut rem: Span<F, S> = match self.values.parse(input) {
            Result::Ok(rem, value_res) => {
                res.push_first(value_res);
                rem
            },
            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(fail) => {
                return Result::Fail(Failure::Common(Common::PunctuatedNValue {
                    n:      self.n,
                    i:      0,
                    values: Box::new(fail.try_into().unwrap()),
                    puncts: self.puncts.expects(),
                }));
            },
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        for i in 1..self.n {
            // Only parse punctuation after the first value has been parsed
            let (punct_rem, punct_res): (Span<F, S>, CP::Output) = match self.puncts.parse(rem.clone()) {
                Result::Ok(rem, res) => (rem, res),
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                // If we fail, that's the end of the list
                Result::Fail(fail) => {
                    return Result::Fail(Failure::Common(Common::PunctuatedNPunct {
                        n: self.n,
                        i,
                        values: self.values.expects(),
                        puncts: Box::new(fail.try_into().unwrap()),
                    }));
                },
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match self.values.parse(punct_rem) {
                Result::Ok(value_rem, value_res) => {
                    rem = value_rem;
                    res.push(punct_res, value_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(fail) => {
                    return Result::Fail(Failure::Common(Common::PunctuatedNValue {
                        n: self.n,
                        i,
                        values: Box::new(fail.try_into().unwrap()),
                        puncts: self.puncts.expects(),
                    }));
                },
                Result::Error(err) => return Result::Error(err),
            }
        }

        // OK, we now made it this far
        #[cfg(debug_assertions)]
        assert_eq!(res.len(), self.n);
        Result::Ok(rem, res)
    }
}



/// The combinator returned by [`punctuated_trailing0()`].
#[cfg(feature = "punctuated")]
pub struct PunctuatedTrailing0<F, S, CV, CP> {
    /// Some nested combinator for the values.
    values: CV,
    /// Some nested combinator for the punctuation.
    puncts: CP,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:     PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:     PhantomData<S>,
}
#[cfg(feature = "punctuated")]
impl<F, S, CV: Expects, CP: Expects> Expects for PunctuatedTrailing0<F, S, CV, CP> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "a list of ")?;
        self.values.fmt(f, indent)?;
        write!(f, " separated by ")?;
        self.puncts.fmt(f, indent)?;
        write!(f, ", with optional trailing punctuation")
    }
}
#[cfg(feature = "punctuated")]
impl<'c, F, S, CV, CP> Combinator<'c, F, S> for PunctuatedTrailing0<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    type Output = PunctuatedTrailing<CV::Output, CP::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        let mut res: PunctuatedTrailing<CV::Output, CP::Output> = PunctuatedTrailing::new();

        // First parse a possible first value
        let mut rem: Span<F, S> = match self.values.parse(input.clone()) {
            Result::Ok(new_rem, new_res) => {
                res.push_value(new_res);
                new_rem
            },
            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(_) => return Result::Ok(input, res),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        loop {
            // Only parse punctuation after the first value has been parsed
            let punct_rem: Span<F, S> = match self.puncts.parse(rem.clone()) {
                Result::Ok(rem, punct_res) => {
                    res.push_punct(punct_res);
                    rem
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                // If we fail, that's the end of the list
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match self.values.parse(punct_rem) {
                Result::Ok(value_rem, value_res) => {
                    rem = value_rem;
                    res.push_value(value_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(_) => {
                    // We don't return the punctuated parsed but only the OG one to not consume it
                    return Result::Ok(rem, res);
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}

/// The combinator returned by [`punctuated_trailing1()`].
#[cfg(feature = "punctuated")]
pub struct PunctuatedTrailing1<F, S, CV, CP> {
    /// Some nested combinator for the values.
    values: CV,
    /// Some nested combinator for the punctuation.
    puncts: CP,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:     PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:     PhantomData<S>,
}
#[cfg(feature = "punctuated")]
impl<F, S, CV: Expects, CP: Expects> Expects for PunctuatedTrailing1<F, S, CV, CP> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        expects_punctuated_trailing1(f, indent, self.values.expects(), self.puncts.expects())
    }
}
#[cfg(feature = "punctuated")]
impl<'c, F, S, CV, CP> Combinator<'c, F, S> for PunctuatedTrailing1<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    type Output = PunctuatedTrailing<CV::Output, CP::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        let mut res: PunctuatedTrailing<CV::Output, CP::Output> = PunctuatedTrailing::new();

        // First parse a possible first value
        let mut rem: Span<F, S> = match self.values.parse(input) {
            Result::Ok(new_rem, new_res) => {
                res.push_value(new_res);
                new_rem
            },
            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(fail) => return Result::Fail(Failure::Common(Common::PunctuatedTrailing1 { fail: Box::new(fail.try_into().unwrap()) })),
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        loop {
            // Only parse punctuation after the first value has been parsed
            let punct_rem: Span<F, S> = match self.puncts.parse(rem.clone()) {
                Result::Ok(rem, punct_res) => {
                    res.push_punct(punct_res);
                    rem
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                // If we fail, that's the end of the list
                Result::Fail(_) => return Result::Ok(rem, res),
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match self.values.parse(punct_rem) {
                Result::Ok(value_rem, value_res) => {
                    rem = value_rem;
                    res.push_value(value_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(_) => {
                    // We don't return the punctuated parsed but only the OG one to not consume it
                    return Result::Ok(rem, res);
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
    }
}

/// The combinator returned by [`punctuated_trailing_n()`].
#[cfg(feature = "punctuated")]
pub struct PunctuatedTrailingN<F, S, CV, CP> {
    /// Some nested combinator for the values.
    values: CV,
    /// Some nested combinator for the punctuation.
    puncts: CP,
    /// The amount of values to parse.
    n:      usize,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:     PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:     PhantomData<S>,
}
#[cfg(feature = "punctuated")]
impl<F, S, CV: Expects, CP: Expects> Expects for PunctuatedTrailingN<F, S, CV, CP> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        expects_punctuated_trailing_n(f, indent, self.n, self.values.expects(), self.puncts.expects())
    }
}
#[cfg(feature = "punctuated")]
impl<'c, F, S, CV, CP> Combinator<'c, F, S> for PunctuatedTrailingN<F, S, CV, CP>
where
    F: Clone,
    S: Clone,
    CV: Combinator<'c, F, S>,
    CP: Combinator<'c, F, S>,
{
    type Output = PunctuatedTrailing<CV::Output, CP::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        // Do nothing if n == 0
        if self.n == 0 {
            return Result::Ok(input, PunctuatedTrailing::new());
        }

        // First parse a possible first value
        let mut res: PunctuatedTrailing<CV::Output, CP::Output> = PunctuatedTrailing::new();
        let mut rem: Span<F, S> = match self.values.parse(input) {
            Result::Ok(rem, value_res) => {
                res.push_value(value_res);
                rem
            },
            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(fail) => {
                return Result::Fail(Failure::Common(Common::PunctuatedTrailingNValue {
                    n:      self.n,
                    i:      0,
                    values: Box::new(fail.try_into().unwrap()),
                    puncts: self.puncts.expects(),
                }));
            },
            Result::Error(err) => return Result::Error(err),
        };

        // Then we parse punctuation/value pairs
        for i in 1..self.n {
            // Only parse punctuation after the first value has been parsed
            let punct_rem: Span<F, S> = match self.puncts.parse(rem.clone()) {
                Result::Ok(rem, punct_res) => {
                    res.push_punct(punct_res);
                    rem
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                // If we fail, that's the end of the list
                Result::Fail(fail) => {
                    return Result::Fail(Failure::Common(Common::PunctuatedTrailingNPunct {
                        n: self.n,
                        i,
                        values: self.values.expects(),
                        puncts: Box::new(fail.try_into().unwrap()),
                    }));
                },
                Result::Error(err) => return Result::Error(err),
            };

            // Then we attempt to parse a value
            match self.values.parse(punct_rem) {
                Result::Ok(value_rem, value_res) => {
                    rem = value_rem;
                    res.push_value(value_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(fail) => {
                    return Result::Fail(Failure::Common(Common::PunctuatedTrailingNValue {
                        n: self.n,
                        i,
                        values: Box::new(fail.try_into().unwrap()),
                        puncts: self.puncts.expects(),
                    }));
                },
                Result::Error(err) => return Result::Error(err),
            }
        }

        // OK, we now made it this far
        #[cfg(debug_assertions)]
        assert_eq!(res.len(), self.n);
        Result::Ok(rem, res)
    }
}
