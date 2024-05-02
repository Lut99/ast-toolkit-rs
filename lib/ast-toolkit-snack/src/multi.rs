//  MULTI.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:46:57
//  Last edited:
//    02 May 2024, 11:38:40
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators that repeat other combinators multiple times. A
//!   _variable_ number of times.
//

// Imports
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

#[cfg(feature = "punctuated")]
use ast_toolkit_punctuated::{Punctuated, PunctuatedTrailing};
use ast_toolkit_span::Span;

use crate::error::{Common, Failure};
use crate::{Combinator, Expects, ExpectsFormatter, Result};


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
/// A combinator [`Many0`] that applies the given `comb`inator until it fails.
///
/// # Fails
/// The returned combinator cannot fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::multi::many0;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hellohellohellogoodbye");
/// let span2 = Span::new("<example>", "hellohelgoodbye");
/// let span3 = Span::new("<example>", "goodbye");
///
/// let mut comb = many0(tag("hello"));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(15..), vec![span1.slice(..5), span1.slice(5..10), span1.slice(10..15)])
/// );
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(5..), vec![span2.slice(..5)]));
/// assert_eq!(comb.parse(span3).unwrap(), (span3, vec![]));
/// ```
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
/// A combinator [`Many1`] that applies the given `comb`inator until it fails.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator couldn't be applied at least once.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::multi::many1;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hellohellohellogoodbye");
/// let span2 = Span::new("<example>", "hellohelgoodbye");
/// let span3 = Span::new("<example>", "goodbye");
///
/// let mut comb = many1(tag("hello"));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(15..), vec![span1.slice(..5), span1.slice(5..10), span1.slice(10..15)])
/// );
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(5..), vec![span2.slice(..5)]));
/// assert!(matches!(comb.parse(span3), SResult::Fail(Failure::Common(Common::Many1 { .. }))));
/// ```
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
/// A combinator [`ManyN`] that applies the given `comb`inator exactly `n` times.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator couldn't be applied `n` times.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::multi::many_n;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hellohellohellogoodbye");
/// let span2 = Span::new("<example>", "hellohelgoodbye");
/// let span3 = Span::new("<example>", "goodbye");
///
/// let mut comb = many_n(2, tag("hello"));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(10..), vec![span1.slice(..5), span1.slice(5..10)])
/// );
/// assert!(matches!(
///     comb.parse(span2),
///     SResult::Fail(Failure::Common(Common::ManyN { i: 1, .. }))
/// ));
/// assert!(matches!(
///     comb.parse(span3),
///     SResult::Fail(Failure::Common(Common::ManyN { i: 0, .. }))
/// ));
/// ```
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
/// A combinator [`SeparatedList0`] that applies the given `comb`inator until it fails.
///
/// Note that any trailing punctuation is not parsed by this combinator.
///
/// # Fails
/// The returned combinator cannot fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::multi::separated_list0;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hello,hello,hello,goodbye");
/// let span2 = Span::new("<example>", "hello,hel,goodbye");
/// let span3 = Span::new("<example>", "hellohello,goodbye");
/// let span4 = Span::new("<example>", "goodbye");
///
/// let mut comb = separated_list0(tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(17..), vec![span1.slice(..5), span1.slice(6..11), span1.slice(12..17)])
/// );
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(5..), vec![span2.slice(..5)]));
/// assert_eq!(comb.parse(span3).unwrap(), (span3.slice(5..), vec![span3.slice(..5)]));
/// assert_eq!(comb.parse(span4).unwrap(), (span4, vec![]));
/// ```
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
/// A combinator [`SeparatedList1`] that applies the given `comb`inator until it fails.
///
/// Note that any trailing punctuation is not parsed by this combinator.
///
/// # Fails
/// The returned combinator fails if the given `value` combinator couldn't be applied at least once.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::multi::separated_list1;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hello,hello,hello,goodbye");
/// let span2 = Span::new("<example>", "hello,hel,goodbye");
/// let span3 = Span::new("<example>", "hellohello,goodbye");
/// let span4 = Span::new("<example>", "goodbye");
///
/// let mut comb = separated_list1(tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(17..), vec![span1.slice(..5), span1.slice(6..11), span1.slice(12..17)])
/// );
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(5..), vec![span2.slice(..5)]));
/// assert_eq!(comb.parse(span3).unwrap(), (span3.slice(5..), vec![span3.slice(..5)]));
/// assert!(matches!(
///     comb.parse(span4),
///     SResult::Fail(Failure::Common(Common::SeparatedList1 { .. }))
/// ));
/// ```
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
/// A combinator [`SeparatedListN`] that applies the given `value` combinator exactly `n` times.
///
/// Note that any trailing punctuation is not parsed by this combinator.
///
/// # Fails
/// The returned combinator fails if the given `value` combinator couldn't be applied `n` times.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::multi::separated_list_n;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hello,hello,hello,goodbye");
/// let span2 = Span::new("<example>", "hello,hel,goodbye");
/// let span3 = Span::new("<example>", "hellohello,goodbye");
/// let span4 = Span::new("<example>", "goodbye");
///
/// let mut comb = separated_list_n(2, tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(11..), vec![span1.slice(..5), span1.slice(6..11)])
/// );
/// assert!(matches!(
///     comb.parse(span2),
///     SResult::Fail(Failure::Common(Common::SeparatedListNValue { i: 1, .. }))
/// ));
/// assert!(matches!(
///     comb.parse(span3),
///     SResult::Fail(Failure::Common(Common::SeparatedListNPunct { i: 1, .. }))
/// ));
/// assert!(matches!(
///     comb.parse(span4),
///     SResult::Fail(Failure::Common(Common::SeparatedListNValue { i: 0, .. }))
/// ));
/// ```
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
/// A combinator [`Punctuated0`] that applies the given `comb`inator until it fails.
///
/// Note that any trailing punctuation is not parsed by this combinator.
///
/// # Fails
/// The returned combinator cannot fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_punctuated::punct;
/// use ast_toolkit_snack::multi::punctuated0;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hello,hello,hello,goodbye");
/// let span2 = Span::new("<example>", "hello,hel,goodbye");
/// let span3 = Span::new("<example>", "hellohello,goodbye");
/// let span4 = Span::new("<example>", "goodbye");
///
/// let mut comb = punctuated0(tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(17..), punct![v => span1.slice(..5), p => span1.slice(5..6), v => span1.slice(6..11), p => span1.slice(11..12), v => span1.slice(12..17)])
/// );
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(5..), punct![v => span2.slice(..5)]));
/// assert_eq!(comb.parse(span3).unwrap(), (span3.slice(5..), punct![v => span3.slice(..5)]));
/// assert_eq!(comb.parse(span4).unwrap(), (span4, punct![]));
/// ```
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
/// A combinator [`Punctuated1`] that applies the given `comb`inator until it fails.
///
/// Note that any trailing punctuation is not parsed by this combinator.
///
/// # Fails
/// The returned combinator fails if the given `value` combinator couldn't be applied at least once.
///
/// # Example
/// ```rust
/// use ast_toolkit_punctuated::punct;
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::multi::punctuated1;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hello,hello,hello,goodbye");
/// let span2 = Span::new("<example>", "hello,hel,goodbye");
/// let span3 = Span::new("<example>", "hellohello,goodbye");
/// let span4 = Span::new("<example>", "goodbye");
///
/// let mut comb = punctuated1(tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(17..), punct![v => span1.slice(..5), p => span1.slice(5..6), v => span1.slice(6..11), p => span1.slice(11..12), v => span1.slice(12..17)])
/// );
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(5..), punct![v => span2.slice(..5)]));
/// assert_eq!(comb.parse(span3).unwrap(), (span3.slice(5..), punct![v => span3.slice(..5)]));
/// assert!(matches!(
///     comb.parse(span4),
///     SResult::Fail(Failure::Common(Common::Punctuated1 { .. }))
/// ));
/// ```
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
/// A combinator [`PunctuatedN`] that applies the given `value` combinator exactly `n` times.
///
/// Note that any trailing punctuation is not parsed by this combinator.
///
/// # Fails
/// The returned combinator fails if the given `value` combinator couldn't be applied `n` times.
///
/// # Example
/// ```rust
/// use ast_toolkit_punctuated::punct;
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::multi::punctuated_n;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hello,hello,hello,goodbye");
/// let span2 = Span::new("<example>", "hello,hel,goodbye");
/// let span3 = Span::new("<example>", "hellohello,goodbye");
/// let span4 = Span::new("<example>", "goodbye");
///
/// let mut comb = punctuated_n(2, tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(11..), punct![v => span1.slice(..5), p => span1.slice(5..6), v => span1.slice(6..11)])
/// );
/// assert!(matches!(
///     comb.parse(span2),
///     SResult::Fail(Failure::Common(Common::PunctuatedNValue { i: 1, .. }))
/// ));
/// assert!(matches!(
///     comb.parse(span3),
///     SResult::Fail(Failure::Common(Common::PunctuatedNPunct { i: 1, .. }))
/// ));
/// assert!(matches!(
///     comb.parse(span4),
///     SResult::Fail(Failure::Common(Common::PunctuatedNValue { i: 0, .. }))
/// ));
/// ```
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
/// A combinator [`PunctuatedTrailing0`] that applies the given `value` combinator until it fails.
///
/// Note that any trailing punctuation is also consumed by this combinator.
///
/// # Fails
/// The returned combinator cannot fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_punctuated::punct_trail;
/// use ast_toolkit_snack::multi::punctuated_trailing0;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hello,hello,hello,goodbye");
/// let span2 = Span::new("<example>", "hello,hel,goodbye");
/// let span3 = Span::new("<example>", "hellohello,goodbye");
/// let span4 = Span::new("<example>", "goodbye");
///
/// let mut comb = punctuated_trailing0(tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(18..), punct_trail![v => span1.slice(..5), p => span1.slice(5..6), v => span1.slice(6..11), p => span1.slice(11..12), v => span1.slice(12..17), p => span1.slice(17..18)])
/// );
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(6..), punct_trail![v => span2.slice(..5), p => span2.slice(5..6)]));
/// assert_eq!(comb.parse(span3).unwrap(), (span3.slice(5..), punct_trail![v => span3.slice(..5)]));
/// assert_eq!(comb.parse(span4).unwrap(), (span4, punct_trail![]));
/// ```
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
/// A combinator [`PunctuatedTrailing1`] that applies the given `value` combinator until it fails.
///
/// Note that any trailing punctuation is also consumed by this combinator.
///
/// # Fails
/// The returned combinator fails if it failed to apply the given `value` combinator at least once.
///
/// # Example
/// ```rust
/// use ast_toolkit_punctuated::punct_trail;
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::multi::punctuated_trailing1;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hello,hello,hello,goodbye");
/// let span2 = Span::new("<example>", "hello,hel,goodbye");
/// let span3 = Span::new("<example>", "hellohello,goodbye");
/// let span4 = Span::new("<example>", "goodbye");
///
/// let mut comb = punctuated_trailing1(tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(18..), punct_trail![v => span1.slice(..5), p => span1.slice(5..6), v => span1.slice(6..11), p => span1.slice(11..12), v => span1.slice(12..17), p => span1.slice(17..18)])
/// );
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(6..), punct_trail![v => span2.slice(..5), p => span2.slice(5..6)]));
/// assert_eq!(comb.parse(span3).unwrap(), (span3.slice(5..), punct_trail![v => span3.slice(..5)]));
/// assert!(matches!(
///     comb.parse(span4),
///     SResult::Fail(Failure::Common(Common::PunctuatedTrailing1 { .. }))
/// ));
/// ```
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
/// A combinator [`PunctuatedTrailingN`] that applies the given `value` combinator exactly `n` times.
///
/// Note that any trailing punctuation is also consumed by this combinator.
///
/// # Fails
/// The returned combinator fails if it failed to apply the given `value` combinator `n` times.
///
/// # Example
/// ```rust
/// use ast_toolkit_punctuated::punct_trail;
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::multi::punctuated_trailing_n;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_snack::{Combinator as _, Result as SResult};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hello,hello,hello,goodbye");
/// let span2 = Span::new("<example>", "hello,hel,goodbye");
/// let span3 = Span::new("<example>", "hellohello,goodbye");
/// let span4 = Span::new("<example>", "goodbye");
///
/// let mut comb = punctuated_trailing_n(2, tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(12..), punct_trail![v => span1.slice(..5), p => span1.slice(5..6), v => span1.slice(6..11), p => span1.slice(11..12)])
/// );
/// assert!(matches!(
///     comb.parse(span2),
///     SResult::Fail(Failure::Common(Common::PunctuatedTrailingNValue { i: 1, .. }))
/// ));
/// assert!(matches!(
///     comb.parse(span3),
///     SResult::Fail(Failure::Common(Common::PunctuatedTrailingNPunct { i: 1, .. }))
/// ));
/// assert!(matches!(
///     comb.parse(span4),
///     SResult::Fail(Failure::Common(Common::PunctuatedTrailingNValue { i: 0, .. }))
/// ));
/// ```
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





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Many0`] combinator.
#[derive(Debug)]
pub struct Many0Expects<'t> {
    /// The thing we expect multiple times.
    fmt: Box<dyn 't + ExpectsFormatter>,
}
impl<'t> Display for Many0Expects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for Many0Expects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "multiple repetitions of ")?;
        self.fmt.expects_fmt(f, indent)
    }
}

/// ExpectsFormatter for the [`Many1`] combinator.
#[derive(Debug)]
pub struct Many1Expects<'t> {
    /// The thing we expect multiple times.
    fmt: Box<dyn 't + ExpectsFormatter>,
}
impl<'t> Display for Many1Expects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for Many1Expects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "at least one repetition of ")?;
        self.fmt.expects_fmt(f, indent)
    }
}

/// ExpectsFormatter for the [`ManyN`] combinator.
#[derive(Debug)]
pub struct ManyNExpects<'t> {
    /// The thing we expect multiple times.
    fmt: Box<dyn 't + ExpectsFormatter>,
    /// How many times we expect the thing.
    n:   usize,
}
impl<'t> Display for ManyNExpects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for ManyNExpects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "exactly {} repetitions of ", self.n)?;
        self.fmt.expects_fmt(f, indent)
    }
}



/// ExpectsFormatter for the [`SeparatedList0`] combinator.
#[derive(Debug)]
pub struct SeparatedList0Expects<'t> {
    /// The thing we expect multiple times.
    value_fmt: Box<dyn 't + ExpectsFormatter>,
    /// The thing separating the values.
    punct_fmt: Box<dyn 't + ExpectsFormatter>,
}
impl<'t> Display for SeparatedList0Expects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for SeparatedList0Expects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "multiple repetitions of ")?;
        self.value_fmt.expects_fmt(f, indent)?;
        write!(f, ", separated by ")?;
        self.punct_fmt.expects_fmt(f, indent)
    }
}

/// ExpectsFormatter for the [`SeparatedList1`] combinator.
#[derive(Debug)]
pub struct SeparatedList1Expects<'t> {
    /// The thing we expect multiple times.
    value_fmt: Box<dyn 't + ExpectsFormatter>,
    /// The thing separating the values.
    punct_fmt: Box<dyn 't + ExpectsFormatter>,
}
impl<'t> Display for SeparatedList1Expects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for SeparatedList1Expects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "at least one repetition of ")?;
        self.value_fmt.expects_fmt(f, indent)?;
        write!(f, ", separated by ")?;
        self.punct_fmt.expects_fmt(f, indent)
    }
}

/// ExpectsFormatter for the [`SeparatedListN`] combinator.
#[derive(Debug)]
pub struct SeparatedListNExpects<'t> {
    /// The thing we expect multiple times.
    value_fmt: Box<dyn 't + ExpectsFormatter>,
    /// The thing separating the values.
    punct_fmt: Box<dyn 't + ExpectsFormatter>,
    /// How many times we expect the value thing.
    n: usize,
}
impl<'t> Display for SeparatedListNExpects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for SeparatedListNExpects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "exactly {} repetitions of ", self.n)?;
        self.value_fmt.expects_fmt(f, indent)?;
        write!(f, ", separated by ")?;
        self.punct_fmt.expects_fmt(f, indent)
    }
}



/// ExpectsFormatter for the [`Punctuated0`] combinator.
#[cfg(feature = "punctuated")]
#[derive(Debug)]
pub struct Punctuated0Expects<'t> {
    /// The thing we expect multiple times.
    value_fmt: Box<dyn 't + ExpectsFormatter>,
    /// The thing separating the values.
    punct_fmt: Box<dyn 't + ExpectsFormatter>,
}
#[cfg(feature = "punctuated")]
impl<'t> Display for Punctuated0Expects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
#[cfg(feature = "punctuated")]
impl<'t> ExpectsFormatter for Punctuated0Expects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "multiple repetitions of ")?;
        self.value_fmt.expects_fmt(f, indent)?;
        write!(f, ", separated by ")?;
        self.punct_fmt.expects_fmt(f, indent)
    }
}

/// ExpectsFormatter for the [`Punctuated1`] combinator.
#[cfg(feature = "punctuated")]
#[derive(Debug)]
pub struct Punctuated1Expects<'t> {
    /// The thing we expect multiple times.
    value_fmt: Box<dyn 't + ExpectsFormatter>,
    /// The thing separating the values.
    punct_fmt: Box<dyn 't + ExpectsFormatter>,
}
#[cfg(feature = "punctuated")]
impl<'t> Display for Punctuated1Expects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
#[cfg(feature = "punctuated")]
impl<'t> ExpectsFormatter for Punctuated1Expects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "at least one repetition of ")?;
        self.value_fmt.expects_fmt(f, indent)?;
        write!(f, ", separated by ")?;
        self.punct_fmt.expects_fmt(f, indent)
    }
}

/// ExpectsFormatter for the [`PunctuatedN`] combinator.
#[cfg(feature = "punctuated")]
#[derive(Debug)]
pub struct PunctuatedNExpects<'t> {
    /// The thing we expect multiple times.
    value_fmt: Box<dyn 't + ExpectsFormatter>,
    /// The thing separating the values.
    punct_fmt: Box<dyn 't + ExpectsFormatter>,
    /// How many times we expect the value thing.
    n: usize,
}
#[cfg(feature = "punctuated")]
impl<'t> Display for PunctuatedNExpects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
#[cfg(feature = "punctuated")]
impl<'t> ExpectsFormatter for PunctuatedNExpects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "exactly {} repetitions of ", self.n)?;
        self.value_fmt.expects_fmt(f, indent)?;
        write!(f, ", separated by ")?;
        self.punct_fmt.expects_fmt(f, indent)
    }
}



/// ExpectsFormatter for the [`PunctuatedTrailing0`] combinator.
#[cfg(feature = "punctuated")]
#[derive(Debug)]
pub struct PunctuatedTrailing0Expects<'t> {
    /// The thing we expect multiple times.
    value_fmt: Box<dyn 't + ExpectsFormatter>,
    /// The thing separating the values.
    punct_fmt: Box<dyn 't + ExpectsFormatter>,
}
#[cfg(feature = "punctuated")]
impl<'t> Display for PunctuatedTrailing0Expects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
#[cfg(feature = "punctuated")]
impl<'t> ExpectsFormatter for PunctuatedTrailing0Expects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "multiple repetitions of ")?;
        self.value_fmt.expects_fmt(f, indent)?;
        write!(f, ", separated by ")?;
        self.punct_fmt.expects_fmt(f, indent)?;
        write!(f, " (optionally trailing)")
    }
}

/// ExpectsFormatter for the [`PunctuatedTrailing1`] combinator.
#[cfg(feature = "punctuated")]
#[derive(Debug)]
pub struct PunctuatedTrailing1Expects<'t> {
    /// The thing we expect multiple times.
    value_fmt: Box<dyn 't + ExpectsFormatter>,
    /// The thing separating the values.
    punct_fmt: Box<dyn 't + ExpectsFormatter>,
}
#[cfg(feature = "punctuated")]
impl<'t> Display for PunctuatedTrailing1Expects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
#[cfg(feature = "punctuated")]
impl<'t> ExpectsFormatter for PunctuatedTrailing1Expects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "at least one repetition of ")?;
        self.value_fmt.expects_fmt(f, indent)?;
        write!(f, ", separated by ")?;
        self.punct_fmt.expects_fmt(f, indent)?;
        write!(f, " (optionally trailing)")
    }
}

/// ExpectsFormatter for the [`PunctuatedTrailingN`] combinator.
#[cfg(feature = "punctuated")]
#[derive(Debug)]
pub struct PunctuatedTrailingNExpects<'t> {
    /// The thing we expect multiple times.
    value_fmt: Box<dyn 't + ExpectsFormatter>,
    /// The thing separating the values.
    punct_fmt: Box<dyn 't + ExpectsFormatter>,
    /// How many times we expect the value thing.
    n: usize,
}
#[cfg(feature = "punctuated")]
impl<'t> Display for PunctuatedTrailingNExpects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
#[cfg(feature = "punctuated")]
impl<'t> ExpectsFormatter for PunctuatedTrailingNExpects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "exactly {} repetitions of ", self.n)?;
        self.value_fmt.expects_fmt(f, indent)?;
        write!(f, ", separated by ")?;
        self.punct_fmt.expects_fmt(f, indent)?;
        write!(f, " (optionally trailing)")
    }
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
impl<'t, F, S, C: Expects<'t>> Expects<'t> for Many0<F, S, C> {
    type Formatter = Many0Expects<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter { Many0Expects { fmt: Box::new(self.comb.expects()) } }
}
impl<'t, F, S, C> Combinator<'t, F, S> for Many0<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'t, F, S>,
{
    type Output = Vec<C::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S> {
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
impl<'t, F, S, C: Expects<'t>> Expects<'t> for Many1<F, S, C> {
    type Formatter = Many1Expects<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter { Many1Expects { fmt: Box::new(self.comb.expects()) } }
}
impl<'t, F, S, C> Combinator<'t, F, S> for Many1<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'t, F, S>,
{
    type Output = Vec<C::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S> {
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
impl<'t, F, S, C: Expects<'t>> Expects<'t> for ManyN<F, S, C> {
    type Formatter = ManyNExpects<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter { ManyNExpects { fmt: Box::new(self.comb.expects()), n: self.n } }
}
impl<'t, F, S, C> Combinator<'t, F, S> for ManyN<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'t, F, S>,
{
    type Output = Vec<C::Output>;

    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S> {
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
impl<'c, F, S, CV: Expects<'c>, CP: Expects<'c>> Expects<'c> for SeparatedList0<F, S, CV, CP> {
    type Formatter = SeparatedList0Expects<'c>;

    #[inline]
    fn expects(&self) -> Self::Formatter {
        SeparatedList0Expects { value_fmt: Box::new(self.values.expects()), punct_fmt: Box::new(self.puncts.expects()) }
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
impl<'c, F, S, CV: Expects<'c>, CP: Expects<'c>> Expects<'c> for SeparatedList1<F, S, CV, CP> {
    type Formatter = SeparatedList1Expects<'c>;

    #[inline]
    fn expects(&self) -> Self::Formatter {
        SeparatedList1Expects { value_fmt: Box::new(self.values.expects()), punct_fmt: Box::new(self.puncts.expects()) }
    }
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
impl<'c, F, S, CV: Expects<'c>, CP: Expects<'c>> Expects<'c> for SeparatedListN<F, S, CV, CP> {
    type Formatter = SeparatedListNExpects<'c>;

    #[inline]
    fn expects(&self) -> Self::Formatter {
        SeparatedListNExpects { value_fmt: Box::new(self.values.expects()), punct_fmt: Box::new(self.puncts.expects()), n: self.n }
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
                    puncts: Box::new(self.puncts.expects()),
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
                        values: Box::new(self.values.expects()),
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
                        puncts: Box::new(self.puncts.expects()),
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
impl<'c, F, S, CV: Expects<'c>, CP: Expects<'c>> Expects<'c> for Punctuated0<F, S, CV, CP> {
    type Formatter = Punctuated0Expects<'c>;

    #[inline]
    fn expects(&self) -> Self::Formatter {
        Punctuated0Expects { value_fmt: Box::new(self.values.expects()), punct_fmt: Box::new(self.puncts.expects()) }
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
impl<'c, F, S, CV: Expects<'c>, CP: Expects<'c>> Expects<'c> for Punctuated1<F, S, CV, CP> {
    type Formatter = Punctuated1Expects<'c>;

    #[inline]
    fn expects(&self) -> Self::Formatter {
        Punctuated1Expects { value_fmt: Box::new(self.values.expects()), punct_fmt: Box::new(self.puncts.expects()) }
    }
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
impl<'c, F, S, CV: Expects<'c>, CP: Expects<'c>> Expects<'c> for PunctuatedN<F, S, CV, CP> {
    type Formatter = PunctuatedNExpects<'c>;

    #[inline]
    fn expects(&self) -> Self::Formatter {
        PunctuatedNExpects { value_fmt: Box::new(self.values.expects()), punct_fmt: Box::new(self.puncts.expects()), n: self.n }
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
                    puncts: Box::new(self.puncts.expects()),
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
                        values: Box::new(self.values.expects()),
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
                        puncts: Box::new(self.puncts.expects()),
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
impl<'c, F, S, CV: Expects<'c>, CP: Expects<'c>> Expects<'c> for PunctuatedTrailing0<F, S, CV, CP> {
    type Formatter = PunctuatedTrailing0Expects<'c>;

    #[inline]
    fn expects(&self) -> Self::Formatter {
        PunctuatedTrailing0Expects { value_fmt: Box::new(self.values.expects()), punct_fmt: Box::new(self.puncts.expects()) }
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
            match self.values.parse(punct_rem.clone()) {
                Result::Ok(value_rem, value_res) => {
                    rem = value_rem;
                    res.push_value(value_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(_) => return Result::Ok(punct_rem, res),
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
impl<'c, F, S, CV: Expects<'c>, CP: Expects<'c>> Expects<'c> for PunctuatedTrailing1<F, S, CV, CP> {
    type Formatter = PunctuatedTrailing1Expects<'c>;

    #[inline]
    fn expects(&self) -> Self::Formatter {
        PunctuatedTrailing1Expects { value_fmt: Box::new(self.values.expects()), punct_fmt: Box::new(self.puncts.expects()) }
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
            match self.values.parse(punct_rem.clone()) {
                Result::Ok(value_rem, value_res) => {
                    rem = value_rem;
                    res.push_value(value_res);
                },
                Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                Result::Fail(_) => return Result::Ok(punct_rem, res),
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
impl<'c, F, S, CV: Expects<'c>, CP: Expects<'c>> Expects<'c> for PunctuatedTrailingN<F, S, CV, CP> {
    type Formatter = PunctuatedTrailingNExpects<'c>;

    #[inline]
    fn expects(&self) -> Self::Formatter {
        PunctuatedTrailingNExpects { value_fmt: Box::new(self.values.expects()), punct_fmt: Box::new(self.puncts.expects()), n: self.n }
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
                    puncts: Box::new(self.puncts.expects()),
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
                        values: Box::new(self.values.expects()),
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
                        puncts: Box::new(self.puncts.expects()),
                    }));
                },
                Result::Error(err) => return Result::Error(err),
            }
        }
        #[cfg(debug_assertions)]
        assert_eq!(res.len(), self.n);

        // Parse the optional trailing punctuation
        match self.puncts.parse(rem.clone()) {
            Result::Ok(rem, punct_res) => {
                res.push_punct(punct_res);
                Result::Ok(rem, res)
            },
            Result::Fail(Failure::NotEnough { needed, span }) => Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(_) => Result::Ok(rem, res),
            Result::Error(err) => Result::Error(err),
        }
    }
}
