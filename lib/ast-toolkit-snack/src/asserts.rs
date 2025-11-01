//  ASSERTS.rs
//    by Lut99
//
//  Description:
//!   Implements a few no-op functions that can be used to assert properties of various input
//!   combinators.
//!
//!   This is generally useful for when you're going to make (potentially unsafe) assumptions about
//!   combinators written by a third-party library (or yourself) and want to be aware at compile
//!   time when they are violated.
//

use std::convert::Infallible;

use crate::span::Source;
use crate::spec::Combinator;


/***** LIBRARY *****/
/// Asserts statically that your given input combinator cannot fail at all (recoverably or
/// otherwise).
///
/// This is useful if you are making assumptions later about e.g. unwrapping the [`Result`]
/// returned by it.
///
/// If you cannot, or don't want to, name the type, see [`assert_infallible_value()`] instead to
/// give an instantiated combinator.
///
/// # Generics
/// - `'c`: Any lifetime upon which the `C`ombinator depends.
/// - `S`: The [`Source`] which may be parsed by your `C`ombinator.
/// - `C`: The type of your combinator to check.
///
/// # Examples
/// We can assert that the [`while0()`](crate::scan::while0())-combinator never fails:
/// ```rust
/// use ast_toolkit_snack::asserts::assert_infallible;
/// use ast_toolkit_snack::scan::while0;
///
/// // We asserted it holds for the given input `S`ource
/// assert_infallible::<&[u8], while0::While0<fn(&u8) -> bool, &[u8]>>();
/// ```
///
/// The same cannot be asserted of the [`tag()`](crate::scan::tag())-combinator:
/// ```compile_fail
/// use ast_toolkit_snack::asserts::assert_infallible;
/// use ast_toolkit_snack::scan::tag;
///
/// // Triggers a compilation error!
/// assert_infallible::<&[u8], tag::Tag<u8, &[u8]>>();
/// ```
#[inline]
pub const fn assert_infallible<'c, 's, S: ?Sized + Source, C: Combinator<'c, 's, S, Recoverable = Infallible, Fatal = Infallible>>() {}

/// Asserts statically that your given input combinator cannot fail at all (recoverably or
/// otherwise).
///
/// This is useful if you are making assumptions later about e.g. unwrapping the [`Result`]
/// returned by it.
///
/// If you cannot, or don't want to, instantiate your combinator to assert it, use
/// [`assert_infallible()`] instead to manually name the type.
///
/// # Generics
/// - `'c`: Any lifetime upon which the `C`ombinator depends.
/// - `S`: The [`Source`] which may be parsed by your `C`ombinator.
/// - `C`: The type of your combinator to check.
///
/// # Arguments
/// - `comb`: An instance of your combinator by which we will derive the generics.
///
/// # Examples
/// We can assert that the [`while0`](crate::scan::while0())-combinator never fails:
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::asserts::assert_infallible_value;
/// use ast_toolkit_snack::scan::while0;
/// use ast_toolkit_span::Span;
///
/// // We asserted it holds for the given instantiation
/// let mut comb = while0("anything", |_| true);
/// assert_infallible_value(&comb);
///
/// // Then we do some parsing with it (helps to deduce 'S')
/// assert!(comb.parse(Span::new("Hello, world")).is_ok());
/// ```
///
/// The same cannot be asserted of the [`tag()`](crate::scan::tag())-combinator:
/// ```compile_fail
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::asserts::assert_infallible_value;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_span::Span;
///
/// // Triggers a compilation error!
/// let mut comb = tag(b"hello");
/// assert_infallible_value(&comb);
///
/// // Then we do some parsing with it (helps to deduce 'S')
/// assert!(comb.parse(Span::new("Hello, world")).is_ok());
/// ```
#[inline]
pub const fn assert_infallible_value<'c, 's, S: ?Sized + Source, C: Combinator<'c, 's, S, Recoverable = Infallible, Fatal = Infallible>>(_comb: &C) {}



/// Asserts statically that your given input combinator cannot fail recoverably.
///
/// This is useful if you are making assumptions later about e.g. unwrapping the [`Result`]
/// returned by it.
///
/// If you cannot, or don't want to, name the type, see [`assert_infallible_recoverable_value()`] instead to
/// give an instantiated combinator.
///
/// # Generics
/// - `'c`: Any lifetime upon which the `C`ombinator depends.
/// - `'s`: A lifetime for which the `S`ource is valid.
/// - `S`: The [`Source`] which may be parsed by your `C`ombinator.
/// - `C`: The type of your combinator to check.
///
/// # Examples
/// We can assert that the [`while0()`](crate::scan::while0())-combinator never fails:
/// ```rust
/// use ast_toolkit_snack::asserts::assert_infallible_recoverable;
/// use ast_toolkit_snack::scan::while0;
///
/// // We asserted it holds for the given input `S`ource
/// assert_infallible_recoverable::<&[u8], while0::While0<fn(&u8) -> bool, &[u8]>>();
/// ```
///
/// The same cannot be asserted of the [`tag()`](crate::scan::tag())-combinator:
/// ```compile_fail
/// use ast_toolkit_snack::asserts::assert_infallible_recoverable;
/// use ast_toolkit_snack::scan::tag;
///
/// // Triggers a compilation error!
/// assert_infallible_recoverable::<&[u8], tag::Tag<u8, &[u8]>>();
/// ```
#[inline]
pub const fn assert_infallible_recoverable<'c, 's, S: ?Sized + Source, C: Combinator<'c, 's, S, Recoverable = Infallible>>() {}

/// Asserts statically that your given input combinator cannot fail recoverably.
///
/// This is useful if you are making assumptions later about e.g. unwrapping the [`Result`]
/// returned by it.
///
/// If you cannot, or don't want to, instantiate your combinator to assert it, use
/// [`assert_infallible_recoverable()`] instead to manually name the type.
///
/// # Generics
/// - `'c`: Any lifetime upon which the `C`ombinator depends.
/// - `'s`: A lifetime for which the `S`ource is valid.
/// - `S`: The [`Source`] which may be parsed by your `C`ombinator.
/// - `C`: The type of your combinator to check.
///
/// # Arguments
/// - `comb`: An instance of your combinator by which we will derive the generics.
///
/// # Examples
/// We can assert that the [`while0`](crate::scan::while0())-combinator never fails:
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::asserts::assert_infallible_recoverable_value;
/// use ast_toolkit_snack::scan::while0;
/// use ast_toolkit_span::Span;
///
/// // We asserted it holds for the given instantiation
/// let mut comb = while0("anything", |_| true);
/// assert_infallible_recoverable_value(&comb);
///
/// // Then we do some parsing with it (helps to deduce 'S')
/// assert!(comb.parse(Span::new("Hello, world")).is_ok());
/// ```
///
/// The same cannot be asserted of the [`tag()`](crate::scan::tag())-combinator:
/// ```compile_fail
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::asserts::assert_infallible_recoverable_value;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_span::Span;
///
/// // Triggers a compilation error!
/// let mut comb = tag(b"hello");
/// assert_infallible_recoverable_value(&comb);
///
/// // Then we do some parsing with it (helps to deduce 'S')
/// assert!(comb.parse(Span::new("Hello, world")).is_ok());
/// ```
#[inline]
pub const fn assert_infallible_recoverable_value<'c, 's, S: ?Sized + Source, C: Combinator<'c, 's, S, Recoverable = Infallible>>(_comb: &C) {}



/// Asserts statically that your given input combinator cannot fail fatally.
///
/// This is useful if you are making assumptions later about e.g. unwrapping the [`Result`]
/// returned by it.
///
/// If you cannot, or don't want to, name the type, see [`assert_infallible_fatal_value()`] instead to
/// give an instantiated combinator.
///
/// # Generics
/// - `'c`: Any lifetime upon which the `C`ombinator depends.
/// - `'s`: A lifetime for which the `S`ource is valid.
/// - `S`: The [`Source`] which may be parsed by your `C`ombinator.
/// - `C`: The type of your combinator to check.
///
/// # Examples
/// We can assert that the [`tag()`](crate::scan::tag())-combinator never fails:
/// ```rust
/// use ast_toolkit_snack::asserts::assert_infallible_fatal;
/// use ast_toolkit_snack::scan::tag;
///
/// // We asserted it holds for the given input `S`ource
/// assert_infallible_fatal::<&[u8], tag::Tag<u8, &[u8]>>();
/// ```
///
/// The same cannot be asserted of the =
/// [`separated_many0()`](crate::multi::separated_many0())-combinator:
/// ```compile_fail
/// use ast_toolkit_snack::asserts::assert_infallible_fatal;
/// use ast_toolkit_snack::multi::separated_many0;
/// use ast_toolkit_snack::scan::tag;
///
/// // Triggers a compilation error!
/// assert_infallible_fatal::<
///     &[u8],
///     separated_many0::SeparatedMany0<tag::Tag<u8, &[u8]>, tag::Tag<u8, &[u8]>, &[u8]>,
/// >();
/// ```
#[inline]
pub const fn assert_infallible_fatal<'c, 's, S: ?Sized + Source, C: Combinator<'c, 's, S, Fatal = Infallible>>() {}

/// Asserts statically that your given input combinator cannot fail fatally.
///
/// This is useful if you are making assumptions later about e.g. unwrapping the [`Result`]
/// returned by it.
///
/// If you cannot, or don't want to, instantiate your combinator to assert it, use
/// [`assert_infallible_fatal()`] instead to manually name the type.
///
/// # Generics
/// - `'c`: Any lifetime upon which the `C`ombinator depends.
/// - `'s`: A lifetime for which the `S`ource is valid.
/// - `S`: The [`Source`] which may be parsed by your `C`ombinator.
/// - `C`: The type of your combinator to check.
///
/// # Arguments
/// - `comb`: An instance of your combinator by which we will derive the generics.
///
/// # Examples
/// We can assert that the [`tag()`](crate::scan::tag())-combinator never fails:
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::asserts::assert_infallible_fatal_value;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_span::Span;
///
/// // We asserted it holds for the given instantiation
/// let mut comb = tag(b"Hello");
/// assert_infallible_fatal_value(&comb);
///
/// // Then we do some parsing with it (helps to deduce 'S')
/// assert!(comb.parse(Span::new("Hello, world")).is_ok());
/// ```
///
/// The same cannot be asserted of the
/// [`separated_many0()`](crate::multi::separated_many0())-combinator:
/// ```compile_fail
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::asserts::assert_infallible_fatal_value;
/// use ast_toolkit_snack::multi::separated_many0;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_span::Span;
///
/// // Triggers a compilation error!
/// let mut comb = separated_many0(tag(b"hello"), tag(b","));
/// assert_infallible_fatal_value(&comb);
///
/// // Then we do some parsing with it (helps to deduce 'S')
/// assert!(comb.parse(Span::new("Hello, world")).is_ok());
/// ```
#[inline]
pub const fn assert_infallible_fatal_value<'c, 's, S: ?Sized + Source, C: Combinator<'c, 's, S, Fatal = Infallible>>(_comb: &C) {}
