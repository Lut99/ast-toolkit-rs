//  LIB.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:37:24
//  Last edited:
//    30 Apr 2024, 17:27:32
//  Auto updated?
//    Yes
//
//  Description:
//!   The sun is coming out, the birds are tweeting... It's that time of
//!   the year again! Lut99 makes another re-code of `nom`!
//!   
//!   Provides a parser-combinator framework heavily inspired by
//!   [nom](https://github.com/rust-bakery/nom), except that it gives up a
//!   little bit of performance over a more human-friendly debug experience.
//

// Declare submodules
// pub mod branch;
// pub mod bytes;
// pub mod combinator;
pub mod error;
// pub mod multi;
// pub mod sequence;
// pub mod span;
// pub mod utf8;

// Imports
use std::fmt::{Debug, Display, Formatter, Result as FResult};

use ast_toolkit_span::Span;
use enum_debug::EnumDebug;
use error::{Error, Failure};


/***** HELPER MACROS *****/
/// Implements [`Combinator`] for a particular size of tuple for us.
macro_rules! tuple_combinator_impl {
    (replace $name:ident) => { "{}" };

    ($(($i:tt, $name:ident)),*) => {
        struct Tuple

        impl<'t $(, $name: Expects<'t>)*> Expects<'t> for ($($name,)*) {
            type Formatter = TupleExpectsFormatter<'t $(, $name::Formatter)*>;


        }

        impl<'c, F, S, $($name: Combinator<'c, F, S>),*> Combinator<'c, F, S> for ($($name,)*) {
            type Output = ($($name::Output,)*);

            fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
                #[allow(unused_mut)]
                let mut rem: Span<F, S> = input;
                let res: ($($name::Output,)*) = ($(
                    match self.$i.parse(rem) {
                        Result::Ok(partial, res) => {
                            rem = partial;
                            res
                        },
                        Result::Fail(f) => return Result::Fail(f),
                        Result::Error(f) => return Result::Error(f),
                    },
                )*);
                Result::Ok(rem, res)
            }
        }
    };
}

/// Given a series, reverses it.
macro_rules! rev {
    (< $fi:ident >) => {
        < $fi >
    };
    (< $fi:ident, $($i:ident),+ >) => {
        < rev!($($i),+), $fi >
    };
}

/// Given a tuple, produces an Expects-string formatter for it.
macro_rules! tuple_combinator_expected_impl {
    // It's the _only_, not the last
    (start: $self:ident, $f:ident, $indent:ident => $i:tt) => {
        $self.fmts.$i.fmt($f, $indent)?;
    };
    // It's more than one
    (start: $self:ident, $f:ident, $indent:ident => $fi:tt $(, $i:tt)+) => {
        $self.fmts.$fi.fmt($f, $indent)?;
        tuple_combinator_expected_impl!($self, $f, $indent => $($i),+);
    };

    // Deal with the last one
    ($self:ident, $f:ident, $indent:ident => $i:tt) => {
        write!($f, ", ")?;
        $self.fmts.$i.fmt($f, $indent)?;
    };
    // Deal with the pre-last one
    ($self:ident, $f:ident, $indent:ident => $i:tt $(, $rem:tt)+) => {
        write!($f, " and then ")?;
        $self.fmts.$i.fmt($f, $indent)?;
    };
}

/// Implements [`Combinator`] for various sizes of tuples for us.
macro_rules! tuple_combinator_impls {
    // Base case; empty tuple implementation (we don't do that here)
    () => {};

    (($fi:tt, $fname:ident) $(, ($i:tt, $name:ident))*) => {
        // Build the smaller edition first
        tuple_combinator_impls!($(($i, $name)),*);

        // Define a Formatter for this edition
        paste::paste! {
            /// Formats the expects-string for a tuple of a particular size
            pub struct [< Tuple $fi Formatter >]<$fname $(, $name)*> {
                /// The formatters for all nested combinators.
                fmts: ($fname, $($name,)*),
            }
            impl<$fname: ExpectsFormatter $(, $name: ExpectsFormatter)*> Display for [< Tuple $fi Formatter >]<$fname $(, $name)*> {
                #[inline]
                fn fmt(&self, f: &mut Formatter) -> FResult {
                    write!(f, "Expected ")?;
                    <Self as ExpectsFormatter>::fmt(self, f, 0)
                }
            }
            impl<$fname: ExpectsFormatter $(, $name: ExpectsFormatter)*> ExpectsFormatter for [< Tuple $fi Formatter >]<$fname $(, $name)*> {
                #[inline]
                fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
                    tuple_combinator_expected_impl!(start: self, f, indent => $fi $(, $i)*);
                    Ok(())
                }
            }

            // Then implement Expects and Combinator for the tuple
            impl<'t, $fname: Expects<'t> $(, $name: Expects<'t>)*> Expects<'t> for ($fname, $($name,)*) {
                type Formatter = [< Tuple $fi Formatter >]<$fname::Formatter $(, $name::Formatter)*>;

                #[inline]
                fn expects(&self) -> Self::Formatter {
                    [< Tuple $fi Formatter >] {
                        fmts: (self.$fi.expects(), $(self.$i.expects(),)*),
                    }
                }
            }
            impl<'t, F, S, $fname: Combinator<'t, F, S> $(, $name: Combinator<'t, F, S>)*> Combinator<'t, F, S> for ($fname, $($name,)*) {
                type Output = ($fname::Output, $($name::Output,)*);

                #[inline]
                fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S> {

                }
            }
        }
    };
}

// Default impls for tuples
tuple_combinator_impls!(
    (0, C1),
    (1, C2),
    (2, C3),
    (3, C4),
    (4, C5),
    (5, C6),
    (6, C7),
    (7, C8),
    (8, C9),
    (9, C10),
    (10, C11),
    (11, C12),
    (12, C13),
    (13, C14),
    (14, C15),
    (15, C16)
);





/***** CONSTANTS *****/
/// Defines the indent size for every level in [`Expects`] formatting.
const EXPECTS_INDENT_SIZE: usize = 4;





/***** LIBRARY *****/
/// A trait that allows something (probably a combinator) to express what it expect(s|ed) as input.
///
/// This actually serves as an interface to an external type that does all the work. This is important in producing [`Failure`]s with lenient lifetimes.
pub trait Expects<'t> {
    /// The type of formatter that actually handles the work.
    type Formatter: 't + ExpectsFormatter;

    /// Returns an [`ExpectsFormatter`] that does the actual formatting.
    ///
    /// This [`Formatter`] may implement [`Display`] to show a fully-fledged error string.
    ///
    /// # Returns
    /// A [`Self::Formatter`](Expects::Formatter) that can be used to create the expects string.
    fn expects(&self) -> Self::Formatter;
}

/// A trait implemented by [`Expects::Formatter`]s.
///
/// This trait actually produces expect-strings.
pub trait ExpectsFormatter {
    /// Formats the thing that this Expects expected as input.
    ///
    /// The string written should be something along the lines of filling in `XXX` in:
    /// ```plain
    /// Expected XXX.
    /// ```
    ///
    /// # Arguments
    /// - `f`: Some [`Formatter`] to write to.
    /// - `indent`: If this formatter writes newlines, they should be indented by this amount.
    ///
    /// # Errors
    /// This function should only error if it failed to write to the given `f`ormatter.
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult;
}



/// The trait that unifies ALL snack parser combinators.
pub trait Combinator<'t, F, S>: Expects<'t> {
    /// The output type for this Combinator.
    type Output;

    /// Runs the parser on the given input to produce something of output [`Self::Output`](Combinator::Output).
    ///
    /// # Arguments
    /// - `input`: The input to parse.
    ///
    /// # Returns
    /// A snack [`Result`] that encodes:
    /// - [`Result::Ok((rem, output))`]: We parsed an `output` of type `R`, with `rem` left unparsed.
    /// - [`Result::Fail(fail)`]: We failed to parse with reason `fail`, but another parser might still parse it.
    /// - [`Result::Error(err)`]: We failed to parse with reason `err` and we know the input is in an unrecoverable state (e.g., exhausted all branches).
    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S>;
}



/// Defines the shape of the output of [`Combinators`].
#[derive(Debug, EnumDebug)]
pub enum Result<'a, R, F, S> {
    /// An `output` of type `R` was parsed (1), with the remainder left unparsed (0).
    Ok(Span<F, S>, R),
    /// Failed to parse input with the given reason, but recoverably so.
    Fail(Failure<'a, F, S>),
    /// Failed to parse input with the given reason, but unrecoverably so.
    Error(Error<'a, F, S>),
}
impl<'a, R, F, S> Result<'a, R, F, S> {
    /// Maps this result's [`Result::Fail`]-case.
    ///
    /// # Arguments
    /// - `fail`: The [`Failure`] to return in case `self` is a [`Result::Fail`].
    ///
    /// # Returns
    /// The given `fail`ure if `self` is a [`Result::Fail`]. Else, `self` is returned as-is.
    #[inline]
    pub fn map_fail(self, map_fn: impl FnOnce(Failure<F, S>) -> Failure<F, S>) -> Self {
        match self {
            Self::Ok(rem, res) => Self::Ok(rem, res),
            Self::Fail(fail) => Self::Fail(map_fn(fail)),
            Self::Error(err) => Self::Error(err),
        }
    }

    /// Unwraps this Result as if it's a [`Result::Ok`].
    ///
    /// # Returns
    /// A tuple with a span describing the unparsed source, and the parsed object, respectively.
    ///
    /// # Panics
    /// This function panics if this Result is not [`Result::Ok`].
    #[inline]
    pub fn unwrap(self) -> (Span<F, S>, R) {
        if let Self::Ok(rem, res) = self {
            (rem, res)
        } else {
            panic!("Cannot unwrap a Result::{} as a Result::Ok", self.variant());
        }
    }

    /// Returns if this Result is [`Result::Ok`].
    ///
    /// # Returns
    /// True if so or false otherwise.
    #[inline]
    pub fn is_ok(&self) -> bool { matches!(self, Self::Ok(_, _)) }

    /// Returns if this Result is [`Result::Fail`].
    ///
    /// # Returns
    /// True if so or false otherwise.
    #[inline]
    pub fn is_fail(&self) -> bool { matches!(self, Self::Fail(_)) }

    /// Returns if this Result is [`Result::Error`].
    ///
    /// # Returns
    /// True if so or false otherwise.
    #[inline]
    pub fn is_err(&self) -> bool { matches!(self, Self::Error(_)) }

    /// Returns if this Result is either [`Result::Fail`] or [`Result::Error`].
    ///
    /// # Returns
    /// True if so or false otherwise.
    #[inline]
    pub fn is_not_ok(&self) -> bool { !self.is_ok() }
}
