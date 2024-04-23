//  LIB.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:37:24
//  Last edited:
//    23 Apr 2024, 17:25:48
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
pub mod branch;
pub mod comb;
pub mod error;
pub mod error_new;
pub mod fail;
pub mod multi;
pub mod sequence;
pub mod span;
pub mod value;

// Imports
use std::fmt::{Display, Formatter, Result as FResult};

use ast_toolkit_span::{Span, Spannable};
use enum_debug::EnumDebug;
use fail::Failure;


/***** HELPER MACROS *****/
/// Implements [`Combinator`] for various sizes of tuples for us.
macro_rules! tuple_combinator_impl {
    (replace $name:ident) => { "{}" };

    ($(($i:tt, $name:ident)),*) => {
        impl<$($name: Expects),*> Expects for ($($name,)*) {
            #[allow(unused_variables)]
            fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
                write!(f, "a sequence of:")?;
                $(
                    write!(f, "{} - ", (0..EXPECTS_INDENT_SIZE * indent).map(|_| ' ').collect::<String>())?;
                    self.$i.fmt(f, indent + 1)?;
                    writeln!(f)?;
                )*
                Ok(())
            }
        }

        impl<F, S, $($name: Combinator<F, S>),*> Combinator<F, S> for ($($name,)*) {
            type Output = ($($name::Output,)*);

            fn parse(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S> {
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

// Default impls for tuples
tuple_combinator_impl!();
tuple_combinator_impl!((0, C1));
tuple_combinator_impl!((0, C1), (1, C2));
tuple_combinator_impl!((0, C1), (1, C2), (2, C3));
tuple_combinator_impl!((0, C1), (1, C2), (2, C3), (3, C4));
tuple_combinator_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5));
tuple_combinator_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6));
tuple_combinator_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7));
tuple_combinator_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7), (7, C8));
tuple_combinator_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7), (7, C8), (8, C9));
tuple_combinator_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7), (7, C8), (8, C9), (9, C10));
tuple_combinator_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7), (7, C8), (8, C9), (9, C10), (10, C11));
tuple_combinator_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7), (7, C8), (8, C9), (9, C10), (10, C11), (11, C12));
tuple_combinator_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7), (7, C8), (8, C9), (9, C10), (10, C11), (11, C12), (12, C13));
tuple_combinator_impl!(
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
    (13, C14)
);
tuple_combinator_impl!(
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
    (14, C15)
);
tuple_combinator_impl!(
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





/***** FORMATTERS *****/
/// Allows something implementing [`Expects`] to be nicely formatted.
pub struct ExpectsFormatter<'e> {
    exp: &'e dyn Expects,
}
impl<'e> Display for ExpectsFormatter<'e> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { self.exp.fmt(f, 0) }
}





/***** LIBRARY *****/
/// A trait that allows something (probably a combinator) to express what it expect(s|ed) as input.
///
/// The expects string should be returned by the parent type's [`Display`]-implementation.
pub trait Expects {
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

/// Extends [`Expects`] to make it nicer to work with.
pub trait ExpectsExt: Expects {
    /// Returns a formatter that only writes what this Expects expects, no context.
    ///
    /// # Returns
    /// An [`ExpectsFormatter`] implementing [`Display`].
    fn expects(&self) -> ExpectsFormatter;
}
impl<T: Expects> ExpectsExt for T {
    fn expects(&self) -> ExpectsFormatter { ExpectsFormatter { exp: self } }
}



/// The trait that unifies ALL snack parser combinators.
pub trait Combinator<F, S>: Expects {
    /// The output type for this Combinator.
    type Output;

    /// Runs the parser on the given input to produce something of output [`Self::Output`](Combinator::Output).
    ///
    /// # Arguments
    /// - `input`: The input to parse.
    ///
    /// # Returns
    /// A snack [`Result`] that encodes:
    /// - [`SnackResult::Ok((rem, output))`]: We parsed an `output` of type `R`, with `rem` left unparsed.
    /// - [`SnackResult::Fail(fail)`]: We failed to parse with reason `fail`, but another parser might still parse it.
    /// - [`SnackResult::Error(err)`]: We failed to parse with reason `err` and we know the input is in an unrecoverable state (e.g., exhausted all branches).
    fn parse(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S>;
}



/// Defines the shape of the output of [`Combinators`].
#[derive(Clone, Debug, EnumDebug)]
pub enum Result<R, F, S> {
    /// An `output` of type `R` was parsed (1), with the remainder left unparsed (0).
    Ok(Span<F, S>, R),
    /// Failed to parse input with the given reason, but recoverably so.
    Fail(fail::Failure<F, S>),
    /// Failed to parse input with the given reason, but unrecoverably so.
    Error(error::Error<F, S>),
}
impl<R, F, S> Result<R, F, S> {
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
impl<R: Eq, F, S: Eq + Spannable> Eq for Result<R, F, S> {}
impl<R: PartialEq, F, S: PartialEq + Spannable> PartialEq for Result<R, F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ok(rem1, res1), Self::Ok(rem2, res2)) => rem1 == rem2 && res1 == res2,
            (Self::Fail(fail1), Self::Fail(fail2)) => fail1 == fail2,
            (Self::Error(err1), Self::Error(err2)) => err1 == err2,

            // Otherwise, different variants, always bad
            _ => false,
        }
    }
}
