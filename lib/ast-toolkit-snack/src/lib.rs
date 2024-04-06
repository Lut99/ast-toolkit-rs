//  LIB.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:37:24
//  Last edited:
//    06 Apr 2024, 13:05:02
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
            fn plain_fmt(&self, f: &mut Formatter) -> FResult {
                write!(f, "a sequence of ")?;
                $(if $i > 0 { write!(f, ", ")?; }; self.$i.plain_fmt(f)?;)*
                Ok(())
            }

            fn context_fmt(&self, f: &mut Formatter, context: Option<&dyn Expects>) -> FResult {
                // There never would be any further details, so just hit it with this
                write!(f, "Expected ")?;
                self.plain_fmt(f)?;
                writeln!(f)?;
                writeln!(f)
            }

            fn details(&self) -> Option<&dyn Expects> { None }
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





/***** FORMATTERS *****/
/// Allows something implementing [`Expects`] to be nicely formatted.
pub struct ExpectsFormatter<'e, 'd> {
    exp:     &'e dyn Expects,
    details: Option<&'d dyn Expects>,
}
impl<'e, 'd> Display for ExpectsFormatter<'e, 'd> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self.details {
            // If there's any details, format it with ourselves as context
            Some(details) => details.context_fmt(f, Some(self.exp)),
            // Else, just format plainly
            None => self.exp.plain_fmt(f),
        }
    }
}





/***** LIBRARY *****/
/// A trait that allows something (probably a combinator) to express what it expect(s|ed) as input.
///
/// The expects string should be returned by the parent type's [`Display`]-implementation.
pub trait Expects {
    /// Formats this Expects plainly, just writing what it itself expects.
    ///
    /// The string written should be something along the lines of filling in `XXX` in:
    /// ```plain
    /// Expected XXX
    /// ```
    ///
    /// # Arguments
    /// - `f`: Some [`Formatter`] to write to.
    ///
    /// # Errors
    /// This function should only error if it failed to write to the given `f`ormatter.
    fn plain_fmt(&self, f: &mut Formatter) -> FResult;

    /// Formats this Expects with additional context, writing a more complete Expects-experience.
    ///
    /// The string written should be something along the lines of:
    /// ```plain
    /// Expected something.
    ///
    /// This happened while parsing:
    /// - Something else;
    /// - Something yet entirely different;
    /// - Something super duper different.
    /// ```
    ///
    /// # Arguments
    /// - `f`: Some [`Formatter`] to write to.
    /// - `context`: An optional additional [`Expects`] that should be given as context. If [`None`], then this is the leaf node of the stack.
    ///
    /// # Errors
    /// This function should only error if it failed to write to the given `f`ormatter.
    fn context_fmt(&self, f: &mut Formatter, context: Option<&dyn Expects>) -> FResult;


    /// Returns some more specified expects string than this.
    ///
    /// You could say that `self` is the context of this more specific expects.
    ///
    /// # Returns
    /// Some Expects if there is a more specified one, or else [`None`].
    fn details(&self) -> Option<&dyn Expects>;
}

/// Extends [`Expects`] to make it nicer to work with.
pub trait ExpectsExt: Expects {
    /// Returns a formatter that only writes what this Expects expects, no context.
    ///
    /// # Returns
    /// An [`ExpectsFormatter`] implementing [`Display`].
    fn expects<'e>(&'e self) -> ExpectsFormatter<'e, 'static>;

    /// Returns a formatter that writes what this Expects expects, with context.
    ///
    /// # Returns
    /// An [`ExpectsFormatter`] implementing [`Display`].
    fn expects_with_context(&self) -> ExpectsFormatter;
}
impl<T: Expects> ExpectsExt for T {
    fn expects<'e>(&'e self) -> ExpectsFormatter<'e, 'static> { ExpectsFormatter { exp: self, details: None } }

    fn expects_with_context<'e>(&'e self) -> ExpectsFormatter<'e, 'e> { ExpectsFormatter { exp: self, details: self.details() } }
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
