//  BRANCH.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 11:40:17
//  Last edited:
//    01 May 2024, 17:47:36
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements branching combinators. Actually just [`alt()`].
//

use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;
use stackvec::StackVec;

use crate::error::{Common, Failure};
use crate::{Combinator, Expects, ExpectsFormatter, Result};


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::super::utf8::complete::tag;
    use super::*;

    type Span = ast_toolkit_span::Span<&'static str, &'static str>;

    #[test]
    fn test_alt() {
        // Some success stories
        let input: Span = Span::new("<test>", "Hello, world!");
        let (rem, res) = alt((tag(&"Hello"), tag(&"Goodbye"))).parse(input).unwrap();
        assert_eq!(rem, input.slice(5..));
        assert_eq!(res, input.slice(..5));
        let input: Span = Span::new("<test>", "Hello, world!");
        let (rem, res) = alt((tag(&"Goodbye"), tag(&"Hello"))).parse(input).unwrap();
        assert_eq!(rem, input.slice(5..));
        assert_eq!(res, input.slice(..5));

        // Failure
        assert!(alt((tag(&"Goodbye"), tag(&"Extra goodbye"))).parse(input).is_fail());
    }
}





/***** HELPER MACROS *****/
/// Counts how many identifiers are given.
macro_rules! count {
    (replace $_name:ident) => (());

    ($($i:ident)*) => (
        <[()]>::len(&[ $(count!(replace $i)),* ])
    );
}

/// Given a tuple, produces an Expects-string formatter for it.
macro_rules! tuple_branchable_expected_impl {
    // It's the _only_, not the last
    (start: $self:ident, $f:ident, $indent:ident => $i:tt) => {
        $self.fmts.$i.expects_fmt($f, $indent)?;
    };
    // It's more than one
    (start: $self:ident, $f:ident, $indent:ident => $fi:tt $(, $i:tt)+) => {
        $self.fmts.$fi.expects_fmt($f, $indent)?;
        tuple_branchable_expected_impl!($self, $f, $indent => $($i),+);
    };

    // Deal with the pre-last one
    ($self:ident, $f:ident, $indent:ident => $i:tt $(, $rem:tt)+) => {
        write!($f, ", ")?;
        $self.fmts.$i.expects_fmt($f, $indent)?;
        tuple_branchable_expected_impl!($self, $f, $indent => $($rem),+);
    };
    // Deal with the last one
    ($self:ident, $f:ident, $indent:ident => $i:tt) => {
        write!($f, " or ")?;
        $self.fmts.$i.expects_fmt($f, $indent)?;
    };
}

/// Implements [`Combinator`] for a particular tuple for us.
macro_rules! tuple_branchable_impl {
    // Non-empty tuple implementation
    (impl => $li:tt : ($fi:tt, $fname:ident) $(, ($i:tt, $name:ident))*) => {
        paste::paste! {
            /// Formats the expects-string for a tuple of a particular size
            #[derive(Debug)]
            pub struct [< Tuple $li Formatter >]<$fname $(, $name)*> {
                /// The formatters for all nested combinators.
                fmts: ($fname, $($name,)*),
            }
            impl<$fname: ExpectsFormatter $(, $name: ExpectsFormatter)*> Display for [< Tuple $li Formatter >]<$fname $(, $name)*> {
                #[inline]
                fn fmt(&self, f: &mut Formatter) -> FResult {
                    write!(f, "Expected ")?;
                    self.expects_fmt(f, 0)
                }
            }
            impl<$fname: ExpectsFormatter $(, $name: ExpectsFormatter)*> ExpectsFormatter for [< Tuple $li Formatter >]<$fname $(, $name)*> {
                #[inline]
                fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
                    write!(f, "either ")?;
                    tuple_branchable_expected_impl!(start: self, f, indent => $fi $(, $i)*);
                    Ok(())
                }
            }

            // Then implement Branchable for the tuple
            paste::paste!(
                impl<'t, F: Clone, S: Clone, O, $fname: Combinator<'t, F, S, Output = O> $(, $name: Combinator<'t, F, S, Output = O>)*> Branchable<'t, F, S> for ($fname, $($name,)*) {
                    type Formatter = [< Tuple $li Formatter >]<$fname::Formatter $(, $name::Formatter)*>;
                    type Output = O;

                    #[inline]
                    fn expects(&self) -> Self::Formatter {
                        [< Tuple $li Formatter >] {
                            fmts: (self.$fi.expects(), $(self.$i.expects(),)*),
                        }
                    }

                    #[inline]
                    fn branch(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S> {
                        let mut fails: StackVec<{ count!($fname $($name)*) }, Common<'t, F, S>> = StackVec::new();
                        match self.$fi.parse(input.clone()) {
                            Result::Ok(rem, res) => return Result::Ok(rem, res),
                            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                            Result::Fail(fail) => fails.push(fail.try_into().unwrap()),
                            Result::Error(err) => return Result::Error(err),
                        }
                        $(match self.$i.parse(input.clone()) {
                            Result::Ok(rem, res) => return Result::Ok(rem, res),
                            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
                            Result::Fail(fail) => fails.push(fail.try_into().unwrap()),
                            Result::Error(err) => return Result::Error(err),
                        })*
                        Result::Fail(Failure::Common(Common::Alt { branches: fails.into(), span: input }))
                    }
                }
            );
        }
    };

    // Done with reversing the arguments; call us forwardly
    ($li:tt: reverse: $(($i:tt, $name:ident)),*) => {
        tuple_branchable_impl!(impl => $li: $(($i, $name)),*);
    };
    // More to reverse
    ($li:tt: ($fi:tt, $fname:ident) $(, ($i:tt, $name:ident))* reverse: $(($ri:tt, $rname:ident)),*) => {
        tuple_branchable_impl!($li: $(($i, $name)),* reverse: ($fi, $fname) $(, ($ri, $rname))*);
    };
}

/// Implements [`Combinator`] for various sizes of tuples for us.
macro_rules! tuple_branchable_impls {
    // Base case; empty tuple implementation (we don't do that here)
    (impl:) => {};
    // "Forward" run of all the arguments once reversed
    (impl: ($fi:tt, $fname:ident) $(, ($i:tt, $name:ident))*) => {
        // Build the smaller edition first
        tuple_branchable_impls!(impl: $(($i, $name)),*);

        // Then implement it
        tuple_branchable_impl!($fi: ($fi, $fname) $(, ($i, $name))* reverse:);
    };

    // More to reverse
    (($fi:tt, $fname:ident) $(, ($i:tt, $name:ident))* reverse: $(($ri:tt, $rname:ident)),*) => {
        tuple_branchable_impls!($(($i, $name)),* reverse: ($fi, $fname) $(, ($ri, $rname))*);
    };
    // Done with reversing the arguments; call us forwardly
    (reverse: $(($i:tt, $name:ident)),*) => {
        tuple_branchable_impls!(impl: $(($i, $name)),*);
    };

    // Calling interface
    ($(($i:tt, $name:ident)),*) => {
        tuple_branchable_impls!($(($i, $name)),* reverse:);
    }
}

// Default impls for tuples
tuple_branchable_impls!(
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





/***** AUXILLARY *****/
/// Abstracts over types that can be [`branch()`]ed over. Most notably, these are tuples.
pub trait Branchable<'t, F, S> {
    /// The formatter for the branched combinators.
    type Formatter: 't + ExpectsFormatter;
    /// The output of all the branched combinators.
    type Output;


    /// Returns an [`ExpectsFormatter`] that does the actual formatting.
    ///
    /// This [`Formatter`] may implement [`Display`] to show a fully-fledged error string.
    ///
    /// # Returns
    /// A [`Self::Formatter`](Expects::Formatter) that can be used to create the expects string.
    fn expects(&self) -> Self::Formatter;

    /// Runs all combinators in this Branchable.
    ///
    /// # Arguments
    /// - `input`: The input to run it with.
    ///
    /// # Returns
    /// A [`Result::Ok`] with the result of the _first_ combinator that returned OK.
    ///
    /// # Fails
    /// This function returns [`Result::Fail`] with [`Failure::Branch`] if _all_ internal combinators failed.
    ///
    /// # Errors
    /// This function returns [`Result:Error`] if _any_ of the internal combinators failed.
    fn branch(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S>;
}





/***** LIBRARY FUNCTIONS *****/
/// Tries different possible combinators and returns the first one that succeeds.
///
/// The combinators are tried in-order. They must all returns the same result (so use enums if you want to have options).
///
/// # Arguments
/// - `branches`: Some [`Branchable`] type that can be given as input. Tuples up to size 16 implement this.
///
/// # Returns
/// A combinator [`Alt`] that will run the given branches and try them one-by-one.
///
/// # Fails
/// This function may fail if _all_ combinators in `branches` fails. In that case, all of the failures are collected in a [`Failure::Branch`].
///
/// # Errors
/// This function errors if _any_ of the branches errors. This is then returned as-is.
pub fn alt<'c, F, S, B>(branches: B) -> Alt<F, S, B>
where
    F: Clone,
    S: Clone,
    B: Branchable<'c, F, S>,
{
    Alt { branches, _f: Default::default(), _s: Default::default() }
}





/***** LIBRARY *****/
/// The concrete type returned by [`alt()`].
pub struct Alt<F, S, B> {
    branches: B,
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<'t, F, S, B> Expects<'t> for Alt<F, S, B>
where
    B: Branchable<'t, F, S>,
{
    type Formatter = B::Formatter;

    #[inline]
    fn expects(&self) -> Self::Formatter { self.branches.expects() }
}
impl<'c, F, S, B> Combinator<'c, F, S> for Alt<F, S, B>
where
    F: Clone,
    S: Clone,
    B: Branchable<'c, F, S>,
{
    type Output = B::Output;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> { self.branches.branch(input) }
}
