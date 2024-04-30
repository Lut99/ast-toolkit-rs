//  BRANCH.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 11:40:17
//  Last edited:
//    30 Apr 2024, 16:41:46
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements branching combinators. Actually just [`alt()`].
//

use std::fmt::{Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning as _};
use stackvec::StackVec;

use crate::error::{Common, Failure};
use crate::{Combinator, Expects, Result};


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

/// Implements [`Branchable`] for a tuple with given number of parameters.
macro_rules! tuple_branchable_impl {
    (last $self:ident, $input:ident, $fails:ident, $fi:tt, $($i:tt),+) => {
        // First, do a non-last one
        match $self.$fi.parse($input.clone()) {
            Result::Ok(rem, res) => return Result::Ok(rem, res),
            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(fail) => $fails.push(fail.try_into().unwrap()),
            Result::Error(err) => return Result::Error(err),
        }
        // Do any others
        tuple_branchable_impl!(last $self, $input, $fails, $($i),+);
    };
    (last $self:ident, $input:ident, $fails:ident, $fi:tt) => {
        // The last one
        match $self.$fi.parse($input.clone()) {
            Result::Ok(rem, res) => return Result::Ok(rem, res),
            Result::Fail(Failure::NotEnough { needed, span }) => return Result::Fail(Failure::NotEnough { needed, span }),
            Result::Fail(fail) => $fails.push(fail.try_into().unwrap()),
            Result::Error(err) => return Result::Error(err),
        }
    };

    (($fi:tt, $fname:ident)) => {
        impl<'c, F: Clone, S: Clone, $fname: Combinator<'c, F, S>> Branchable<'c, F, S> for ($fname,) {
            type Output = $fname::Output;

            fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult { self.0.fmt(f, indent) }

            fn branch(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
                match self.$fi.parse(input) {
                    Result::Ok(rem, res) => Result::Ok(rem, res),
                    Result::Fail(Failure::NotEnough { needed, span }) => Result::Fail(Failure::NotEnough { needed, span }),
                    Result::Fail(fail) => { let span = fail.span(); Result::Fail(Failure::Common(Common::Alt { branches: vec![ fail.try_into().unwrap() ], span })) },
                    Result::Error(err) => Result::Error(err),
                }
            }
        }
    };
    (($fi:tt, $fname:ident), $(($i:tt, $name:ident)),+) => {
        ::paste::paste! {
        impl<'c, F: Clone, S: Clone, R, $fname: Combinator<'c, F, S, Output = R> $(, $name: Combinator<'c, F, S, Output = R>)+> Branchable<'c, F, S> for ($fname, $($name),*) {
            type Output = R;

            #[inline]
            fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
                struct ExpectsIter<'a, $fname $(, $name)+> { count: usize, data: (&'a $fname $(, &'a $name)+) }
                impl<'a, $fname: Expects $(, $name: Expects)+> Iterator for ExpectsIter<'a, $fname $(, $name)+> {
                    type Item = &'a dyn Expects;
                    #[inline]
                    fn next(&mut self) -> Option<Self::Item> {
                        if self.count == $fi {
                            self.count += 1;
                            Some(self.data.$fi)
                        }
                        $(else if self.count == $i {
                            self.count += 1;
                            Some(self.data.$i)
                        })+
                        else {
                            None
                        }
                    }
                }

                expects_alt(f, indent, ExpectsIter { count: 0, data: (&self.$fi $(, &self.$i)+) })
            }

            fn branch(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
                // Some cheap store for collecting failures
                let mut fails: StackVec<{ count!($fname $($name)+) }, Common<F, S>> = StackVec::new();

                // Go over all
                tuple_branchable_impl!(last self, input, fails, $fi, $($i),+);

                // If we made it here, none of them early quit
                Result::Fail(Failure::Common(Common::Alt { branches: fails.into(), span: input.start_onwards() }))
            }
        }
        }
    };
}

// Default [`Branchable`] impls for tuples.
tuple_branchable_impl!((0, C1));
tuple_branchable_impl!((0, C1), (1, C2));
tuple_branchable_impl!((0, C1), (1, C2), (2, C3));
tuple_branchable_impl!((0, C1), (1, C2), (2, C3), (3, C4));
tuple_branchable_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5));
tuple_branchable_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6));
tuple_branchable_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7));
tuple_branchable_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7), (7, C8));
tuple_branchable_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7), (7, C8), (8, C9));
tuple_branchable_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7), (7, C8), (8, C9), (9, C10));
tuple_branchable_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7), (7, C8), (8, C9), (9, C10), (10, C11));
tuple_branchable_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7), (7, C8), (8, C9), (9, C10), (10, C11), (11, C12));
tuple_branchable_impl!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7), (7, C8), (8, C9), (9, C10), (10, C11), (11, C12), (12, C13));
tuple_branchable_impl!(
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
tuple_branchable_impl!(
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
tuple_branchable_impl!(
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





/***** EXPECTS FUNCTIONS *****/
/// Defines what we expect from an [`Alt`](crate::branch::Alt).
///
/// # Arguments
/// - `f`: Some [`Formatter`] to write what we expect to.
/// - `indent`: Some indentation level to apply when writing new lines.
/// - `branches`: Some iterator yielding [`Expects`] for all branches.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
pub(crate) fn expects_alt<'b>(f: &mut Formatter, indent: usize, branches: impl IntoIterator<Item = &'b dyn Expects>) -> FResult {
    writeln!(f, "one of:")?;
    for b in branches {
        write!(f, "{} - ", (0..indent).map(|_| ' ').collect::<String>())?;
        b.fmt(f, indent + 1)?;
        writeln!(f)?;
    }
    writeln!(f)
}





/***** AUXILLARY *****/
/// Abstracts over types that can be [`branch()`]ed over. Most notably, these are tuples.
pub trait Branchable<'c, F, S> {
    /// The output of all the branched combinators.
    type Output;


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
    fn branch(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S>;
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
impl<'c, F, S, B> Expects for Alt<F, S, B>
where
    B: Branchable<'c, F, S>,
{
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult { self.branches.fmt(f, indent) }
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
