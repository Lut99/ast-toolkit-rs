//  BRANCH.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 11:40:17
//  Last edited:
//    05 Apr 2024, 13:33:16
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements branching combinators. Actually just [`alt()`].
//

use ast_toolkit_span::Span;
use stackvec::StackVec;

use crate::fail::Failure;
use crate::{Combinator, Result};


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::super::value::tag;
    use super::*;

    type Span = ast_toolkit_span::Span<&'static str, &'static str>;

    #[test]
    fn test_alt() {
        // Some success stories
        let input: Span = Span::new("<test>", "Hello, world!");
        let (rem, res) = alt((tag(&"Hello"), tag(&"Goodbye")))(input).unwrap();
        assert_eq!(rem, input.slice(5..));
        assert_eq!(res, input.slice(..5));
        let input: Span = Span::new("<test>", "Hello, world!");
        let (rem, res) = alt((tag(&"Goodbye"), tag(&"Hello")))(input).unwrap();
        assert_eq!(rem, input.slice(5..));
        assert_eq!(res, input.slice(..5));

        // Failure
        assert_eq!(
            alt((tag(&"Goodbye"), tag(&"Extra goodbye")))(input),
            Result::Fail(Failure::Alt { branches: vec![Failure::Tag { tag: &"Goodbye" }, Failure::Tag { tag: &"Extra goodbye" }] })
        );
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
            Result::Fail(fail) => $fails.push(fail),
            Result::Error(err) => return Result::Error(err),
        }
        // Do any others
        tuple_branchable_impl!(last $self, $input, $fails, $($i),+);
    };
    (last $self:ident, $input:ident, $fails:ident, $fi:tt) => {
        // The last one
        match $self.$fi.parse($input) {
            Result::Ok(rem, res) => return Result::Ok(rem, res),
            Result::Fail(fail) => $fails.push(fail),
            Result::Error(err) => return Result::Error(err),
        }
    };

    (($fi:tt, $fname:ident)) => {
        impl<F, S, $fname: Combinator<F, S>> Branchable<F, S> for ($fname,) {
            type Output = $fname::Output;

            fn branch(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S> {
                match self.$fi.parse(input) {
                    Result::Ok(rem, res) => Result::Ok(rem, res),
                    Result::Fail(fail) => Result::Fail(Failure::Alt { branches: vec![ fail ]}),
                    Result::Error(err) => Result::Error(err),
                }
            }
        }
    };
    (($fi:tt, $fname:ident), $(($i:tt, $name:ident)),+) => {
        impl<F: Clone, S: Clone, R, $fname: Combinator<F, S, Output = R> $(, $name: Combinator<F, S, Output = R>)+> Branchable<F, S> for ($fname, $($name),*) {
            type Output = R;

            fn branch(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S> {
                // Some cheap store for collecting failures
                let mut fails: StackVec<{ count!($fname $($name)+) }, Failure> = StackVec::new();

                // Go over all
                tuple_branchable_impl!(last self, input, fails, $fi, $($i),+);

                // If we made it here, none of them early quit
                Result::Fail(Failure::Alt { branches: fails.into() })
            }
        }
    };
}





/***** AUXILLARY *****/
/// Abstracts over types that can be [`branch()`]ed over. Most notably, these are tuples.
pub trait Branchable<F, S> {
    /// The output of all the branched combinators.
    type Output;


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
    fn branch(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S>;
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





/***** LIBRARY *****/
/// Tries different possible combinators and returns the first one that succeeds.
///
/// The combinators are tried in-order. They must all returns the same result (so use enums if you want to have options).
///
/// # Arguments
/// - `branches`: Some [`Branchable`] type that can be given as input. Tuples up to size 16 implement this.
///
/// # Returns
/// The output of the first combinator in `branches` that succeeds.
///
/// # Fails
/// This function may fail if _all_ combinators in `branches` fails. In that case, all of the failures are collected in a [`Failure::Branch`].
///
/// # Errors
/// This function errors if _any_ of the branches errors. This is then returned as-is.
pub fn alt<F, S, T>(mut branches: T) -> impl FnMut(Span<F, S>) -> Result<T::Output, F, S>
where
    F: Clone,
    S: Clone,
    T: Branchable<F, S>,
{
    move |input: Span<F, S>| -> Result<T::Output, F, S> {
        // Just hit the `T` one
        branches.branch(input)
    }
}