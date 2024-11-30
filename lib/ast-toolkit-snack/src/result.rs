//  RESULT.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 16:52:42
//  Last edited:
//    30 Nov 2024, 23:46:38
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines the things returned from [`Combinator`](crate::Combinator)s,
//!   including the error types.
//

use std::fmt::{Debug, Display};

use ast_toolkit_span::{Span, SpannableEq, Spanning};


/***** INTERFACES *****/
/// Something is a snack error.
pub trait Error<F, S>: Debug + Display + Spanning<F, S> {
    /// Returns a more specific error explaining additional details.
    ///
    /// For example: you're trying to parse an integer. The main error may be something indicating
    /// that an integer was failed to be parsed. Then the source of that error may be an error
    /// indicating further this was because the number was too large to fit in the buffer.
    ///
    /// # Returns
    /// A nested [`Error`] if there is a more specific one, or else [`None`].
    #[inline]
    fn source(&self) -> Option<&(dyn Error<F, S>)> { None }
}





/***** LIBRARY *****/
/// The return type of all snack [`Combinator`](crate::Combinator)s.
///
/// It is essentially a three-way return type but separated in two levels to use the stock [`Result`] (so that `?` works).
pub type Result<F, S, T, E1, E2> = std::result::Result<(Span<F, S>, T), SnackError<F, S, E1, E2>>;

/// The main snack error type.
///
/// Snack, like nom, returns three possible errors:
/// 1. Recoverable errors, which means that [branches](crate::branch::alt) can continue searching;
/// 2. Fatal errors, which means that branches should stop searching (the branch was correct but
///    the input is malformed); and
/// 3. Not enough input, which is only relevant for streaming versions of combinators. In this
///    case, it signals that the branch _looks_ incorrect/incomplete, but that additional things
///    can be given after the current end-of-file that collapses the correctness one way or another.
#[derive(Debug)]
pub enum SnackError<F, S, E1, E2> {
    /// It's a recoverable error.
    ///
    /// This means that any [branch::alt](crate::branch::alt) combinator should try another branch
    /// and might still have some luck.
    Recoverable(E1),
    /// It's a non-recoverable error.
    ///
    /// This means that any [branch::alt](crate::branch::alt) combinator should stop searching.
    /// You can interpret this error as "correct branch, but malformed input". An example is a
    /// missing closing parenthesis.
    Fatal(E2),
    /// The parser is not sure whether an error occurred.
    ///
    /// This means that the current branch is _probably_ incorrect or malformed, specifically due
    /// to being incomplete somehow (more was expected). More input may be given to turn this error
    /// into a complete phrase.
    NotEnough {
        /// How much more input should (at least) be given to make it correct - if this is known.
        ///
        /// Note the input is given in _bytes_.
        needed: Option<usize>,
        /// The span pointing to the end of the input stream.
        span:   Span<F, S>,
    },
}
impl<F, S, E1, E2> Eq for SnackError<F, S, E1, E2>
where
    S: SpannableEq,
    E1: Eq,
    E2: Eq,
{
}
impl<F, S, E1, E2> PartialEq for SnackError<F, S, E1, E2>
where
    S: SpannableEq,
    E1: PartialEq,
    E2: PartialEq,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Only match other with the same variant
            (Self::Recoverable(err1), Self::Recoverable(err2)) => err1 == err2,
            (Self::Fatal(err1), Self::Fatal(err2)) => err1 == err2,
            (Self::NotEnough { needed: needed1, span: span1 }, Self::NotEnough { needed: needed2, span: span2 }) => {
                needed1 == needed2 && span1 == span2
            },

            // Anything else never equals
            _ => false,
        }
    }
}
