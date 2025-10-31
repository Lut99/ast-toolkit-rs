//  SPEC.rs
//    by Lut99
//
//  Description:
//!   A place where the main interfaces of the library live.
//

use std::borrow::Cow;
use std::cell::{Ref, RefMut};
use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};

use ast_toolkit_loc::{Loc, Located};

use crate::auxillary::CutError;
use crate::slice::{Slice, Source};


/***** HELPER MACROS *****/
/// Implements pointer-like implementations for [`ExpectsFormatter`].
macro_rules! expects_formatter_ptr_impl {
    ('a,Cow < 'a,T >) => {
        impl<'a, T: ?Sized + ExpectsFormatter + ToOwned> ExpectsFormatter for Cow<'a, T>
        where
            T::Owned: Debug + Display,
        {
            #[inline]
            fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult { <T as ExpectsFormatter>::expects_fmt(self, f, indent) }
        }
    };
    ('a, $ty:ty) => {
        impl<'a, T: ?Sized + ExpectsFormatter> ExpectsFormatter for $ty {
            #[inline]
            fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult { <T as ExpectsFormatter>::expects_fmt(self, f, indent) }
        }
    };
    ($ty:ty) => {
        impl<T: ?Sized + ExpectsFormatter> ExpectsFormatter for $ty {
            #[inline]
            fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult { <T as ExpectsFormatter>::expects_fmt(self, f, indent) }
        }
    };
}

/// Implements pointer-like implementations for [`Combinator`].
macro_rules! combinator_ptr_impl {
    /* Private interface */
    (__BODY) => {
        type ExpectsFormatter = <T as Combinator<'c, S>>::ExpectsFormatter;
        type Output = <T as Combinator<'c, S>>::Output;
        type Recoverable = <T as Combinator<'c, S>>::Recoverable;
        type Fatal = <T as Combinator<'c, S>>::Fatal;

        #[inline]
        fn expects(&self) -> Self::ExpectsFormatter { <T as Combinator<'c, S>>::expects(self) }

        #[inline]
        fn parse<'s>(&mut self, input: Slice<'s, S>) -> Result<'s, Self::Output, Self::Recoverable, Self::Fatal, S> {
            <T as Combinator<'c, S>>::parse(self, input)
        }
    };

    /* Public interface */
    ('a, $ty:ty) => {
        impl<'c, 'a, S: Source, T: Combinator<'c, S>> Combinator<'c, S> for $ty {
            combinator_ptr_impl!(__BODY);
        }
    };
    ($ty:ty) => {
        impl<'c, S: Source, T: Combinator<'c, S>> Combinator<'c, S> for $ty {
            combinator_ptr_impl!(__BODY);
        }
    };
}

/// Implements pointer-like implementations for [`BranchingCombinator`].
macro_rules! branching_combinator_ptr_impl {
    /* Private interface */
    (__BODY) => {
        type ExpectsFormatter = <T as BranchingCombinator<'c, S>>::ExpectsFormatter;
        type Output = <T as BranchingCombinator<'c, S>>::Output;
        type Recoverable = <T as BranchingCombinator<'c, S>>::Recoverable;
        type Fatal = <T as BranchingCombinator<'c, S>>::Fatal;

        #[inline]
        fn expects(&self) -> Self::ExpectsFormatter { <T as BranchingCombinator<'c, S>>::expects(self) }

        #[inline]
        fn parse<'s>(&mut self, input: Slice<'s, S>) -> Result<'s, Self::Output, Self::Recoverable, Self::Fatal, S> {
            <T as BranchingCombinator<'c, S>>::parse(self, input)
        }
    };

    /* Public interface */
    ('a, $ty:ty) => {
        impl<'c, 'a, S: Source, T: BranchingCombinator<'c, S>> BranchingCombinator<'c, S> for $ty {
            branching_combinator_ptr_impl!(__BODY);
        }
    };
    ($ty:ty) => {
        impl<'c, S: Source, T: BranchingCombinator<'c, S>> BranchingCombinator<'c, S> for $ty {
            branching_combinator_ptr_impl!(__BODY);
        }
    };
}





/***** TYPE ALIASES *****/
/// The return type of all snack [`Combinator`]s.
///
/// It is essentially a three-way return type but separated in two levels to use the stock
/// [`Result`](std::result::Result) (so that `?` works).
pub type Result<'s, T, E1, E2, S> = std::result::Result<(Slice<'s, S>, T), SnackError<E1, E2>>;





/***** ERROR TYPES *****/
/// The main snack error type.
///
/// Snack, like nom, returns three possible errors:
/// 1. Recoverable errors, which means that [branches](crate::branch::alt) can continue searching;
/// 2. Fatal errors, which means that branches should stop searching (the branch was correct but
///    the input is malformed); and
/// 3. Not enough input, which is only relevant for streaming versions of combinators. In this
///    case, it signals that the branch _looks_ incorrect/incomplete, but that additional things
///    can be given after the current end-of-file that collapses the correctness one way or another.
#[derive(Debug, Eq, PartialEq)]
pub enum SnackError<E1, E2> {
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
}
impl<E1, E2> SnackError<E1, E2> {
    /// "Cuts" this SnackError.
    ///
    /// This is a shortcut for using the [`cut()`](crate::error::cut())-combinator. In essence, it
    /// will turn all [recoverable](SnackError::Recoverable) errors into
    /// [fatal](SnackError::Fatal), ones, "cutting" the branching search.
    ///
    /// # Returns
    /// A [`SnackError::Fatal`] over a [`CutError`].
    #[inline]
    pub fn cut(self) -> SnackError<Infallible, CutError<E1, E2>> {
        match self {
            Self::Recoverable(err) => SnackError::Fatal(CutError::Recoverable(err)),
            Self::Fatal(err) => SnackError::Fatal(CutError::Fatal(err)),
        }
    }

    /// "Uncuts" this SnackError.
    ///
    /// This is a shortcut for using the [`uncut()`](crate::error::uncut())-combinator. In essence,
    /// it will turn all [fatal](SnackError::Fatal) errors into
    /// [recoverable](SnackError::Recoverable) ones, "catching" the fatal error and instead
    /// allowing parent branches to be searched again.
    ///
    /// # Returns
    /// A [`SnackError::Recoverable`] over a [`CutError`].
    #[inline]
    pub fn uncut(self) -> SnackError<CutError<E1, E2>, Infallible> {
        match self {
            Self::Recoverable(err) => SnackError::Recoverable(CutError::Recoverable(err)),
            Self::Fatal(err) => SnackError::Recoverable(CutError::Fatal(err)),
        }
    }



    /// Maps the recoverable error in this SnackError to another one.
    ///
    /// # Arguments
    /// - `map`: Some [`FnOnce`] that does the mapping.
    ///
    /// # Returns
    /// A SnackError that, if it was a [`SnackError::Recoverable`], has the result of the `map`
    /// instead of the original error. If it was anything else, it is untouched.
    #[inline]
    pub fn map_recoverable<E>(self, map: impl FnOnce(E1) -> E) -> SnackError<E, E2> {
        match self {
            Self::Recoverable(err) => SnackError::Recoverable(map(err)),
            Self::Fatal(err) => SnackError::Fatal(err),
        }
    }

    /// Maps the fatal error in this SnackError to another one.
    ///
    /// # Arguments
    /// - `map`: Some [`FnOnce`] that does the mapping.
    ///
    /// # Returns
    /// A SnackError that, if it was a [`SnackError::Fatal`], has the result of the `map`
    /// instead of the original error. If it was anything else, it is untouched.
    #[inline]
    pub fn map_fatal<E>(self, map: impl FnOnce(E2) -> E) -> SnackError<E1, E> {
        match self {
            Self::Recoverable(err) => SnackError::Recoverable(err),
            Self::Fatal(err) => SnackError::Fatal(map(err)),
        }
    }
}
// impl<E1, E2> SnackError<E1, E2> {
//     /// Boxes the two errors in the SnackError.
//     ///
//     /// This is different from calling [`BoxableParseError::boxed()`] on the SnackError, as that
//     /// boxes the error as a whole, whereas this function boxes its innards.
//     ///
//     /// # Returns
//     /// An identical SnackError that has box `E1` and `E2` replaced with [`BoxedParseError`]s.
//     #[inline]
//     pub fn into_boxed<'e1, 'e2, S>(self) -> SnackError<BoxedParseError<'e1, S>, BoxedParseError<'e2, S>>
//     where
//         E1: 'e1 + BoxableParseError<S>,
//         E2: 'e2 + BoxableParseError<S>,
//         S: Clone,
//     {
//         match self {
//             Self::Recoverable(err) => SnackError::Recoverable(err.boxed()),
//             Self::Fatal(err) => SnackError::Fatal(err.boxed()),
//         }
//     }
// }
impl<E1: Display, E2: Display> Display for SnackError<E1, E2> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Recoverable(err) => err.fmt(f),
            Self::Fatal(err) => err.fmt(f),
        }
    }
}
impl<E1: Error, E2: Error> Error for SnackError<E1, E2> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Recoverable(err) => err.source(),
            Self::Fatal(err) => err.source(),
        }
    }
}
impl<E1: Located, E2: Located> Located for SnackError<E1, E2> {
    #[inline]
    fn loc(&self) -> Loc {
        match self {
            Self::Recoverable(err) => err.loc(),
            Self::Fatal(err) => err.loc(),
        }
    }
}
impl<E1: ParseError, E2: ParseError> ParseError for SnackError<E1, E2> {
    #[inline]
    fn more_might_fix(&self) -> bool {
        match self {
            Self::Recoverable(err) => err.more_might_fix(),
            Self::Fatal(err) => err.more_might_fix(),
        }
    }

    #[inline]
    fn needed_to_fix(&self) -> Option<usize> {
        match self {
            Self::Recoverable(err) => err.needed_to_fix(),
            Self::Fatal(err) => err.needed_to_fix(),
        }
    }
}





/***** INTERFACES *****/
/// A trait implemented by errors that are returned by snack [`Combinator`]s.
pub trait ParseError: Error + Located {
    /// This function tells if more input might fix this error.
    ///
    /// This is very useful for streaming input. If you see a
    /// ([recoverable](SnackError::Recoverable)) error occur that might be fixed with more
    /// input, it means that the error occurred because an unexpected end-of-file was encountered,
    /// but otherwise this input was promising. Hence, if you request more input from the stream,
    /// this still might be a valid prefix.
    ///
    /// See [`ParseError::needed_to_fix()`] to obtain an estimate for how many bytes you should
    /// stream before trying again.
    ///
    /// # Default implementation
    /// By default, this function returns that no additional bytes might fix this error.
    ///
    /// # Returns
    /// True if more input might turns this frown upside down (fix the error). False if this is not
    /// the case (we explicitly detected something illegal for this combinator).
    fn more_might_fix(&self) -> bool;

    /// If [more input might fix this error](ParseError::more_might_fix()) (i.e., it was because we
    /// reached an unexpected end-of-file), this function returns an estimate for how many elements
    /// are needed _at minimum_ to do so.
    ///
    /// For example, for [`tag`](bytes::tag)s, this function returns the size of the part of the
    /// tag that was out-of-bounds for the input. For a greedy, repetitive combinator, this might
    /// return the number of bytes needed to finish the current iteration.
    ///
    /// # Default implementation
    /// By default, this function returns that no estimate exists.
    ///
    /// # Returns
    /// The estimated **minimum** number of elements needed to potentially fix this error (it might
    /// just be something else after all). It can return [`None`] if
    /// [`ParseError::more_might_fix()`] is false or, if it is, if there is no such estimate
    /// available.
    fn needed_to_fix(&self) -> Option<usize>;
}

// Std impls
impl ParseError for Infallible {
    #[inline]
    fn more_might_fix(&self) -> bool { false }

    #[inline]
    fn needed_to_fix(&self) -> Option<usize> { None }
}



/// A trait implemented by things returned from [`Combinator::expects()`]s.
///
/// This trait actually produces expect-strings.
pub trait ExpectsFormatter: Debug + Display {
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
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult;
}

// Default impls for string-like types
impl ExpectsFormatter for str {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        // If it begins with `Expected`, cut that off
        if self.starts_with("Expected ") { <str as Display>::fmt(&self[9..], f) } else { <str as Display>::fmt(self, f) }
    }
}
impl ExpectsFormatter for String {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        // If it begins with `Expected`, cut that off
        if self.starts_with("Expected ") { <str as Display>::fmt(&self[9..], f) } else { <str as Display>::fmt(self, f) }
    }
}

// Default impls for pointer-like types
expects_formatter_ptr_impl!('a, &'a T);
expects_formatter_ptr_impl!('a, &'a mut T);
expects_formatter_ptr_impl!('a, Cow<'a, T>);
expects_formatter_ptr_impl!(Box<T>);
expects_formatter_ptr_impl!(Rc<T>);
expects_formatter_ptr_impl!(Arc<T>);
expects_formatter_ptr_impl!('a, Ref<'a, T>);
expects_formatter_ptr_impl!('a, RefMut<'a, T>);
expects_formatter_ptr_impl!('a, MutexGuard<'a, T>);
expects_formatter_ptr_impl!('a, RwLockReadGuard<'a, T>);
expects_formatter_ptr_impl!('a, RwLockWriteGuard<'a, T>);



/// A trait that unifies all snack combinators.
///
/// A combinator is a stateful parser that parses a small part of an AST. By composing them, one
/// can create modular parsers that are efficient and logical to reason over. Moreover, because
/// the combinator tree tends to represent an AST, friendly error messages can be generated by
/// expressing what each combinator expects.
///
/// This is the standard combinator trait. For combinators that do multiple other combinators in
/// succession (e.g., a tuple), this trait encodes conjuction, i.e., all of the parsers must
/// succeed and their results are all returned. By contrast, the [`BranchingCombinator`]-trait
/// encodes disjunction, i.e., the parsers are tried in-order and the first to succeed becomes the
/// result of parsing. In that case, all errors are collected instead.
///
/// # Generics
/// - `'c`: Some lifetime of something upon which the _combinator_ depends. For example, this is
///   the lifetime `'c` in the `&'c str` given as input to a [`tag()`](utf8::complete::tag)-
///   combinator. This is used to delay serialization of the arguments to the last moment.
/// - `S`: Some [`Source`]-string that will be parsed by this Combinator. This is what is
///   effectively parsed.
pub trait Combinator<'c, S: Source> {
    /// The type that is in charge of generating the expects-string.
    type ExpectsFormatter: ExpectsFormatter;
    /// The output type for this Combinator.
    type Output;
    /// Some error type that is thrown when the combinator fails but in a recoverable way.
    ///
    /// This means that any wrapping [`alt()`](branch::alt()) should still try another branch.
    type Recoverable: ParseError;
    /// Some error type that is thrown when the combinator fails unrecoverably.
    ///
    /// This means that there is no point for any wrapping [`alt()`](branch::alt()) to still try
    /// another branch.
    type Fatal: ParseError;


    /// Returns some [`ExpectsFormatter`] that can write a string describing what
    /// input this combinator expects.
    ///
    /// It typically implements [`Display`] to show a fully-fledged error string.
    ///
    /// # Returns
    /// A [`Combinator::ExpectsFormatter`] that can be used to create the expects string.
    fn expects(&self) -> Self::ExpectsFormatter;

    /// Runs the combinator on a [`Span`] of input.
    ///
    /// # Arguments
    /// - `input`: The input to parse.
    ///
    /// # Returns
    /// A pair of the remaining input left unparsed (as a [`Span`]) and something of type
    /// [`Combinator::Output`] that encodes the result of this parser.
    ///
    /// # Errors
    /// The parse function should error if it failed to parse anything. It has three ways of doing
    /// so:
    /// - It can emit a [`SnackError::Recoverable`](crate::result::SnackError::Recoverable), which
    ///   returns something of [`Combinator::Recoverable`] and encodes that any wrapping
    ///   [`alt()`](branch::alt()) should still try another branch;
    /// - It can emit a [`SnackError::Fatal`](crate::result::SnackError::Fatal), which returns
    ///   something of [`Combinator::Fatal`] and encodes that there is no point for any wrapping
    ///   [`alt()`](branch::alt()) to still try another branch.
    /// - It can emit a [`SnackError::NotEnough`](crate::result::SnackError::NotEnough), which
    ///   encodes that the input *may* become parsable if additional input is given. This is useful
    ///   when streaming the input from e.g. stdin or a socket, and one retrieves input in chunks
    ///   that aren't necessarily valid.
    ///
    /// # Examples
    /// For examples, look at any of the combinators that are shipped with the Snack library.
    fn parse<'s>(&mut self, input: Slice<'s, S>) -> Result<'s, Self::Output, Self::Recoverable, Self::Fatal, S>;
}

// Ptr-like impls
combinator_ptr_impl!('a, &'a mut T);
combinator_ptr_impl!(Box<T>);
combinator_ptr_impl!('a, RefMut<'a, T>);
combinator_ptr_impl!('a, MutexGuard<'a, T>);
combinator_ptr_impl!('a, RwLockWriteGuard<'a, T>);



/// A trait that unifies snack combinators that are trying different parsing paths. This in
/// contrast to the [`Combinator`], which assumes that combinators are doing all paths
/// sequentially (see it for more information).
///
/// You typically don't have to interact with this trait; it is hidden behind
/// [`alt()`](branch::alt()). However, you can implement it manually for your combinators in order
/// to extend the parser combinator library with powerful schemes for trying different ways of
/// parsing.
///
/// # Generics
/// - `'c`: Some lifetime of something upon which the combinator depends. Typically, this is used
///   to make the [`Combinator::ExpectsFormatter`] depend on it too and efficiently delay
///   serialization of the expects-string until the last moment.
/// - `S`: Some [`Source`]-string that will be parsed by this BranchingCombinator. This is what is
///   effectively parsed.
pub trait BranchingCombinator<'c, S>
where
    S: Source,
{
    /// The type that is in charge of generating the expects-string.
    type ExpectsFormatter: ExpectsFormatter;
    /// The output type for all paths of this Combinator.
    type Output;
    /// Some error type that is thrown when a combinator fails but in a recoverable way.
    ///
    /// This means that any wrapping [`alt()`](branch::alt()) should still try another branch.
    type Recoverable: ParseError;
    /// Some error type that is thrown when a combinator fails unrecoverably.
    ///
    /// This means that there is no point for any wrapping [`alt()`](branch::alt()) to still try
    /// another branch.
    type Fatal: ParseError;


    /// Returns some [`ExpectsFormatter`] that can write a string describing what
    /// input this combinator expects.
    ///
    /// It typically implements [`Display`] to show a fully-fledged error string.
    ///
    /// # Returns
    /// A [`BranchingCombinator::ExpectsFormatter`] that can be used to create the expects string.
    fn expects(&self) -> Self::ExpectsFormatter;

    /// Runs the combinator on a [`Span`] of input.
    ///
    /// # Arguments
    /// - `input`: The input to parse.
    ///
    /// # Returns
    /// A pair of the remaining input left unparsed (as a [`Span`]) and something of type
    /// [`BranchingCombinator::Output`] that encodes the result of this parser.
    ///
    /// # Errors
    /// The parse function should error if it failed to parse anything. It has three ways of doing
    /// so:
    /// - It can emit a [`SnackError::Recoverable`](crate::result::SnackError::Recoverable), which
    ///   returns something of [`BranchingCombinator::Recoverable`] and encodes that any wrapping
    ///   [`alt()`](branch::alt()) should still try another branch;
    /// - It can emit a [`SnackError::Fatal`](crate::result::SnackError::Fatal), which returns
    ///   something of [`BranchingCombinator::Fatal`] and encodes that there is no point for any
    ///   wrapping [`alt()`](branch::alt()) to still try another branch.
    /// - It can emit a [`SnackError::NotEnough`](crate::result::SnackError::NotEnough), which
    ///   encodes that the input *may* become parsable if additional input is given. This is useful
    ///   when streaming the input from e.g. stdin or a socket, and one retrieves input in chunks
    ///   that aren't necessarily valid.
    ///
    /// # Examples
    /// For examples, look at any of the combinators that are shipped with the Snack library.
    fn parse<'s>(&mut self, input: Slice<'s, S>) -> Result<'s, Self::Output, Self::Recoverable, Self::Fatal, S>;
}

// Ptr-like impls
branching_combinator_ptr_impl!('a, &'a mut T);
branching_combinator_ptr_impl!(Box<T>);
branching_combinator_ptr_impl!('a, RefMut<'a, T>);
branching_combinator_ptr_impl!('a, MutexGuard<'a, T>);
branching_combinator_ptr_impl!('a, RwLockWriteGuard<'a, T>);
