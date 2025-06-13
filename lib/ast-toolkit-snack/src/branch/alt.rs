//  ALT.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 17:26:29
//  Last edited:
//    08 May 2025, 11:14:52
//  Auto updated?
//    Yes
//
//  Description:
//!   Experimental test for branching using the new error type.
//

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;
use std::mem::MaybeUninit;

use ast_toolkit_span::{Span, Spannable, Spanning};

use crate::result::{Result as SResult, SnackError};
use crate::{BranchingCombinator, Combinator, ExpectsFormatter, ParseError};


/***** HELPER MACROS *****/
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
macro_rules! tuple_branching_comb_impl {
    // Non-empty tuple implementation
    (impl => $li:tt : $fi:tt $(, $i:tt)*) => {
        paste::paste! {
            /***** ERRORS *****/
            #[doc = concat!("The recoverable error returned by an [`alt()`] of ", stringify!($li), " branches.\n\nThis error type contains the recoverable error of every branch, as it only occurs when all branches fail.")]
            #[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
            #[better_derive(
                impl_gen = <'s, [<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*, S>,
                bound = ([<F $fi>]: r#trait $(, [<F $i>]: r#trait)*, [<E $fi>]: r#trait $(, [<E $i>]: r#trait)*, S: Spannable<'s>),
            )]
            pub struct [<Recoverable $li>]<[<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*, S> {
                /// The formatter built to describe what we expected.
                pub fmt: [<ExpectsFormatter $li>]<[<F $fi>] $(, [<F $i>])*>,
                /// The nested failures of all branches.
                pub fails: ([<E $fi>], $([<E $i>]),*),
                /// The span where the failure occurred.
                pub span: Span<S>,
            }
            impl<[<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*, S> Display for [<Recoverable $li>]<[<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*, S>
            where
                [<F $fi>]: ExpectsFormatter,
                $([<F $i>]: ExpectsFormatter,)*
            {
                #[inline]
                fn fmt(&self, f: &mut Formatter) -> FResult { write!(f, "{}", self.fmt) }
            }
            impl<'s, [<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*, S: Spannable<'s>> Error for [<Recoverable $li>]<[<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*, S>
            where
                [<F $fi>]: ExpectsFormatter,
                $([<F $i>]: ExpectsFormatter,)*
                [<E $fi>]: Debug,
                $([<E $i>]: Debug,)*
            {}
            impl<[<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*, S> Spanning<S> for [<Recoverable $li>]<[<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*, S>
            where
                S: Clone,
            {
                #[inline]
                fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(&self.span) }
                #[inline]
                fn into_span(self) -> Span<S> { self.span }
            }
            impl<'s, [<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*, S> ParseError<S> for [<Recoverable $li>]<[<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*, S>
            where
                [<F $fi>]: ExpectsFormatter,
                $([<F $i>]: ExpectsFormatter,)*
                [<E $fi>]: ParseError<S>,
                $([<E $i>]: ParseError<S>,)*
                S: Clone + Spannable<'s>,
            {
                /// This error is fixable if any of the errors in the branch is.
                #[inline]
                fn more_might_fix(&self) -> bool {
                    if self.fails.$fi.more_might_fix() { return true; }
                    $(if self.fails.$i.more_might_fix() { return true; })*
                    false
                }

                /// This will return the minimum number across all of the branches.
                #[inline]
                fn needed_to_fix(&self) -> Option<usize> {
                    let mut res = None;
                    if let Some(needed) = self.fails.$fi.needed_to_fix() { match res { Some(needed2) => res = Some(std::cmp::min(needed, needed2)), None => res = Some(needed), } }
                    $(if let Some(needed) = self.fails.$i.needed_to_fix() { match res { Some(needed2) => res = Some(std::cmp::min(needed, needed2)), None => res = Some(needed), } })*
                    res
                }
            }



            #[doc = concat!("The fatal error returned by an [`alt()`] of ", stringify!($li), " branches.\n\nThis error type only contains the fatal error of the branch that failed.")]
            #[derive(Debug, Eq, PartialEq)]
            pub enum [<Fatal $li>]<[<E $fi>] $(, [<E $i>])*> {
                #[doc = concat!("Branch ", stringify!($fi), " has failed.")]
                [<Branch $fi>]([<E $fi>]),
                $(
                    #[doc = concat!("Branch ", stringify!($i), " has failed.")]
                    [<Branch $i>]([<E $i>]),
                )*
            }
            impl<[<E $fi>]: Display $(, [<E $i>]: Display)*> Display for [<Fatal $li>]<[<E $fi>] $(, [<E $i>])*> {
                #[inline]
                fn fmt(&self, f: &mut Formatter) -> FResult {
                    match self {
                        Self::[<Branch $fi>](err) => err.fmt(f),
                        $(Self::[<Branch $i>](err) => err.fmt(f),)*
                    }
                }
            }
            impl<[<E $fi>]: Spanning<S> $(, [<E $i>]: Spanning<S>)*, S: Clone> Spanning<S> for [<Fatal $li>]<[<E $fi>] $(, [<E $i>])*> {
                #[inline]
                fn span(&self) -> Cow<Span<S>> {
                    match self {
                        Self::[<Branch $fi>](err) => err.span(),
                        $(Self::[<Branch $i>](err) => err.span(),)*
                    }
                }
                #[inline]
                fn into_span(self) -> Span<S> {
                    match self {
                        Self::[<Branch $fi>](err) => err.into_span(),
                        $(Self::[<Branch $i>](err) => err.into_span(),)*
                    }
                }
            }
            impl<[<E $fi>]: Error $(, [<E $i>]: Error)*> Error for [<Fatal $li>]<[<E $fi>] $(, [<E $i>])*> {
                #[inline]
                fn source(&self) -> Option<&(dyn 'static + Error)> {
                    match self {
                        Self::[<Branch $fi>](err) => err.source(),
                        $(Self::[<Branch $i>](err) => err.source(),)*
                    }
                }
            }
            impl<'s, [<E $fi>] $(, [<E $i>])*, S> ParseError<S> for [<Fatal $li>]<[<E $fi>] $(, [<E $i>])*>
            where
                [<E $fi>]: ParseError<S>,
                $([<E $i>]: ParseError<S>,)*
                S: Clone,
            {
                /// This error is fixable if the internal error is.
                #[inline]
                fn more_might_fix(&self) -> bool {
                    match self {
                        Self::[<Branch $fi>](err) => err.more_might_fix(),
                        $(Self::[<Branch $i>](err) => err.more_might_fix(),)*
                    }
                }

                #[inline]
                fn needed_to_fix(&self) -> Option<usize> {
                    match self {
                        Self::[<Branch $fi>](err) => err.needed_to_fix(),
                        $(Self::[<Branch $i>](err) => err.needed_to_fix(),)*
                    }
                }
            }





            /***** FORMATTER *****/
            /// Formats the expects-string for a tuple of a particular size
            #[derive(Clone, Debug, Eq, PartialEq)]
            pub struct [<ExpectsFormatter $li>]<[<F $fi>] $(, [<F $i>])*> {
                /// The formatters for all nested combinators.
                pub fmts: ([<F $fi>], $([<F $i>],)*),
            }
            impl<[<F $fi>]: ExpectsFormatter $(, [<F $i>]: ExpectsFormatter)*> Display for [<ExpectsFormatter $li>]<[<F $fi>] $(, [<F $i>])*> {
                #[inline]
                fn fmt(&self, f: &mut Formatter) -> FResult {
                    write!(f, "Expected ")?;
                    self.expects_fmt(f, 0)
                }
            }
            impl<[<F $fi>]: ExpectsFormatter $(, [<F $i>]: ExpectsFormatter)*> ExpectsFormatter for [<ExpectsFormatter $li>]<[<F $fi>] $(, [<F $i>])*> {
                #[inline]
                fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
                    write!(f, "either ")?;
                    tuple_branchable_expected_impl!(start: self, f, indent => $fi $(, $i)*);
                    Ok(())
                }
            }





            /***** IMPL *****/
            // Then implement Branchable for the tuple
            paste::paste!(
                impl<'c, 's, S, O, [<C $fi>]: Combinator<'c, 's, S, Output = O> $(, [<C $i>]: Combinator<'c, 's, S, Output = O>)*> BranchingCombinator<'c, 's, S> for ([<C $fi>], $([<C $i>],)*)
                where
                    S: Clone + Spannable<'s>,
                {
                    type ExpectsFormatter = [<ExpectsFormatter $li>]<[<C $fi>]::ExpectsFormatter $(, [<C $i>]::ExpectsFormatter)*>;
                    type Output = O;
                    type Recoverable = [<Recoverable $li>]<[<C $fi>]::ExpectsFormatter $(, [<C $i>]::ExpectsFormatter)*, [<C $fi>]::Recoverable $(, [<C $i>]::Recoverable)*, S>;
                    type Fatal = [<Fatal $li>]<[<C $fi>]::Fatal $(, [<C $i>]::Fatal)*>;

                    #[inline]
                    fn expects(&self) -> Self::ExpectsFormatter {
                        [<ExpectsFormatter $li>] {
                            fmts: (self.$fi.expects(), $(self.$i.expects(),)*),
                        }
                    }

                    #[inline]
                    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
                        // We attempt to parse first, collecting errors as they occur
                        let mut fails: (MaybeUninit<[<C $fi>]::Recoverable>, $(MaybeUninit<[<C $i>]::Recoverable>),*) = (MaybeUninit::<[<C $fi>]::Recoverable>::uninit(), $(MaybeUninit::<[<C $i>]::Recoverable>::uninit()),*);
                        match self.$fi.parse(input.clone()) {
                            Ok((rem, res)) => return Ok((rem, res)),
                            Err(SnackError::Recoverable(err)) => {
                                fails.$fi.write(err);
                            },
                            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal([<Fatal $li>]::[<Branch $fi>](err))),
                        }
                        $(match self.$i.parse(input.clone()) {
                            Ok((rem, res)) => return Ok((rem, res)),
                            Err(SnackError::Recoverable(err)) => {
                                fails.$i.write(err);
                            },
                            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal([<Fatal $li>]::[<Branch $i>](err))),
                        })*

                        // If we're here, all branches failed. So we can assume that their error is initialized...
                        // SAFETY: All branches are initialized above.
                        let fails: ([<C $fi>]::Recoverable, $([<C $i>]::Recoverable),*) = unsafe {(
                            fails.$fi.assume_init(),
                            $(fails.$i.assume_init(),)*
                        )};

                        // ...and then return it
                        Err(SnackError::Recoverable([<Recoverable $li>] { fmt: [<ExpectsFormatter $li>] { fmts: (self.$fi.expects(), $(self.$i.expects()),*) }, fails, span: input }))
                    }
                }
            );
        }
    };

    // Done with reversing the arguments; call us forwardly
    ($li:tt: reverse: $($i:tt),*) => {
        tuple_branching_comb_impl!(impl => $li: $($i),*);
    };
    // More to reverse
    ($li:tt: $fi:tt $(, $i:tt)* reverse: $($ri:tt),*) => {
        tuple_branching_comb_impl!($li: $($i),* reverse: $fi $(, $ri)*);
    };
}

/// Implements [`Combinator`] for various sizes of tuples for us.
macro_rules! tuple_branching_comb_impls {
    // Base case; empty tuple implementation (we don't do that here)
    (impl:) => {};
    // "Forward" run of all the arguments once reversed
    (impl: ($fli:tt, $fi:tt) $(, ($li:tt, $i:tt))*) => {
        // Build the smaller edition first
        tuple_branching_comb_impls!(impl: $(($li, $i)),*);

        // Then implement this length
        tuple_branching_comb_impl!($fli: $fi $(, $i)* reverse:);
    };

    // More to reverse
    (($fli:tt, $fi:tt) $(, ($li:tt, $i:tt))* reverse: $(($rli:tt, $ri:tt)),*) => {
        tuple_branching_comb_impls!($(($li, $i)),* reverse: ($fli, $fi) $(, ($rli, $ri))*);
    };
    // Done with reversing the arguments; call us forwardly
    (reverse: $(($li:tt, $i:tt)),*) => {
        tuple_branching_comb_impls!(impl: $(($li, $i)),*);
    };

    // Calling interface
    ($(($li:tt, $i:tt)),*) => {
        tuple_branching_comb_impls!($(($li, $i)),* reverse:);
    }
}





/***** COMBINATORS *****/
// Default impls for tuples
tuple_branching_comb_impls!((1, 0), (2, 1), (3, 2), (4, 3), (5, 4), (6, 5), (7, 6), (8, 7), (9, 8), (10, 9), (11, 10), (12, 11));

/// Actual implementation of [`alt()`].
pub struct Alt<B, S> {
    branches: B,
    _s: PhantomData<S>,
}
impl<'c, 's, B, S> Combinator<'c, 's, S> for Alt<B, S>
where
    B: BranchingCombinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = B::ExpectsFormatter;
    type Output = B::Output;
    type Recoverable = B::Recoverable;
    type Fatal = B::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.branches.expects() }
    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> { self.branches.parse(input) }
}





/***** LIBRARY *****/

/// Tries different possible combinators and returns the first one that succeeds.
///
/// The combinators are tried in-order. They must all return the same result (so use enums if you
/// want to have options).
///
/// In essence, all this combinator does is turn something [`BranchingCombinator`] (i.e., join
/// combinators by disjunction) into a [`Combinator`] for compatability with other combinators.
///
/// # Arguments
/// - `branches`: Some [`BranchingCombinator`] type that can be given as input. Tuples up to and
///   including size 12 implement this.
///
/// # Returns
/// A combinator [`Alt`] that will run the given branches and try them one-by-one.
///
/// # Errors
/// The returned combinator may fail recoverably if _all_ combinators in the given `branches` fail
/// recoverably.
///
/// The returned combinator may fail fatally if _any_ combinator in the given `branches` fails
/// fatally. This behaviour short-circuits (i.e., it is guaranteed no combinators after the failing
/// one are called).
///
/// # Examples
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::branch::alt;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
/// let span3 = Span::new("World!");
///
/// let mut comb = alt((tag("Hello"), tag("Goodbye")));
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(7..), span2.slice(..7)));
/// assert!(matches!(comb.parse(span3), Err(SnackError::Recoverable(alt::Recoverable2 { .. }))));
/// ```
#[inline]
pub const fn alt<'c, 's, B, S>(branches: B) -> Alt<B, S>
where
    B: BranchingCombinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    Alt { branches, _s: PhantomData }
}
