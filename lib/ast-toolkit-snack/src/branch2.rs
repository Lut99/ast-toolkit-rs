//  BRANCH 2.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 17:26:29
//  Last edited:
//    02 Nov 2024, 13:02:03
//  Auto updated?
//    Yes
//
//  Description:
//!   Experimental test for branching using the new error type.
//


use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;
use std::mem::MaybeUninit;

use ast_toolkit_span::{Span, Spanning};

use crate::result::{Error, Result as SResult, SnackError};
use crate::{Combinator2, Expects, ExpectsFormatter};


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
macro_rules! tuple_branchable_impl {
    // Non-empty tuple implementation
    (impl => $li:tt : $fi:tt $(, $i:tt)*) => {
        paste::paste! {
            /***** ERRORS *****/
            #[doc = concat!("The recoverable error returned by an [`alt()`] of ", stringify!($li), " branches.\n\nThis error type contains the recoverable error of every branch, as it only occurs when all branches fail.")]
            #[derive(Debug)]
            pub struct [<Alt $li Recoverable>]<F, S, [<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*> {
                /// The formatter built to describe what we expected.
                pub fmt: [< Alt $li Formatter >]<[<F $fi>] $(, [<F $i>])*>,
                /// The nested failures of all branches.
                pub fails: ([<E $fi>], $([<E $i>]),*),
                /// The span where the failure occurred.
                pub span: Span<F, S>,
            }
            impl<F, S, [<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*> Display for [<Alt $li Recoverable>]<F, S, [<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*>
            where
                [<F $fi>]: ExpectsFormatter,
                $([<F $i>]: ExpectsFormatter,)*
            {
                #[inline]
                fn fmt(&self, f: &mut Formatter) -> FResult { write!(f, "{}", self.fmt) }
            }
            impl<F, S, [<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*> Spanning<F, S> for [<Alt $li Recoverable>]<F, S, [<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*>
            where
                F: Clone,
                S: Clone,
            {
                #[inline]
                fn span(&self) -> Span<F, S> { self.span.clone() }
            }
            impl<F, S, [<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*> Error<F, S> for [<Alt $li Recoverable>]<F, S, [<F $fi>] $(, [<F $i>])*, [<E $fi>] $(, [<E $i>])*>
            where
                F: Clone + Debug,
                S: Clone + Debug,
                [<F $fi>]: ExpectsFormatter,
                $([<F $i>]: ExpectsFormatter,)*
                [<E $fi>]: Error<F, S>,
                $([<E $i>]: Error<F, S>,)*
            {}



            #[doc = concat!("The fatal error returned by an [`alt()`] of ", stringify!($li), " branches.\n\nThis error type only contains the fatal error of the branch that failed.")]
            #[derive(Debug)]
            pub enum [<Alt $li Fatal>]<[<E $fi>] $(, [<E $i>])*> {
                #[doc = concat!("Branch ", stringify!($fi), " has failed.")]
                [<Branch $fi>]([<E $fi>]),
                $(
                    #[doc = concat!("Branch ", stringify!($i), " has failed.")]
                    [<Branch $i>]([<E $i>]),
                )*
            }
            impl<[<E $fi>]: Display $(, [<E $i>]: Display)*> Display for [<Alt $li Fatal>]<[<E $fi>] $(, [<E $i>])*> {
                #[inline]
                fn fmt(&self, f: &mut Formatter) -> FResult {
                    match self {
                        Self::[<Branch $fi>](err) => write!(f, "{err}"),
                        $(Self::[<Branch $i>](err) => write!(f, "{err}"),)*
                    }
                }
            }
            impl<F, S, [<E $fi>]: Spanning<F, S> $(, [<E $i>]: Spanning<F, S>)*> Spanning<F, S> for [<Alt $li Fatal>]<[<E $fi>] $(, [<E $i>])*> {
                #[inline]
                fn span(&self) -> Span<F, S> {
                    match self {
                        Self::[<Branch $fi>](err) => err.span(),
                        $(Self::[<Branch $i>](err) => err.span(),)*
                    }
                }
            }
            impl<F, S, [<E $fi>]: Error<F, S> $(, [<E $i>]: Error<F, S>)*> Error<F, S> for [<Alt $li Fatal>]<[<E $fi>] $(, [<E $i>])*> {
                #[inline]
                fn source(&self) -> Option<&dyn Error<F, S>> {
                    match self {
                        Self::[<Branch $fi>](err) => Some(err),
                        $(Self::[<Branch $i>](err) => Some(err),)*
                    }
                }
            }





            /***** FORMATTER *****/
            /// Formats the expects-string for a tuple of a particular size
            #[derive(Clone, Debug)]
            pub struct [< Alt $li Formatter >]<[<F $fi>] $(, [<F $i>])*> {
                /// The formatters for all nested combinators.
                pub(crate) fmts: ([<F $fi>], $([<F $i>],)*),
            }
            impl<[<F $fi>]: ExpectsFormatter $(, [<F $i>]: ExpectsFormatter)*> Display for [< Alt $li Formatter >]<[<F $fi>] $(, [<F $i>])*> {
                #[inline]
                fn fmt(&self, f: &mut Formatter) -> FResult {
                    write!(f, "Expected ")?;
                    self.expects_fmt(f, 0)
                }
            }
            impl<[<F $fi>]: ExpectsFormatter $(, [<F $i>]: ExpectsFormatter)*> ExpectsFormatter for [< Alt $li Formatter >]<[<F $fi>] $(, [<F $i>])*> {
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
                impl<'t, F: Clone, S: Clone, O, [<C $fi>]: Combinator2<'t, F, S, Output = O> $(, [<C $i>]: Combinator2<'t, F, S, Output = O>)*> Branchable<'t, F, S> for ([<C $fi>], $([<C $i>],)*) {
                    type Formatter = [< Alt $li Formatter >]<[<C $fi>]::Formatter $(, [<C $i>]::Formatter)*>;
                    type Output = O;
                    type Recoverable = [<Alt $li Recoverable>]<F, S, [<C $fi>]::Formatter $(, [<C $i>]::Formatter)*, [<C $fi>]::Recoverable $(, [<C $i>]::Recoverable)*>;
                    type Fatal = [<Alt $li Fatal>]<[<C $fi>]::Fatal $(, [<C $i>]::Fatal)*>;

                    #[inline]
                    fn expects(&self) -> Self::Formatter {
                        [< Alt $li Formatter >] {
                            fmts: (self.$fi.expects(), $(self.$i.expects(),)*),
                        }
                    }

                    #[inline]
                    fn branch(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
                        // We attempt to parse first, collecting errors as they occur
                        let mut fails: (MaybeUninit<[<C $fi>]::Recoverable>, $(MaybeUninit<[<C $i>]::Recoverable>),*) = (MaybeUninit::<[<C $fi>]::Recoverable>::uninit(), $(MaybeUninit::<[<C $i>]::Recoverable>::uninit()),*);
                        match self.$fi.parse(input.clone()) {
                            Ok((rem, res)) => return Ok((rem, res)),
                            Err(SnackError::Recoverable(err)) => {
                                fails.$fi.write(err);
                            },
                            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal([<Alt $li Fatal>]::[<Branch $fi>](err))),
                            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
                        }
                        $(match self.$i.parse(input.clone()) {
                            Ok((rem, res)) => return Ok((rem, res)),
                            Err(SnackError::Recoverable(err)) => {
                                fails.$i.write(err);
                            },
                            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal([<Alt $li Fatal>]::[<Branch $i>](err))),
                            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
                        })*

                        // If we're here, all branches failed. So we can assume that their error is initialized...
                        // SAFETY: All branches are initialized above.
                        let fails: ([<C $fi>]::Recoverable, $([<C $i>]::Recoverable),*) = unsafe {(
                            fails.$fi.assume_init(),
                            $(fails.$i.assume_init(),)*
                        )};

                        // ...and then return it
                        Err(SnackError::Recoverable([<Alt $li Recoverable>] { fmt: [< Alt $li Formatter >] { fmts: (self.$fi.expects(), $(self.$i.expects()),*) }, fails, span: input }))
                    }
                }
            );
        }
    };

    // Done with reversing the arguments; call us forwardly
    ($li:tt: reverse: $($i:tt),*) => {
        tuple_branchable_impl!(impl => $li: $($i),*);
    };
    // More to reverse
    ($li:tt: $fi:tt $(, $i:tt)* reverse: $($ri:tt),*) => {
        tuple_branchable_impl!($li: $($i),* reverse: $fi $(, $ri)*);
    };
}

/// Implements [`Combinator`] for various sizes of tuples for us.
macro_rules! tuple_branchable_impls {
    // Base case; empty tuple implementation (we don't do that here)
    (impl:) => {};
    // "Forward" run of all the arguments once reversed
    (impl: ($fli:tt, $fi:tt) $(, ($li:tt, $i:tt))*) => {
        // Build the smaller edition first
        tuple_branchable_impls!(impl: $(($li, $i)),*);

        // Then implement this length
        tuple_branchable_impl!($fli: $fi $(, $i)* reverse:);
    };

    // More to reverse
    (($fli:tt, $fi:tt) $(, ($li:tt, $i:tt))* reverse: $(($rli:tt, $ri:tt)),*) => {
        tuple_branchable_impls!($(($li, $i)),* reverse: ($fli, $fi) $(, ($rli, $ri))*);
    };
    // Done with reversing the arguments; call us forwardly
    (reverse: $(($li:tt, $i:tt)),*) => {
        tuple_branchable_impls!(impl: $(($li, $i)),*);
    };

    // Calling interface
    ($(($li:tt, $i:tt)),*) => {
        tuple_branchable_impls!($(($li, $i)),* reverse:);
    }
}





/***** AUXILLARY *****/
/// Abstracts over types that can be [`branch()`]ed over. Most notably, these are tuples.
pub trait Branchable<'t, F, S> {
    /// The formatter for the branched combinators combined.
    type Formatter: 't + ExpectsFormatter;
    /// The output type shared by all branched combinators.
    type Output;
    /// The custom recoverable error type of all branched combinators combined.
    type Recoverable;
    /// The custom fatal error type of all branched combinators combined.
    type Fatal;


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
    /// This function returns [`Result:Error`] if _any_ of the internal combinators failed./// The output type for this Combinator.
    fn branch(&mut self, input: Span<F, S>) -> crate::result::Result<F, S, Self::Output, Self::Recoverable, Self::Fatal>;
}

// Default impls for tuples
tuple_branchable_impls!((1, 0), (2, 1), (3, 2), (4, 3), (5, 4), (6, 5), (7, 6), (8, 7), (9, 8), (10, 9), (11, 10), (12, 11));





/***** COMBINATORS *****/
/// Actual implementation of [`alt()`].
pub struct Alt<B, F, S> {
    branches: B,
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<'t, B: Branchable<'t, F, S>, F, S> Expects<'t> for Alt<B, F, S> {
    type Formatter = B::Formatter;

    #[inline]
    fn expects(&self) -> Self::Formatter { self.branches.expects() }
}
impl<'t, B: Branchable<'t, F, S>, F, S> Combinator2<'t, F, S> for Alt<B, F, S> {
    type Output = B::Output;
    type Recoverable = B::Recoverable;
    type Fatal = B::Fatal;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> { self.branches.branch(input) }
}





/***** LIBRARY *****/

/// Tries different possible combinators and returns the first one that succeeds.
///
/// The combinators are tried in-order. They must all return the same result (so use enums if you want to have options).
///
/// # Arguments
/// - `branches`: Some [`Branchable`] type that can be given as input. Tuples up to and including size 12 implement this.
///
/// # Returns
/// A combinator [`Alt`] that will run the given branches and try them one-by-one.
///
/// # Fails
/// The returned combinator may fail if _all_ combinators in `branches` fails.
///
/// # Examples
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::branch2::{Alt2Recoverable, alt};
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
/// let span3 = Span::new("<example>", "World!");
///
/// let mut comb = alt((tag("Hello"), tag("Goodbye")));
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
/// assert_eq!(comb.parse(span2).unwrap(), (span2.slice(7..), span2.slice(..7)));
/// assert!(matches!(comb.parse(span3), Err(SnackError::Recoverable(Alt2Recoverable { .. }))));
/// ```
#[inline]
pub const fn alt<B, F, S>(branches: B) -> Alt<B, F, S> { Alt { branches, _f: PhantomData, _s: PhantomData } }
