//  TUPLE.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:05:30
//  Last edited:
//    07 Mar 2025, 14:45:17
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`tuple()`]-combinator.
//

use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::mem::MaybeUninit;

use ast_toolkit_span::{Span, Spanning};

use crate::result::SnackError;
use crate::{Combinator, ExpectsFormatter};


/***** IMPLEMENTATIONS *****/
/// Implements [`Combinator`] for a particular tuple size for us.
macro_rules! tuple_comb_impl {
    // Non-empty tuple implementation
    (impl => $li:tt : $fi:tt $(, $i:tt)*) => {
        ::paste::paste! {
            /* ERRORS */
            #[doc = concat!("The recoverable error returned by a tuple of ", stringify!($li), " combinators.")]
            pub type [<Recoverable $li >]<[<E $fi>] $(, [<E $i>])*> = [<Error $li >]<[<E $fi>] $(, [<E $i>])*>;

            #[doc = concat!("The fatal error returned by a tuple of ", stringify!($li), " combinators.")]
            pub type [<Fatal $li>]<[<E $fi>] $(, [<E $i>])*> = [<Error $li>]<[<E $fi>] $(, [<E $i>])*>;

            #[doc = concat!("The recoverable/fatal error returned by a tuple of ", stringify!($li), " combinators.")]
            #[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
            pub enum [<Error $li>]<[<E $fi>] $(, [<E $i>])*> {
                #[doc = concat!("Combinator ", stringify!($fi), " failed.")]
                [<Comb $fi>]([<E $fi>]),
                $(
                    #[doc = concat!("Combinator ", stringify!($i), " failed.")]
                    [<Comb $i>]([<E $i>]),
                )*
            }
            impl<[<E $fi>]: Display $(, [<E $i>]: Display)*> Display for [<Error $li>]<[<E $fi>] $(, [<E $i>])*> {
                #[inline]
                fn fmt(&self, f: &mut Formatter) -> FResult {
                    match self {
                        Self::[<Comb $fi>](err) => write!(f, "{err}"),
                        $(Self::[<Comb $i>](err) => write!(f, "{err}"),)*
                    }
                }
            }
            impl<[<E $fi>]: 'static + Error $(, [<E $i>]: 'static + Error)*> Error for [<Error $li>]<[<E $fi>] $(, [<E $i>])*> {
                #[inline]
                fn source(&self) -> Option<&(dyn 'static + Error)> {
                    match self {
                        Self::[<Comb $fi>](err) => Some(err),
                        $(Self::[<Comb $i>](err) => Some(err),)*
                    }
                }
            }
            impl<F: Clone, S: Clone, [<E $fi>]: Spanning<F, S> $(, [<E $i>]: Spanning<F, S>)*> Spanning<F, S> for [<Error $li>]<[<E $fi>] $(, [<E $i>])*> {
                #[inline]
                fn span(&self) -> Span<F, S> {
                    match self {
                        Self::[<Comb $fi>](err) => err.span(),
                        $(Self::[<Comb $i>](err) => err.span(),)*
                    }
                }

                #[inline]
                fn into_span(self) -> Span<F, S> {
                    match self {
                        Self::[<Comb $fi>](err) => err.into_span(),
                        $(Self::[<Comb $i>](err) => err.into_span(),)*
                    }
                }
            }



            /* FORMATTERS */
            #[doc = concat!("Expects formatter for the ", stringify!([<Tuple $li>]), " combinator.")]
            #[derive(Debug, Eq, PartialEq)]
            pub struct [<ExpectsFormatter $li>]<[<F $fi>] $(, [<F $i>])*> {
                /// The internal formatters for every combinator making up the tuple.
                pub fmts: ([<F $fi>], $([<F $i>]),*),
            }
            impl<[<F $fi>]: ExpectsFormatter $(, [<F $i>]: ExpectsFormatter)*> Display for [<ExpectsFormatter $li>]<[<F $fi>] $(, [<F $i>])*> {
                #[inline]
                fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
                    write!(f, "Expected ")?;
                    self.expects_fmt(f, 0)
                }
            }
            impl<[<F $fi>]: ExpectsFormatter $(, [<F $i>]: ExpectsFormatter)*> ExpectsFormatter for [<ExpectsFormatter $li>]<[<F $fi>] $(, [<F $i>])*> {
                #[inline]
                fn expects_fmt(&self, f: &mut Formatter<'_>, indent: usize) -> FResult {
                    self.fmts.$fi.expects_fmt(f, indent)?;
                    $(
                        write!(f, ", then ")?;
                        self.fmts.$i.expects_fmt(f, indent)?;
                    )*
                    Ok(())
                }
            }



            /* COMBINATORS */
            impl<'t, [<C $fi>] $(, [<C $i>])*, F, S> Combinator<'t, F, S> for ([<C $fi>], $([<C $i>],)*)
            where
                [<C $fi>]: Combinator<'t, F, S>,
                $([<C $i>]: Combinator<'t, F, S>,)*
            {
                type ExpectsFormatter = [<ExpectsFormatter $li>]<[<C $fi>]::ExpectsFormatter $(, [<C $i>]::ExpectsFormatter)*>;
                type Output = ([<C $fi>]::Output, $([<C $i>]::Output),*);
                type Recoverable = [<Error $li>]<[<C $fi>]::Recoverable $(, [<C $i>]::Recoverable)*>;
                type Fatal = [<Error $li>]<[<C $fi>]::Fatal $(, [<C $i>]::Fatal)*>;

                #[inline]
                fn expects(&self) -> Self::ExpectsFormatter {
                    [<ExpectsFormatter $li>] { fmts: (self.$fi.expects(), $(self.$i.expects(),)*) }
                }

                fn parse(&mut self, input: Span<F, S>) -> Result<(Span<F, S>, Self::Output), SnackError<Self::Recoverable, Self::Fatal, F, S>> {
                    // We collect the results as we find them
                    let mut results: (MaybeUninit<[<C $fi>]::Output>, $(MaybeUninit<[<C $i>]::Output>,)*) = (MaybeUninit::<[<C $fi>]::Output>::uninit(), $(MaybeUninit::<[<C $i>]::Output>::uninit(),)*);
                    let rem: Span<F, S> = match self.$fi.parse(input) {
                        Ok((rem, res)) => {
                            results.$fi.write(res);
                            rem
                        },
                        Err(SnackError::Recoverable(err)) => return Err(SnackError::Recoverable([<Error $li>]::[<Comb $fi>](err))),
                        Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal([<Error $li>]::[<Comb $fi>](err))),
                        Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
                    };
                    $(
                        let rem: Span<F, S> = match self.$i.parse(rem) {
                            Ok((rem, res)) => {
                                results.$i.write(res);
                                rem
                            },
                            Err(SnackError::Recoverable(err)) => return Err(SnackError::Recoverable([<Error $li>]::[<Comb $i>](err))),
                            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal([<Error $li>]::[<Comb $i>](err))),
                            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
                        };
                    )*

                    // Assume all of them are OK before returning
                    // SAFETY: All are initialized above.
                    Ok((rem, (
                        unsafe { results.$fi.assume_init() },
                        $( unsafe { results.$i.assume_init() }, )*
                    )))
                }
            }
        }
    };

    // Done with reversing the arguments; call us forwardly
    ($li:tt: reverse: $($i:tt),*) => {
        tuple_comb_impl!(impl => $li: $($i),*);
    };
    // More to reverse
    ($li:tt: $fi:tt $(, $i:tt)* reverse: $($ri:tt),*) => {
        tuple_comb_impl!($li: $($i),* reverse: $fi $(, $ri)*);
    };
}

/// Implements [`Combinator`] for various sizes of tuples for us.
macro_rules! tuple_comb_impls {
    // Base case; empty tuple implementation (we don't do that here)
    (impl:) => {};
    // "Forward" run of all the arguments once reversed
    (impl: ($fli:tt, $fi:tt) $(, ($li:tt, $i:tt))*) => {
        // Build the smaller edition first
        tuple_comb_impls!(impl: $(($li, $i)),*);

        // Then implement this length
        tuple_comb_impl!($fli: $fi $(, $i)* reverse:);
    };

    // More to reverse
    (($fli:tt, $fi:tt) $(, ($li:tt, $i:tt))* reverse: $(($rli:tt, $ri:tt)),*) => {
        tuple_comb_impls!($(($li, $i)),* reverse: ($fli, $fi) $(, ($rli, $ri))*);
    };
    // Done with reversing the arguments; call us forwardly
    (reverse: $(($li:tt, $i:tt)),*) => {
        tuple_comb_impls!(impl: $(($li, $i)),*);
    };

    // Calling interface
    ($(($li:tt, $i:tt)),*) => {
        tuple_comb_impls!($(($li, $i)),* reverse:);
    }
}

// Implement `Combinator`
tuple_comb_impls!((1, 0), (2, 1), (3, 2), (4, 3), (5, 4), (6, 5), (7, 6), (8, 7), (9, 8), (10, 9), (11, 10), (12, 11));





/***** LIBRARY *****/
/// Applies a tuple of combinators, in-order.
///
/// This combinator will try the given ones one-by-one, returning all of their results as a tuple.
///
/// # Note
/// Actually, this combinator is really a no-op, and simply returns the given combinator. This is
/// possible because tuples up to a size of 12 already implement [`Combinator`] themselves. If you
/// want larger tuples, consider nesting the last element in another tuple of up to size 12.
///
/// # Arguments
/// - `combs`: The tuple of combinators to apply.
///
/// # Returns
/// The given combinator, which is likely a `TupleN`-combinator for a tuple of size N.
///
/// # Fails
/// The returned combinator fails if the given `combs` fails.
///
/// Note that, for tuples, this short circuits; no combinators beyond the first failing one are
/// called.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::error::{Common, Failure};
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::sequence::tuple;
/// use ast_toolkit_snack::utf8::complete::{digit1, tag};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello123");
/// let span2 = Span::new("<example>", "123");
/// let span3 = Span::new("<example>", "HelloWorld");
///
/// let mut comb = tuple((tag("Hello"), digit1()));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(8..), (span1.slice(..5), span1.slice(5..8)))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tuple::Recoverable2::Comb0(tag::Recoverable {
///         tag:  "Hello",
///         span: span2,
///     })))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(tuple::Recoverable2::Comb1(digit1::Recoverable {
///         fmt:  digit1::ExpectsFormatter,
///         span: span3.slice(5..),
///     })))
/// );
/// ```
#[inline]
pub const fn tuple<C>(combs: C) -> C { combs }
