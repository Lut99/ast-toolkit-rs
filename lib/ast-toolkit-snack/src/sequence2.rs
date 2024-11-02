//  SEQUENCE 2.rs
//    by Lut99
//
//  Created:
//    19 Sep 2024, 13:15:39
//  Last edited:
//    02 Nov 2024, 10:42:42
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements combinators for doing other combinators in sequence.
//

use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;
use std::mem::MaybeUninit;

use ast_toolkit_span::{Span, Spanning};

use crate::result::SnackError;
use crate::utils::{comb_impl, fmt_impl};
use crate::{Combinator2, Expects};


/***** IMPLEMENTATIONS *****/
/// Implements [`Combinator2`] for a particular tuple size for us.
macro_rules! tuple_comb_impl {
    // Non-empty tuple implementation
    (impl => $li:tt : $fi:tt $(, $i:tt)*) => {
        ::paste::paste! {
            #[doc = concat!("The recoverable error returned by a tuple of ", stringify!($li), " combinators.")]
            pub enum [<Tuple $li Error>]<F, S, [<E $fi>] $(, [<E $i>])*> {
                #[doc = concat!("Combinator ", stringify!($fi), " failed.")]
                [<Comb $fi>]{ err: [<E $fi>], span: Span<F, S> },
                $(
                    #[doc = concat!("Combinator ", stringify!($i), " failed.")]
                    [<Comb $i>]{ err: [<E $i>], span: Span<F, S> },
                )*
            }
            impl<F, S, [<E $fi>]: Debug $(, [<E $i>]: Debug)*> Debug for [<Tuple $li Error>]<F, S, [<E $fi>] $(, [<E $i>])*> {
                fn fmt(&self, f: &mut Formatter) -> FResult {
                    match self {
                        Self::[<Comb $fi>] { err, span } => {
                            let mut fmt = f.debug_struct(concat!(stringify!([<Tuple $li Error>]), "::", stringify!([<Comb $fi>])));
                            fmt.field("err", err);
                            fmt.field("span", span);
                            fmt.finish()
                        },
                        $(Self::[<Comb $i>] { err, span } => {
                            let mut fmt = f.debug_struct(concat!(stringify!([<Tuple $li Error>]), "::", stringify!([<Comb $i>])));
                            fmt.field("err", err);
                            fmt.field("span", span);
                            fmt.finish()
                        },)*
                    }
                }
            }
            impl<F, S, [<E $fi>]: Display $(, [<E $i>]: Display)*> Display for [<Tuple $li Error>]<F, S, [<E $fi>] $(, [<E $i>])*> {
                #[inline]
                fn fmt(&self, f: &mut Formatter) -> FResult {
                    match self {
                        Self::[<Comb $fi>] { err, .. } => write!(f, "{err}"),
                        $(Self::[<Comb $i>] { err, .. } => write!(f, "{err}"),)*
                    }
                }
            }
            impl<F, S, [<E $fi>]: 'static + Error $(, [<E $i>]: 'static + Error)*> Error for [<Tuple $li Error>]<F, S, [<E $fi>] $(, [<E $i>])*> {
                #[inline]
                fn source(&self) -> Option<&(dyn 'static + Error)> {
                    match self {
                        Self::[<Comb $fi>] { err, .. } => Some(err),
                        $(Self::[<Comb $i>] { err, .. } => Some(err),)*
                    }
                }
            }
            impl<F: Clone, S: Clone, [<E $fi>] $(, [<E $i>])*> Spanning<F, S> for [<Tuple $li Error>]<F, S, [<E $fi>] $(, [<E $i>])*> {
                #[inline]
                fn span(&self) -> Span<F, S> {
                    match self {
                        Self::[<Comb $fi>] { span, .. } => span.clone(),
                        $(Self::[<Comb $i>] { span, .. } => span.clone(),)*
                    }
                }
            }



            fmt_impl! {
                #[comb = stringify!([<Tuple $li>])]
                gen [<Tuple $li ExpectsFormatter>]<[<F $fi>] $(, [<F $i>])*> {
                    /// The internal formatters for every combinator making up the tuple.
                    fmts: ([<F $fi>], $([<F $i>]),*),
                } impl {
                    fn expects_fmt(&self, f: &mut Formatter, indent: usize)
                    where
                        [<F $fi>]: (crate::ExpectsFormatter),
                        $([<F $i>]: (crate::ExpectsFormatter),)*
                    {
                        self.fmts.$fi.expects_fmt(f, indent)?;
                        $(
                            write!(f, ", then ")?;
                            self.fmts.$i.expects_fmt(f, indent)?;
                        )*
                        Ok(())
                    }
                }
            }



            comb_impl! {
                gen [<Tuple $li>]<[<C $fi>] $(, [<C $i>])*> {
                    /// The internal combinators making up the tuple.
                    combs: ([<C $fi>], $([<C $i>]),*),
                } impl {
                    type Formatter = [<Tuple $li ExpectsFormatter>]<[<C $fi>]::Formatter $(, [<C $i>]::Formatter)*>;
                    type Output = ([<C $fi>]::Output, $([<C $i>]::Output),*);
                    type Recoverable = [<Tuple $li Error>]<F, S, [<C $fi>]::Recoverable $(, [<C $i>]::Recoverable)*>;
                    type Fatal = [<Tuple $li Error>]<F, S, [<C $fi>]::Fatal $(, [<C $i>]::Fatal)*>;


                    fn<'t, [<C $fi>] $(, [<C $i>])*> Expects<'t>::expects(&self: Self<[<C $fi>] $(, [<C $i>])*>)
                    where
                        [<C $fi>]: (crate::Expects<'t>),
                        $([<C $i>]: (crate::Expects<'t>),)*
                    {
                        [<Tuple $li ExpectsFormatter>] { fmts: (self.combs.$fi.expects(), $(self.combs.$i.expects(),)*) }
                    }

                    fn<'t, F, S, [<C $fi>] $(, [<C $i>])*> Combinator<'t, F, S>::parse(&mut self: Self<[<C $fi>] $(, [<C $i>])*>, input: Span<F, S>)
                    where
                        F: (Clone),
                        S: (Clone),
                        [<C $fi>]: (crate::Combinator2<'t, F, S>),
                        $([<C $i>]: (crate::Combinator2<'t, F, S>),)*
                    {
                        // We collect the results as we find them
                        let mut results: (MaybeUninit<[<C $fi>]::Output>, $(MaybeUninit<[<C $i>]::Output>,)*) = (MaybeUninit::<[<C $fi>]::Output>::uninit(), $(MaybeUninit::<[<C $i>]::Output>::uninit(),)*);
                        let rem: Span<F, S> = match self.combs.$fi.parse(input.clone()) {
                            Ok((rem, res)) => {
                                results.$fi.write(res);
                                rem
                            },
                            Err(SnackError::Recoverable(err)) => return Err(SnackError::Recoverable([<Tuple $li Error>]::[<Comb $fi>]{ err, span: input })),
                            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal([<Tuple $li Error>]::[<Comb $fi>]{ err, span: input })),
                            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
                        };
                        $(
                            let rem: Span<F, S> = match self.combs.$i.parse(rem.clone()) {
                                Ok((rem, res)) => {
                                    results.$i.write(res);
                                    rem
                                },
                                Err(SnackError::Recoverable(err)) => return Err(SnackError::Recoverable([<Tuple $li Error>]::[<Comb $i>]{ err, span: rem })),
                                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal([<Tuple $li Error>]::[<Comb $i>]{ err, span: rem })),
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

                    comb<'t, F, S, [<C $fi>] $(, [<C $i>])*> [<tuple $li>](combs: ([<C $fi>], $( [<C $i>]),*)) -> Self<[<C $fi>] $(, [<C $i>])*> {
                        [<Tuple $li>] { combs }
                    }
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

/// Implements [`Combinator2`] for various sizes of tuples for us.
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

// Implement `Combinator2`
tuple_comb_impls!((1, 0), (2, 1), (3, 2), (4, 3), (5, 4), (6, 5), (7, 6), (8, 7), (9, 8), (10, 9), (11, 10), (12, 11));





/***** LIBRARY *****/
comb_impl! {
    /// Applies a tuple of combinators, in-order.
    ///
    /// This combinator will try the given ones one-by-one. It either returns the first non-OK [`Result`], or a same-sized tuple of all the combinator's results.
    ///
    /// # Note
    /// Actually, this combinator is really a no-op, and simply returns the given combinator. This is possible because tuples simply implement [`Combinator`].
    /// Only tuples up to a size of 12 are implemented. For more, consider nesting tuples within each other.
    ///
    /// # Arguments
    /// - `combs`: The tuple of combinators to apply.
    ///
    /// # Returns
    /// A combinator `TupleN` that will first apply the given N combinators in-order.
    ///
    /// # Fails
    /// The returned combinator fails if any of the given `combs`inators fails.
    ///
    /// # Example
    /// ```rust
    /// use ast_toolkit_snack::error::{Common, Failure};
    /// use ast_toolkit_snack::sequence::tuple;
    /// use ast_toolkit_snack::utf8::complete::{digit1, tag};
    /// use ast_toolkit_snack::{Combinator as _, Result as SResult};
    /// use ast_toolkit_span::Span;
    ///
    /// let span1 = Span::new("<example>", "Hello123");
    /// let span2 = Span::new("<example>", "123");
    /// let span3 = Span::new("<example>", "HelloWorld");
    ///
    /// let mut comb = tuple((tag("Hello"), digit1()));
    /// assert_eq!(
    ///     comb.parse(span1).unwrap(),
    ///     (span1.slice(8..), (span1.slice(..5), span1.slice(5..8)))
    /// );
    /// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::TagUtf8 { .. }))));
    /// assert!(matches!(comb.parse(span3), SResult::Fail(Failure::Common(Common::Digit1 { .. }))));
    /// ```
    gen Tuple<T, F, S> {
        tuple: T,
        _f: PhantomData<F>,
        _s: PhantomData<S>,
    } impl {
        type Formatter = T::Formatter;
        type Output = T::Output;
        type Recoverable = T::Recoverable;
        type Fatal = T::Fatal;


        fn<'t, T, F, S> Expects<'t>::expects(&self: Self<T, F, S>)
        where
            T: (Expects<'t>),
        {
            self.tuple.expects()
        }

        fn<'t, T, F, S> Combinator<'t, F, S>::parse(&mut self: Self<T, F, S>, input: Span<F, S>)
        where
            T: (Combinator2<'t, F, S>),
        {
            self.tuple.parse(input)
        }

        comb<'t, T, F, S> tuple(tuple: T) -> Self<T, F, S> {
            Tuple { tuple, _f: PhantomData, _s: PhantomData }
        }
    }
}
