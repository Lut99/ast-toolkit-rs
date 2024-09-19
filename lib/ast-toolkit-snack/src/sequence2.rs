//  SEQUENCE 2.rs
//    by Lut99
//
//  Created:
//    19 Sep 2024, 13:15:39
//  Last edited:
//    19 Sep 2024, 14:13:10
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements combinators for doing other combinators in sequence.
//

use std::marker::PhantomData;

use crate::utils::{comb_impl, error_impl, fmt_impl};
use crate::{Combinator2, Expects};


/***** IMPLEMENTATIONS *****/
/// Implements [`Combinator2`] for a particular tuple size for us.
macro_rules! tuple_comb_impl {
    // Non-empty tuple implementation
    (impl => $li:tt : $fi:tt $(, $i:tt)*) => {
        ::paste::paste! {
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
                    }
                }
            }

            comb_impl! {
                gen [<Tuple $li>]<[<C $fi>] $(, [<C $i>])*> {
                    /// The internal combinators making up the tuple.
                    combs: ([<C $fi>], $([<C $i>]),*),
                } impl {
                    type Formatter = ();
                    type Output = ([<C $fi>]::Output, $([<C $i>]::Output),*);
                    type Recoverable = std::convert::Infallible;
                    type Fatal = std::convert::Infallible;


                    fn<'t, [<C $fi>] $(, [<C $i>])*> Expects<'t>::expects(&self: Self<[<C $fi>] $(, [<C $i>])*>) {
                        todo!()
                    }

                    fn<'t, F, S, [<C $fi>] $(, [<C $i>])*> Combinator<'t, F, S>::parse(&mut self: Self<[<C $fi>] $(, [<C $i>])*>, input: Span<F, S>)
                    where
                        [<C $fi>]: (crate::Combinator2<'t, F, S>),
                        $([<C $i>]: (crate::Combinator2<'t, F, S>),)*
                    {
                        todo!()
                    }

                    comb<'t, F, S, [<C $fi>] $(, [<C $i>])*> [<tuple $li>](combs: ([<C $fi>], $( [<C $i>]),*)) -> Self<[<C $fi>] $(, [<C $i>])*> {
                        todo!()
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
    /// Only tuples up to a size of 16 is implemented. For more, consider nesting tuples within each other.
    ///
    /// # Arguments
    /// - `combs`: The tuple of combinators to apply.
    ///
    /// # Returns
    /// A combinator [`Tuple`] that will first apply the given combinators in-order.
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
