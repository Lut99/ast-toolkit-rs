//  SEQUENCE 2.rs
//    by Lut99
//
//  Created:
//    19 Sep 2024, 13:15:39
//  Last edited:
//    02 Nov 2024, 11:05:31
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements combinators for doing other combinators in sequence.
//

use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::mem::MaybeUninit;

use ast_toolkit_span::{Span, Spanning};

use crate::result::SnackError;
use crate::{Combinator2, Expects, ExpectsFormatter};


/***** IMPLEMENTATIONS *****/
/// Implements [`Combinator2`] for a particular tuple size for us.
macro_rules! tuple_comb_impl {
    // Non-empty tuple implementation
    (impl => $li:tt : $fi:tt $(, $i:tt)*) => {
        ::paste::paste! {
            /* ERRORS */
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



            /* FORMATTERS */
            #[doc = concat!("Expects formatter for the ", stringify!([<Tuple $li>]), " combinator.")]
            #[derive(Debug)]
            pub struct [<Tuple $li ExpectsFormatter>]<[<F $fi>] $(, [<F $i>])*> {
                /// The internal formatters for every combinator making up the tuple.
                fmts: ([<F $fi>], $([<F $i>]),*),
            }
            impl<[<F $fi>]: ExpectsFormatter $(, [<F $i>]: ExpectsFormatter)*> Display for [<Tuple $li ExpectsFormatter>]<[<F $fi>] $(, [<F $i>])*> {
                #[inline]
                fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
                    write!(f, "Expected ")?;
                    self.expects_fmt(f, 0)
                }
            }
            impl<[<F $fi>]: ExpectsFormatter $(, [<F $i>]: ExpectsFormatter)*> ExpectsFormatter for [<Tuple $li ExpectsFormatter>]<[<F $fi>] $(, [<F $i>])*> {
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
            #[doc = concat!("Combinator for a tuple of ", stringify!($li), " combinators.\n\nAll combinators are applied in-order, where any failure short-circuits the parsing.")]
            pub struct [<Tuple $li>]<[<C $fi>] $(, [<C $i>])*> {
                /// The internal combinators making up the tuple.
                combs: ([<C $fi>], $([<C $i>]),*),
            }
            impl<'t, [<C $fi>]: Expects<'t> $(, [<C $i>]: Expects<'t>)*> Expects<'t> for [<Tuple $li>]<[<C $fi>] $(, [<C $i>])*> {
                type Formatter = [<Tuple $li ExpectsFormatter>]<[<C $fi>]::Formatter $(, [<C $i>]::Formatter)*>;

                #[inline]
                fn expects(&self) -> Self::Formatter {
                    [<Tuple $li ExpectsFormatter>] { fmts: (self.combs.$fi.expects(), $(self.combs.$i.expects(),)*) }
                }
            }
            impl<'t, F, S, [<C $fi>] $(, [<C $i>])*> Combinator2<'t, F, S> for [<Tuple $li>]<[<C $fi>] $(, [<C $i>])*>
            where
                F: Clone,
                S: Clone,
                [<C $fi>]: Combinator2<'t, F, S>,
                $([<C $i>]: Combinator2<'t, F, S>,)*
            {
                type Output = ([<C $fi>]::Output, $([<C $i>]::Output),*);
                type Recoverable = [<Tuple $li Error>]<F, S, [<C $fi>]::Recoverable $(, [<C $i>]::Recoverable)*>;
                type Fatal = [<Tuple $li Error>]<F, S, [<C $fi>]::Fatal $(, [<C $i>]::Fatal)*>;

                fn parse(&mut self, input: Span<F, S>) -> Result<(Span<F, S>, Self::Output), SnackError<F, S, Self::Recoverable, Self::Fatal>> {
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
#[inline]
pub const fn tuple<C>(combs: C) -> C { combs }