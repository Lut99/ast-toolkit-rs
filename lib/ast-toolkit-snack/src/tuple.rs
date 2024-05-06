//  TUPLE.rs
//    by Lut99
//
//  Created:
//    01 May 2024, 15:42:04
//  Last edited:
//    06 May 2024, 11:14:30
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements [`Combinator`] for tuples.
//

use std::fmt::{Debug, Display, Formatter, Result as FResult};

use ast_toolkit_span::Span;

use crate::{Combinator, Expects, ExpectsFormatter, Result};


/***** HELPER MACROS *****/
/// Given a tuple, produces an Expects-string formatter for it.
macro_rules! tuple_combinator_expected_impl {
    // It's the _only_, not the last
    (start: $self:ident, $f:ident, $indent:ident => $i:tt) => {
        $self.fmts.$i.expects_fmt($f, $indent)?;
    };
    // It's more than one
    (start: $self:ident, $f:ident, $indent:ident => $fi:tt $(, $i:tt)+) => {
        $self.fmts.$fi.expects_fmt($f, $indent)?;
        tuple_combinator_expected_impl!($self, $f, $indent => $($i),+);
    };

    // Deal with the pre-last one
    ($self:ident, $f:ident, $indent:ident => $i:tt $(, $rem:tt)+) => {
        write!($f, ", ")?;
        $self.fmts.$i.expects_fmt($f, $indent)?;
        tuple_combinator_expected_impl!($self, $f, $indent => $($rem),+);
    };
    // Deal with the last one
    ($self:ident, $f:ident, $indent:ident => $i:tt) => {
        write!($f, " and then ")?;
        $self.fmts.$i.expects_fmt($f, $indent)?;
    };
}

/// Implements [`Combinator`] for a particular tuple for us.
macro_rules! tuple_combinator_impl {
    // Non-empty tuple implementation
    (impl => $li:tt : ($fi:tt, $fname:ident) $(, ($i:tt, $name:ident))*) => {
        paste::paste! {
            /// Formats the expects-string for a tuple of a particular size
            #[derive(Debug)]
            pub struct [< Tuple $li Formatter >]<$fname: Debug $(, $name: Debug)*> {
                /// The formatters for all nested combinators.
                pub(crate) fmts: ($fname, $($name,)*),
            }
            impl<$fname: ExpectsFormatter $(, $name: ExpectsFormatter)*> Display for [< Tuple $li Formatter >]<$fname $(, $name)*> {
                #[inline]
                fn fmt(&self, f: &mut Formatter) -> FResult {
                    write!(f, "Expected ")?;
                    self.expects_fmt(f, 0)
                }
            }
            impl<$fname: ExpectsFormatter $(, $name: ExpectsFormatter)*> ExpectsFormatter for [< Tuple $li Formatter >]<$fname $(, $name)*> {
                #[inline]
                fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
                    tuple_combinator_expected_impl!(start: self, f, indent => $fi $(, $i)*);
                    Ok(())
                }
            }

            // Then implement Expects and Combinator for the tuple
            paste::paste!(
                impl<'t, $fname: Expects<'t> $(, $name: Expects<'t>)*> Expects<'t> for ($fname, $($name,)*) {
                    type Formatter = [< Tuple $li Formatter >]<$fname::Formatter $(, $name::Formatter)*>;

                    #[inline]
                    fn expects(&self) -> Self::Formatter {
                        [< Tuple $li Formatter >] {
                            fmts: (self.$fi.expects(), $(self.$i.expects(),)*),
                        }
                    }
                }
                impl<'t, F, S, E, $fname: Combinator<'t, F, S, Error = E> $(, $name: Combinator<'t, F, S, Error = E>)*> Combinator<'t, F, S> for ($fname, $($name,)*) {
                    type Output = ($fname::Output, $($name::Output,)*);
                    type Error = E;

                    #[inline]
                    fn parse(&mut self, input: Span<F, S>) -> Result<'t, Self::Output, F, S, Self::Error> {
                        let (rem, [<res $fi>]): (Span<F, S>, $fname::Output) = match self.$fi.parse(input) {
                            Result::Ok(rem, res) => (rem, res),
                            Result::Fail(fail) => return Result::Fail(fail),
                            Result::Error(err) => return Result::Error(err),
                        };
                        $(let (rem, [<res $i>]): (Span<F, S>, $name::Output) = match self.$i.parse(rem) {
                            Result::Ok(rem, res) => (rem, res),
                            Result::Fail(fail) => return Result::Fail(fail),
                            Result::Error(err) => return Result::Error(err),
                        };)*
                        Result::Ok(rem, ([<res $fi>], $([<res $i>],)*))
                    }
                }
            );
        }
    };

    // Done with reversing the arguments; call us forwardly
    ($li:tt: reverse: $(($i:tt, $name:ident)),*) => {
        tuple_combinator_impl!(impl => $li: $(($i, $name)),*);
    };
    // More to reverse
    ($li:tt: ($fi:tt, $fname:ident) $(, ($i:tt, $name:ident))* reverse: $(($ri:tt, $rname:ident)),*) => {
        tuple_combinator_impl!($li: $(($i, $name)),* reverse: ($fi, $fname) $(, ($ri, $rname))*);
    };
}

/// Implements [`Combinator`] for various sizes of tuples for us.
macro_rules! tuple_combinator_impls {
    // Base case; empty tuple implementation (we don't do that here)
    (impl:) => {};
    // "Forward" run of all the arguments once reversed
    (impl: ($fi:tt, $fname:ident) $(, ($i:tt, $name:ident))*) => {
        // Build the smaller edition first
        tuple_combinator_impls!(impl: $(($i, $name)),*);

        // Then implement it
        tuple_combinator_impl!($fi: ($fi, $fname) $(, ($i, $name))* reverse:);
    };

    // More to reverse
    (($fi:tt, $fname:ident) $(, ($i:tt, $name:ident))* reverse: $(($ri:tt, $rname:ident)),*) => {
        tuple_combinator_impls!($(($i, $name)),* reverse: ($fi, $fname) $(, ($ri, $rname))*);
    };
    // Done with reversing the arguments; call us forwardly
    (reverse: $(($i:tt, $name:ident)),*) => {
        tuple_combinator_impls!(impl: $(($i, $name)),*);
    };

    // Calling interface
    ($(($i:tt, $name:ident)),*) => {
        tuple_combinator_impls!($(($i, $name)),* reverse:);
    }
}

// Default impls for tuples
tuple_combinator_impls!((0, C1), (1, C2), (2, C3), (3, C4), (4, C5), (5, C6), (6, C7), (7, C8), (8, C9), (9, C10), (10, C11), (11, C12));
