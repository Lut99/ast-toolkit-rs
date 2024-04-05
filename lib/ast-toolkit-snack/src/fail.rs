//  FAIL.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:53:58
//  Last edited:
//    05 Apr 2024, 19:35:01
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines `snack`'s extensive failure type.
//

use std::error::{self, Error};
use std::fmt::{Debug, Display, Formatter, Result as FResult};


/***** HELPER MACROS *****/
/// Defines the fields of the [`Failure`], but macro'ified to make implementing them for [`Error`] easier.
macro_rules! failure_impl {
    (
        $(#[$attrs:meta])*
        pub enum $name:ident<F, S> {
            $(
                $(#[$var_attrs:meta])*
                $variants:ident $({
                    $(
                        $(#[$field_attrs:meta])*
                        $fields_name:ident: $fields_ty:ty
                    ),* $(,)?
                })?,
            )*
        }
        impl Expects {
            $($expects_variants:ident $({ $($expects_fields:ident),* $(,)? $(..)? })? => $expects_spans:expr),*
            $(,)?
        }
        impl Display {
            $($display_variants:ident $({ $($display_fields:ident),* $(,)? $(..)? })? => $display_spans:expr),*
            $(,)?
        }
        impl<F, S> Spanning<F, S> {
            $($spanning_variants:ident $({ $($spanning_fields:ident),* $(,)? $(..)? })? => $spanning_spans:expr),*
            $(,)?
        }
    ) => {
        $(#[$attrs])*
        pub enum $name<F, S> {
            // Failure fields first
            /// All possible options failed.
            Alt { branches: Vec<Self> },
            /// A `map_fallible()` failed because the user interfered with it.
            Custom { problem: std::rc::Rc<dyn crate::fail::ErrorSpanning<F, S>> },
            /// Failed to match at least one digit.
            Digit1 { span: Span<F, S> },
            /// Failed to match exactly `times` custom combinators.
            ManyN { times: usize, got: usize, fail: Box<Self> },
            /// A combinator succeeded... when it shouldn't!
            Not { span: Span<F, S> },
            /// Failed to match a one-of byteset.
            OneOfBytes1 { byteset: &'static dyn crate::fail::DebugAsRef, span: Span<F, S> },
            /// Failed to match a one-of charset.
            OneOfUtf81 { charset: &'static dyn crate::fail::DebugAsRef, span: Span<F, S> },
            /// Failed to match exactly `times` custom combinators separated by some other combinator, where it's the punct that we failed to parse.
            PunctuatedNPunct { times: usize, got: usize, fail: Box<Self> },
            /// Failed to match exactly `times` custom combinators separated by some other combinator.
            PunctuatedNValue { times: usize, got: usize, fail: Box<Self> },
            /// Failed to match exactly `times` custom combinators separated by some other combinator, where it's the punct that we failed to parse.
            PunctuatedTrailingNPunct { times: usize, got: usize, fail: Box<Self> },
            /// Failed to match exactly `times` custom combinators separated by some other combinator.
            PunctuatedTrailingNValue { times: usize, got: usize, fail: Box<Self> },
            /// Failed to match exactly `times` custom combinators separated by some other combinator, where it's the punct that we failed to parse.
            SeparatedListNPunct { times: usize, got: usize, fail: Box<Self> },
            /// Failed to match exactly `times` custom combinators separated by some other combinator.
            SeparatedListNValue { times: usize, got: usize, fail: Box<Self> },
            /// Failed to match a particular tag.
            Tag { tag: &'static dyn crate::fail::DebugAsRef, span: Span<F, S> },
            /// Failed to match at least one whitespace.
            Whitespace1 { span: Span<F, S> },

            // Then any fields given by e.g. Error
            $(
                $(#[$var_attrs])*
                $variants $({
                    $(
                        $(#[$field_attrs])*
                        $fields_name: $fields_ty,
                    )*
                })?,
            )*
        }

        impl<F, S: ast_toolkit_span::Spannable> Eq for $name<F, S> {}
        ::paste::paste! {
            impl<F, S: ast_toolkit_span::Spannable> PartialEq for $name<F, S> {
                #[inline]
                fn eq(&self, other: &Self) -> bool {
                    match (self, other) {
                        // Failure fields first
                        (Self::Alt { branches: branches1 }, Self::Alt { branches: branches2 }) => branches1 == branches2,
                        (Self::Custom { .. }, Self::Custom { .. }) => true,
                        (Self::Digit1 { span: span1 }, Self::Digit1 { span: span2 }) => span1 == span2,
                        (Self::ManyN { times: times_lhs, got: got_lhs, fail: fail_lhs }, Self::ManyN { times: times_rhs, got: got_rhs, fail: fail_rhs }) => times_lhs == times_rhs && got_lhs == got_rhs && fail_lhs == fail_rhs,
                        (Self::Not { span: span1 }, Self::Not { span: span2 }) => span1 == span2,
                        (Self::OneOfBytes1 { byteset: lhs, span: span1 }, Self::OneOfBytes1 { byteset: rhs, span: span2 }) => lhs.as_ref() == rhs.as_ref() && span1 == span2,
                        (Self::OneOfUtf81 { charset: lhs, span: span1 }, Self::OneOfUtf81 { charset: rhs, span: span2 }) => lhs.as_ref() == rhs.as_ref() && span1 == span2,
                        (Self::SeparatedListNPunct { times: times_lhs, got: got_lhs, fail: fail_lhs }, Self::SeparatedListNPunct { times: times_rhs, got: got_rhs, fail: fail_rhs }) => times_lhs == times_rhs && got_lhs == got_rhs && fail_lhs == fail_rhs,
                        (Self::SeparatedListNValue { times: times_lhs, got: got_lhs, fail: fail_lhs }, Self::SeparatedListNValue { times: times_rhs, got: got_rhs, fail: fail_rhs }) => times_lhs == times_rhs && got_lhs == got_rhs && fail_lhs == fail_rhs,
                        (Self::PunctuatedNPunct { times: times_lhs, got: got_lhs, fail: fail_lhs }, Self::PunctuatedNPunct { times: times_rhs, got: got_rhs, fail: fail_rhs }) => times_lhs == times_rhs && got_lhs == got_rhs && fail_lhs == fail_rhs,
                        (Self::PunctuatedNValue { times: times_lhs, got: got_lhs, fail: fail_lhs }, Self::PunctuatedNValue { times: times_rhs, got: got_rhs, fail: fail_rhs }) => times_lhs == times_rhs && got_lhs == got_rhs && fail_lhs == fail_rhs,
                        (Self::PunctuatedTrailingNPunct { times: times_lhs, got: got_lhs, fail: fail_lhs }, Self::PunctuatedTrailingNPunct { times: times_rhs, got: got_rhs, fail: fail_rhs }) => times_lhs == times_rhs && got_lhs == got_rhs && fail_lhs == fail_rhs,
                        (Self::PunctuatedTrailingNValue { times: times_lhs, got: got_lhs, fail: fail_lhs }, Self::PunctuatedTrailingNValue { times: times_rhs, got: got_rhs, fail: fail_rhs }) => times_lhs == times_rhs && got_lhs == got_rhs && fail_lhs == fail_rhs,
                        (Self::Tag { tag: lhs, span: span1 }, Self::Tag { tag: rhs, span: span2 }) => lhs.as_ref() == rhs.as_ref() && span1 == span2,
                        (Self::Whitespace1 { span: span1 }, Self::Whitespace1 { span: span2 }) => span1 == span2,

                        // Then any fields given by e.g. Error
                        $(
                            (
                                Self::$variants $({ $($fields_name: [<$fields_name _lhs>]),* })?,
                                Self::$variants $({ $($fields_name: [<$fields_name _rhs>]),* })?
                            ) => {
                                true $(&& $([<$fields_name _lhs>] == [<$fields_name _rhs>])&&*)?
                            },
                        )*

                        // Anything else is not the same variant, never matches
                        (_, _) => false,
                    }
                }
            }
        }

        impl<F, S> $name<F, S> {
            /// Returns some string description of what this variant expected to parse.
            ///
            /// # Returns
            /// Some [`String`] describing what was expected.
            pub fn expects(&self) -> String {
                match self {
                    // Failure fields first
                    Self::Alt { branches } => branches.iter().map(|b| b.expects()).collect::<Vec<String>>().join(", "),
                    Self::Custom { .. } => todo!(),
                    Self::Digit1 { .. } => "(a digit [0-9])".into(),
                    Self::ManyN { times, fail, .. } => format!("({} times {})", times, fail.expects()),
                    Self::Not { span } => format!("(anything else)"),
                    Self::OneOfBytes1 { byteset } => format!("({})", byteset.as_ref().iter().map(|b| format!("{:0x}"))),
                }
            }
        }
        impl<F, S> Display for $name<F, S> {
            fn fmt(&self, f: &mut Formatter) -> FResult {
                match self {
                    // Failure fields first
                    Self::Alt { branches } => write!(f, "(Expected one of: {})", self.expects()),
                    _ => todo!(),
                }
            }
        }

        impl<F: Clone, S: Clone + ast_toolkit_span::Spannable> ast_toolkit_span::Spanning<F, S> for $name<F, S> {
            #[inline]
            fn span(&self) -> Span<F, S> {
                match self {
                    // Failure fields first
                    Self::Alt { branches } => {
                        // Collect all spans into one.
                        let mut combs = branches.iter();
                        let mut span: Span<F, S> = combs.next().unwrap().span();
                        for comb in combs {
                            span.join_mut(&comb.span());
                        }
                        span
                    },
                    Self::Custom { problem } => problem.span(),
                    Self::Digit1 { span } => span.clone(),
                    Self::ManyN { fail, .. } => fail.span(),
                    Self::Not { span } => span.clone(),
                    Self::OneOfBytes1 { span, .. } => span.clone(),
                    Self::OneOfUtf81 { span, .. } => span.clone(),
                    Self::PunctuatedNPunct { fail, .. } => fail.span(),
                    Self::PunctuatedNValue { fail, .. } => fail.span(),
                    Self::PunctuatedTrailingNPunct { fail, .. } => fail.span(),
                    Self::PunctuatedTrailingNValue { fail, .. } => fail.span(),
                    Self::SeparatedListNPunct { fail, .. } => fail.span(),
                    Self::SeparatedListNValue { fail, .. } => fail.span(),
                    Self::Tag { span, .. } => span.clone(),
                    Self::Whitespace1 { span } => span.clone(),

                    // Then any fields given by e.g. Error
                    $(
                        Self::$spanning_variants $({ $($spanning_fields,)* .. })? => $spanning_spans,
                    )*
                }
            }
        }
    };
}
use ast_toolkit_span::{Span, Spanning};
pub(crate) use failure_impl;





/***** AUXILLARY *****/
/// A helper trait that abstracts over things both [`Debug`] and [`AsRef<[u8]>`].
pub trait DebugAsRef: Debug + AsRef<[u8]> {}
impl<T: ?Sized + Debug + AsRef<[u8]>> DebugAsRef for T {}

/// A helper trait that abstracts over things both [`Debug`] and [`Spanning`].
pub trait ErrorSpanning<F, S>: Error + Spanning<F, S> {}
impl<F, S, T: ?Sized + Error + Spanning<F, S>> ErrorSpanning<F, S> for T {}





/***** LIBRARY *****/
failure_impl! {
    /// `snack`'s extensive failure type that can be used to generate explanatory diagnostics.
    ///
    /// One can think of this as a subset of [`Error`](crate::error::Error), as any recoverable error may be turned into an unrecoverable one.
    #[derive(Clone, Debug)]
    pub enum Failure<F, S> {
        /// There wasn't enough input yet to parse. Only returned by streaming combinators.
        NotEnough { span: Span<F, S> },
    }
    impl Display {
        NotEnough { .. } => write!(f, "Not enough input"),
    }
    impl<F, S> Spanning<F, S> {
        NotEnough { span } => span.clone(),
    }
}
