//  FAIL.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:53:58
//  Last edited:
//    05 Apr 2024, 13:15:33
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines `snack`'s extensive failure type.
//

use std::error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};


/***** HELPER MACROS *****/
/// Defines the fields of the [`Failure`], but macro'ified to make implementing them for [`Error`] easier.
macro_rules! failure_impl {
    (
        $(#[$attrs:meta])*
        pub enum $name:ident {
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
    ) => {
        $(#[$attrs])*
        pub enum $name {
            // Failure fields first
            /// All possible options failed.
            Alt { branches: Vec<Self> },
            /// Failed to match at least one digit.
            Digit1,
            /// Failed to match a one-of byteset.
            OneOfBytes1 { byteset: &'static dyn crate::fail::DebugAsRef },
            /// Failed to match a one-of charset.
            OneOfUtf81 { charset: &'static dyn crate::fail::DebugAsRef },
            /// Failed to match a particular tag.
            Tag { tag: &'static dyn crate::fail::DebugAsRef },
            /// Failed to match at least one whitespace.
            Whitespace1,

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
        impl Eq for $name {}
        ::paste::paste! {
            impl PartialEq for $name {
                #[inline]
                fn eq(&self, other: &Self) -> bool {
                    match (self, other) {
                        // Failure fields first
                        (Self::Alt { branches: lhs }, Self::Alt { branches: rhs }) => lhs == rhs,
                        (Self::Digit1, Self::Digit1) => true,
                        (Self::OneOfBytes1 { byteset: lhs }, Self::OneOfBytes1 { byteset: rhs }) => lhs.as_ref() == rhs.as_ref(),
                        (Self::OneOfUtf81 { charset: lhs }, Self::OneOfUtf81 { charset: rhs }) => lhs.as_ref() == rhs.as_ref(),
                        (Self::Tag { tag: lhs }, Self::Tag { tag: rhs }) => lhs.as_ref() == rhs.as_ref(),
                        (Self::Whitespace1, Self::Whitespace1) => true,

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
    };
}
pub(crate) use failure_impl;





/***** AUXILLARY *****/
/// A helper trait that abstracts over things both [`Debug`] and [`AsRef<[u8]>`].
pub trait DebugAsRef: Debug + AsRef<[u8]> {}
impl<T: ?Sized + Debug + AsRef<[u8]>> DebugAsRef for T {}





/***** LIBRARY *****/
failure_impl! {
    /// `snack`'s extensive failure type that can be used to generate explanatory diagnostics.
    ///
    /// One can think of this as a subset of [`Error`](crate::error::Error), as any recoverable error may be turned into an unrecoverable one.
    #[derive(Clone, Debug)]
    pub enum Failure {
        /// There wasn't enough input yet to parse. Only returned by streaming combinators.
        NotEnough,
    }
}
