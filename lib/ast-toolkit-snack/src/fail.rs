//  FAIL.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:53:58
//  Last edited:
//    05 Apr 2024, 11:33:31
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines `snack`'s extensive failure type.
//

use std::error;
use std::fmt::{Display, Formatter, Result as FResult};


/***** HELPER MACROS *****/
/// Defines the fields of the [`Failure`], but macro'ified to make implementing them for [`Error`] easier.
macro_rules! failure_impl {
    (
        $(#[$attrs:meta])*
        pub enum $name:ident {
            $(
                $(#[$var_attrs:meta])*
                $variants:ident {
                    $(
                        $(#[$field_attrs:meta])*
                        $fields_name:ident: $fields_ty:ty
                    ),* $(,)?
                },
            )*
        }
    ) => {
        $(#[$attrs])*
        pub enum $name {
            // Failure fields first
            /// Failed to match at least one digit.
            Digit1,
            /// Failed to match a one-of byteset.
            OneOfBytes1 { byteset: &'static dyn std::fmt::Debug },
            /// Failed to match a one-of charset.
            OneOfUtf81 { charset: &'static dyn std::fmt::Debug },
            /// Failed to match a particular tag.
            Tag { tag: &'static dyn std::fmt::Debug },
            /// Failed to match at least one whitespace.
            Whitespace1,

            // Then any fields given by e.g. Error
            $(

                $(#[$var_attrs])*
                $variants {
                    $(
                        $(#[$field_attrs])*
                        $fields_name: $fields_ty,
                    )*
                },
            )*
        }
        impl $name {
            /// Comparable to [`PartialEq::eq`], but then only to see if the variants match.
            ///
            /// # Returns
            /// True if `self` has the same variant as `other`.
            pub fn is_same(&self, other: &Self) -> bool {
                match (self, other) {
                    // Failure fields first
                    (Self::Digit1 { .. }, Self::Digit1 { .. }) => true,
                    (Self::OneOfBytes1 { .. }, Self::OneOfBytes1 { .. }) => true,
                    (Self::OneOfUtf81 { .. }, Self::OneOfUtf81 { .. }) => true,
                    (Self::Tag { .. }, Self::Tag { .. }) => true,
                    (Self::Whitespace1 { .. }, Self::Whitespace1 { .. }) => true,

                    // Then any fields given by e.g. Error
                    $((Self::$variants { .. }, Self::$variants { .. }) => true,)*

                    // Not a match if not the same
                    _ => false,
                }
            }
        }
    };
}
pub(crate) use failure_impl;





/***** LIBRARY *****/
failure_impl! {
    /// `snack`'s extensive failure type that can be used to generate explanatory diagnostics.
    ///
    /// One can think of this as a subset of [`Error`](crate::error::Error), as any recoverable error may be turned into an unrecoverable one.
    #[derive(Clone, Copy, Debug)]
    pub enum Failure {}
}
