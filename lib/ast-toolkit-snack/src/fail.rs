//  FAIL.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:53:58
//  Last edited:
//    30 Mar 2024, 11:51:06
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
            $($fields:ident {},)*
        }
    ) => {
        $(#[$attrs])*
        pub enum $name {
            // Failure fields first
            /// Failed to match a particular tag.
            Tag { tag: &'static dyn std::fmt::Debug },

            // Then any fields given by e.g. Error
            $($fields:ident {},)*
        }
        impl $name {
            /// Comparable to [`PartialEq::eq`], but then only to see if the variants match.
            ///
            /// # Returns
            /// True if `self` has the same variant as `other`.
            pub fn is_same(&self, other: &Self) -> bool {
                match (self, other) {
                    (Self::Tag { .. }, Self::Tag { .. }) => true,
                    $((Self::$fields { .. }, Self::$fields { .. }) => true,)*
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
    /// One can think of this as a more restricted version of [`Error`](crate::error::Error), as any recoverable error may be turned into an unrecoverable one.
    #[derive(Clone, Copy, Debug)]
    pub enum Failure {}
}
