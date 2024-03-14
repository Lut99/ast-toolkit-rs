//  ERROR.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:51:38
//  Last edited:
//    14 Mar 2024, 09:03:41
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines `snack`'s extensive error type.
//

use std::error;
use std::fmt::{Display, Formatter, Result as FResult};

use crate::fail::{failure_impl, Failure};


/***** LIBRARY *****/
failure_impl! {
    /// `snack`'s extensive error type that can be used to generate explanatory diagnostics.
    ///
    /// One can think of this as a more verbose version of [`Failure`], as any recoverable error may be turned into an unrecoverable one.
    #[derive(Clone, Copy, Debug)]
    pub enum Error<'t> {}
}
