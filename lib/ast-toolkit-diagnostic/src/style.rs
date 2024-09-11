//  STYLE.rs
//    by Lut99
//
//  Created:
//    24 May 2024, 17:40:01
//  Last edited:
//    24 May 2024, 18:09:50
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines the possible colourations for diagnostic renderings.
//

use std::fmt::Debug;


/***** LIBRARY *****/
/// Abstracts over a particular colour/style palette used when rendering [`Diagnostic`](crate::Diagnostic)s.
pub trait Style: Debug {}





/***** STYLES *****/
/// A plain style that doesn't colourize anything.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Plain;
impl Style for Plain {}
