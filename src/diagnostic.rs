//  DIAGNOSTIC.rs
//    by Lut99
// 
//  Created:
//    04 Jul 2023, 19:17:50
//  Last edited:
//    04 Jul 2023, 19:18:39
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Diagnostic`] object, which concerns itself with
//!   prettily formatting an error.
// 

use std::io::Write;

use enum_debug::EnumDebug;

use crate::span::Span;


/***** AUXILLARY *****/
/// Defines the possible types of [`Diagnostic`].
#[derive(Clone, Copy, Debug, EnumDebug, Eq, Hash, PartialEq)]
pub enum DiagnosticKind {
    /// An fatal error.
    Error,
    /// A warning.
    Warning,
    /// A note.
    Note,
    /// A suggestion.
    Suggestion,
}





/***** LIBRARY *****/

