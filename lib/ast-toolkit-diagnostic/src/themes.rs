//  THEMES.rs
//    by Lut99
//
//  Description:
//!   Defines the base [`Theme`] trait, as well as a few default themes coming
//!   with the library.
//


/***** LIBRARY *****/
/// Defines a theme for customizing layouting of source snippets with their annotations.
pub trait Theme {}



/// Defines a "plain" theme without colors.
pub struct Plain;
impl Theme for Plain {}
