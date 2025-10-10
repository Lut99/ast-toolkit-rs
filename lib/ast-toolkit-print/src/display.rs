//  DISPLAY.rs
//    by Lut99
//
//  Description:
//!   Provides the [`Display`] formatter for
//!   [`Displayable`](crate::spec::Displayable) types.
//

use std::fmt::{self, Result as FResult};

#[cfg(feature = "color")]
use crate::formatter::Coloring;
use crate::formatter::Formatter;
use crate::spec::DisplayFmt;


/***** LIBRARY *****/
/// Formatter for serializing [`Output`] and friends.
pub struct Display<'a, T: ?Sized>(pub(crate) &'a T, #[cfg(feature = "color")] pub(crate) Coloring);
impl<'a, T: ?Sized + DisplayFmt> fmt::Display for Display<'a, T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> FResult {
        #[cfg(not(feature = "color"))]
        let mut fmt = Formatter::new(f);
        #[cfg(feature = "color")]
        let mut fmt = Formatter::with_color(f, self.1);
        self.0.display_fmt(&mut fmt)
    }
}
