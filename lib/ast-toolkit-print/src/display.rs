//  DISPLAY.rs
//    by Lut99
//
//  Description:
//!   Provides the [`Display`] formatter for
//!   [`Displayable`](crate::spec::Displayable) types.
//

use std::fmt::{self, Result as FResult};

use crate::formatter::Formatter;
use crate::spec::DisplayFmt;


/***** LIBRARY *****/
/// Formatter for serializing [`Output`] and friends.
pub struct Display<'a, T: ?Sized>(pub(crate) &'a T, #[cfg(feature = "color")] pub(crate) Option<bool>);
impl<'a, T: ?Sized + DisplayFmt> fmt::Display for Display<'a, T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> FResult {
        let mut fmt = Formatter::new(f);
        #[cfg(feature = "color")]
        match self.1 {
            Some(color) => {
                fmt.use_color(color);
            },
            None => {
                fmt.use_color_auto_stdout();
            },
        }
        self.0.display_fmt(&mut Formatter::new(f))
    }
}
