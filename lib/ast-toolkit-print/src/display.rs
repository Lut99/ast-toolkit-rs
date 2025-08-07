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


/***** HELPERS *****/
/// Helps us to remember what coloring policy to apply.
#[cfg(feature = "color")]
pub(crate) enum Coloring {
    /// Use a fixed coloring
    Manual(bool),
    /// Automatically determine based on stdout
    AutoStdout,
    /// Automatically determine based on stderr
    AutoStderr,
}





/***** LIBRARY *****/
/// Formatter for serializing [`Output`] and friends.
pub struct Display<'a, T: ?Sized>(pub(crate) &'a T, #[cfg(feature = "color")] pub(crate) Coloring);
impl<'a, T: ?Sized + DisplayFmt> fmt::Display for Display<'a, T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> FResult {
        let mut fmt = Formatter::new(f);
        #[cfg(feature = "color")]
        match self.1 {
            Coloring::Manual(color) => {
                fmt.use_color(color);
            },
            Coloring::AutoStdout => {
                fmt.use_color_auto_stdout();
            },
            Coloring::AutoStderr => {
                fmt.use_color_auto_stderr();
            },
        }
        self.0.display_fmt(&mut fmt)
    }
}
