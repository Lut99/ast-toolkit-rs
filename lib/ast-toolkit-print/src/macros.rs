//  MACROS.rs
//    by Lut99
//
//  Description:
//!   Defines convenience macros for this crate.
//


/***** LIBRARY *****/
/// A macro for conveniently calling [`Style::apply_to_args()`] and writing it to a formatter.
#[macro_export]
macro_rules! write_styled {
    ($f:expr, $style:expr, $($t:tt)*) => {
        ::std::write!($f, "{}", <::console::Style>::apply_to_args($style, ::std::format_args!($($t)*)))
    };
}

/// A macro for conveniently calling [`Style::apply_to_args()`] and writing it to a formatter with a newline.
#[macro_export]
macro_rules! writeln_styled {
    ($f:expr, $style:expr, $($t:tt)*) => {
        ::std::writeln!($f, "{}", <::console::Style>::apply_to_args($style, ::std::format_args!($($t)*)))
    };
}
