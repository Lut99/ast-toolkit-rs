//  MACROS.rs
//    by Lut99
//
//  Description:
//!   Defines convenience macros for this crate.
//


/***** LIBRARY *****/
/// A macro for conveniently calling
/// [`Formatter::write_dedup()`](crate::formatter::Formatter::write_dedup()) using format
/// arguments.
#[macro_export]
macro_rules! write_dedup {
    ($f:expr, $($t:tt)*) => {
        <$crate::Formatter<_>>::write_dedup($f, ::std::format_args!($($t)*))
    };
}



/// A macro for conveniently calling
/// [`Style::apply_to()`](crate::formatter::Style::apply_to()) and writing it to a formatter.
#[cfg(feature = "color")]
#[macro_export]
macro_rules! write_styled {
    ($f:expr, $style:expr, $($t:tt)*) => {
        ::std::write!($f, "{}", <$crate::Style>::apply_to($style, ::std::format_args!($($t)*)))
    };
}

/// A macro for conveniently calling
/// [`Style::apply_to()`](crate::formatter::Style::apply_to()) and writing it to a formatter with a
/// newline.
#[cfg(feature = "color")]
#[macro_export]
macro_rules! writeln_styled {
    ($f:expr, $style:expr, $($t:tt)*) => {
        ::std::writeln!($f, "{}", <$crate::Style>::apply_to($style, ::std::format_args!($($t)*)))
    };
}
