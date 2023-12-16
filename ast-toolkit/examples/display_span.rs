//  DISPLAY SPAN.rs
//    by Lut99
//
//  Created:
//    16 Dec 2023, 12:04:54
//  Last edited:
//    16 Dec 2023, 12:43:41
//  Auto updated?
//    Yes
//
//  Description:
//!   Shows some usage of the [`DisplaySpan`]-trait.
//

#[cfg(not(feature = "diagnostic-located-span"))]
compile_error!("Please enable the `diagnostic-located-span` feature when running the `display_span` example");

use ast_toolkit::diagnostic::located_span::LocatedSpan;
use ast_toolkit::diagnostic::{DisplaySpan as _, RustStyle};


/***** ENTRYPOINT *****/
fn main() {
    // Run some tests
    print!("{}", LocatedSpan::new_extra("Hello, world!", "<example>").display_span(RustStyle));
    print!("{}", LocatedSpan::new_extra("Hello, world!\nGeneral Kenobi!", "<example>").display_span(RustStyle));
}
