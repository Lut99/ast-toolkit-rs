//  DOC.rs
//    by Lut99
// 
//  Created:
//    16 Jul 2023, 11:24:32
//  Last edited:
//    17 Jul 2023, 19:14:02
//  Auto updated?
//    Yes
// 
//  Description:
//!   Produces the examples used in the docs.
// 

use ast_toolkit::{Diagnostic, Span};


/***** ENTRYPOINT *****/
fn main() {
    println!();
    Diagnostic::error("Invalid word 'Hlelo'", Span::from_idx("<example>", "Hlelo World!", 0, 4)).emit();
    Diagnostic::warn("Second word shouldn't be capitalized", Span::from_idx("<example>", "Hlelo World!", 6, 6)).emit();
    Diagnostic::note("The most classical program in the world is given here", Span::new("<example>", "Hlelo World!")).emit();
    Diagnostic::suggestion("Consider writing it properly", Span::new("<example>", "Hlelo World!"), "Hello, world!").emit();

    Diagnostic::error("Greeting not found", Span::new("<example>", "Hlelo World!")).set_code("E001").emit();
    Diagnostic::warn("Missing comma before 'world'", Span::from_idx("<example>", "Hlelo World!", 5, 5))
        .set_remark("Warning `comma` enabled by default")
        .add_suggestion("Add the missing comma", Span::from_idx("<example>", "Hlelo World!", 5, 5), ", ")
        .emit();
}
