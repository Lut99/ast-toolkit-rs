use ast_toolkit::diagnostic::{RustStyle, Span};
use console::Style;


fn main() {
    let span = Span::new("<example>", "Hello there!");
    println!("{}", span.text_snippet_styled(Style::new().bold().red(), RustStyle));
}
