//  PARENS.rs
//    by Lut99
//
//  Created:
//    28 Nov 2024, 12:54:18
//  Last edited:
//    24 Mar 2025, 12:29:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Shows an example for the ast-toolkit-tokens macros for delimited
//!   tokens.
//

use ast_toolkit_span::Span;
use ast_toolkit_tokens::{Utf8Delimiter as _, utf8_delim};


/***** AST *****/
utf8_delim!(Parens, "(", ")");
#[cfg(feature = "snack")]
ast_toolkit_tokens::utf8_delim_snack!(Parens);
#[cfg(feature = "railroad")]
ast_toolkit_tokens::utf8_delim_railroad!(Parens, "(", ")");


/***** ENTRYPOINT *****/
fn main() {
    let span1 = Span::new("(foo)");
    let span2 = Span::new("(bar)");
    let paren1 = Parens { open: span1.slice(..1), close: span1.slice(4..) };
    let paren2 = Parens { open: span2.slice(..1), close: span2.slice(4..) };

    // We can create them as expected
    assert!(format!("{paren1:?}").starts_with("Parens { open: Span<&str> { source: "));
    assert_eq!(paren1, paren2);
    assert_eq!(Parens::<()>::OPEN_TOKEN, "(");

    // Also parse some string
    #[cfg(feature = "snack")]
    use ast_toolkit_snack::Combinator as _;
    #[cfg(feature = "snack")]
    let parens = Parens::parser(ast_toolkit_snack::utf8::complete::tag("foo")).parse(Span::new("(foo)")).unwrap().1.1;
    #[cfg(feature = "snack")]
    assert!(format!("{parens:?}").starts_with("Parens { open: Span<&str> { source: "));

    // Also generate railroad diagram nodes
    #[cfg(feature = "railroad")]
    let _node1 = <Parens<()> as ast_toolkit_railroad::ToDelimNode>::railroad_open();
    #[cfg(feature = "railroad")]
    let _node2 = <Parens<()> as ast_toolkit_railroad::ToDelimNode>::railroad_close();
}
