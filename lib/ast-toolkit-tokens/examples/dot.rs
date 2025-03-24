//  DOT.rs
//    by Lut99
//
//  Created:
//    28 Nov 2024, 12:47:11
//  Last edited:
//    24 Mar 2025, 12:28:46
//  Auto updated?
//    Yes
//
//  Description:
//!   Showcases the macros of the `ast-toolkit-tokens` crate.
//

use ast_toolkit_span::Span;
use ast_toolkit_tokens::{Utf8Token as _, utf8_token};


/***** AST *****/
utf8_token!(Dot, ".");
#[cfg(feature = "snack")]
ast_toolkit_tokens::utf8_token_snack!(Dot);
#[cfg(feature = "railroad")]
ast_toolkit_tokens::utf8_token_railroad!(Dot, ".");





/***** ENTRYPOINT *****/
fn main() {
    // We can create them as expected
    let dot1 = Dot { span: Span::new(".") };
    let dot2 = Dot { span: Span::new(".") };

    // Now we can do stuff with it
    assert!(format!("{dot1:?}").starts_with("Dot { span: Span<&str> { source: "));
    assert_eq!(dot1, dot2);
    assert_eq!(Dot::<()>::TOKEN, ".");

    // Also parse some string
    #[cfg(feature = "snack")]
    use ast_toolkit_snack::Combinator as _;
    #[cfg(feature = "snack")]
    let dot3 = Dot::parser(ast_toolkit_snack::combinator::nop()).parse(Span::new(".")).unwrap().1;
    #[cfg(feature = "snack")]
    assert!(format!("{dot3:?}").starts_with("Dot { span: Span<&str> { source: "));

    // Also generate railroad diagram nodes
    #[cfg(feature = "railroad")]
    let _node = <Dot<&str> as ast_toolkit_railroad::ToNode>::railroad();
}
