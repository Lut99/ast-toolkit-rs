//  DERIVE GENERICS.rs
//    by Lut99
//
//  Created:
//    06 Feb 2025, 09:51:01
//  Last edited:
//    06 Feb 2025, 10:10:44
//  Auto updated?
//    Yes
//
//  Description:
//!   An integration test for testing whether [`ToNode`] and
//!   [`ToDelimNode`] work with generics.
//

use ast_toolkit_railroad::{ToNode, ToNonTerm};


/***** AST *****/
#[derive(ToNonTerm)]
struct Top<W> {
    _wrapper: W,
}

#[derive(ToNode)]
struct Wrapper<B> {
    _base: B,
}

#[derive(ToNode)]
#[railroad(terminal = "hello")]
struct Base;





/***** ENTRYPOINT *****/
#[test]
fn test_derive_generics() { let _ = Top::<Wrapper<Base>>::railroad(); }
