//  TESTS.rs
//    by Lut99
//
//  Created:
//    01 Mar 2024, 15:56:47
//  Last edited:
//    14 Mar 2024, 08:32:43
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines some tests for the [`Span`].
//

use function_name::named;

use super::*;


/***** LIBRARY *****/
/// Tests if Span creating works
#[test]
#[named]
fn test_span_create() {
    // Create some spans
    let span = Span::empty(function_name!(), "Hello there!");
    assert_eq!(span.from(), function_name!());
    assert_eq!(span.source(), "Hello there!");
    assert_eq!(span.spanned(), "");

    let span = Span::new(function_name!(), "Hello there!");
    assert_eq!(span.spanned(), "Hello there!");

    let span = Span::raw_ranged(function_name!(), "Hello there!", RawUsize(0)..RawUsize(5));
    assert_eq!(span.spanned(), "Hello");

    let span = Span::raw_ranged(function_name!(), "Hello there!", RawUsize(6)..);
    assert_eq!(span.spanned(), "there!");

    let span = Span::ranged(function_name!(), "Hellÿ there!", LogicUsize(0)..LogicUsize(5));
    assert_eq!(span.spanned(), "Hellÿ");

    let span = Span::ranged(function_name!(), "Hellÿ there!", LogicUsize(6)..);
    assert_eq!(span.spanned(), "there!");
}

/// Tests if setting span sizes works
#[test]
#[named]
fn test_span_set() {
    // Create a span and mutate it
    let mut span = Span::new(function_name!(), "Hellÿ there!");
    assert_eq!(span.spanned(), "Hellÿ there!");

    span.set_raw_start(7);
    assert_eq!(span.spanned(), "there!");

    span.set_raw_start(0);
    span.set_raw_end(6);
    assert_eq!(span.spanned(), "Hellÿ");

    span.set_raw_end(13);
    span.set_start(6);
    assert_eq!(span.spanned(), "there!");

    span.set_start(0);
    span.set_end(5);
    assert_eq!(span.spanned(), "Hellÿ");
}

// /// Tests some parsing
// #[cfg(feature = "nom")]
// #[test]
// #[named]
// fn test_span_nom() {
//     // Some simple tag parsing
//     let span = Span::new(function_name!(), "Hello there!");
//     let (rem, hello) = nom::bytes::complete::tag::<&str, Span<&str, &str>, ()>("Hello")(span).unwrap();
//     assert_eq!(hello.spanned(), "Hello");
//     assert_eq!(rem.spanned(), " there!");

//     // Whitespaces
//     let src: String = "a".into();
//     let src: &str = src.as_str();
//     for c in <Span<&str, &str> as nom::InputTakeAtPosition>::split_at_position::<_, ()>(&Span::new(src, src), |_| true) {
//         println!("{:?}", std::any::type_name_of_val(&c));
//     }
//     // let (rem, space) = nom::character::complete::multispace1(rem).unwrap();
//     // assert_eq!(space.spanned(), " ");
//     // assert_eq!(rem.spanned(), "there!");
// }
