//  TESTS.rs
//    by Lut99
//
//  Created:
//    01 Mar 2024, 15:56:47
//  Last edited:
//    01 Mar 2024, 16:06:33
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
    let _ = Span::empty(function_name!(), "Hello there!");
    let _ = Span::new(function_name!(), "Hello there!");
    let _ = Span::raw_range(function_name!(), "Hello there!", 0..5);
    let _ = Span::raw_range(function_name!(), "Hello there!", 6..);
    let _ = Span::range(function_name!(), "Hellÿ there!", 0..5);
    let _ = Span::range(function_name!(), "Hellÿ there!", 6..);
}
