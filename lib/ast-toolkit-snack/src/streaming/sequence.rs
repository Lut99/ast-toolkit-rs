//  SEQUENCE.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 11:41:38
//  Last edited:
//    05 Apr 2024, 11:42:05
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators that apply multiple combinators in-order.
//!   
//!   These combinators are streaming, meaning they will throw [`Failure::NotEnough`] if there wasn't enough input.
//


/***** LIBRARY *****/
