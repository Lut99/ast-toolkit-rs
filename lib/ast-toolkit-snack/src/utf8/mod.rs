//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:37:29
//  Last edited:
//    19 Mar 2025, 10:40:00
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines counterparts for [`scan`](super::scan)-combiantors that are
//!   working over unicode graphemes instead of individual bytes.
//

// Submodules
// pub mod graph;
// pub mod one_of0;
// pub mod one_of1;
pub mod tag_s;
// pub mod while0;
// pub mod while1;

// Use the combinator functions themselves
// pub use graph::graph;
// pub use one_of0::one_of0;
// pub use one_of1::one_of1;
pub use tag_s::tag_s;
// pub use while0::while0;
// pub use while1::while1;
