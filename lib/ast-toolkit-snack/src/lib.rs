//  LIB.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:37:24
//  Last edited:
//    08 May 2025, 16:53:50
//  Auto updated?
//    Yes
//
//  Description:
//!   The sun is coming out, the birds are tweeting... It's that time of
//!   the year again! Lut99 makes another re-code of `nom`!
//!
//!   Provides a parser-combinator framework heavily inspired by
//!   [nom](https://github.com/rust-bakery/nom), except that it gives up a
//!   little bit of performance over a more human-friendly debug experience.
//


// IDEA: Let's completely revamp this library?
//
// Not really an idea, more of a discussion. What are the issues with `snack` currently?
// - Have to load the entire input text in memory;
// - Complex on the type system; and
// - Very low-level to write.
//
// Instead, consider Rust's parser. It's lighter on the type system (because they have a
// predictable input stream, to be fair) and very convenient to use (object-oriented rather than
// function-oriented. Recognizes we're parsing objects anyway).
//
// The problem however, is to efficiently backtrack. Nom solves this by loading the entire input in
// memory; hence, it's not a problem to randomly access it. You use `Span`s as parse state to
// remember where you are when you fail a branch and go back; very elegant and fast memory-wise.
// Trying entire branches is pretty cheap because of cache efficiency (you tend to be in smaller,
// consequtive areas) and the parser can be pretty much pre-assembled by the compiler (if you're
// willing to suffer compilation times, that is).
//
// Alternatively, what if we don't backtrack but lookahead? We already noticed that efficient snack
// parsing comes when you embrace that parts of a struct are "recognizers" for what's going to go
// down: you match e.g. an integer, then a plus, and commit from there on out (turn `Recoverable`
// into `Fatal`s). Maybe it pays to formalize this process?
//
// Suppose that you have an `InputStream` as follows:
// ```rust
// trait InputStream {
//     type Elem;
//
//     /// The usual thing that advances the stream by `N` (convenience functions possible).
//     fn next(&mut self, buf: &mut [Self::Elem]) {
//         self.peek(buf);
//         self.advance(buf.len());
//     }
//
//     /// Similar but does not return objects. For efficiency.
//     fn advance(&mut self, n: usize);
//
//     /// Similar but does not advance the stream.
//     fn peek(&mut self, buf: &mut [Self::Elem]);
// }
// ```
//
// Then you implement a parser as:
// ```rust
// trait Parser<I: InputStream> {
//     type Error;
//
//     fn parse(input: I) -> Result<Option<Self>, Self::Error> {}
// }
// ```


// Declare submodules
// pub mod ascii;
pub mod asserts;
// pub mod boxed;
// pub mod branch;
// pub mod combinator;
// pub mod debug;
// pub mod error;
// #[cfg(feature = "extra")]
// pub mod extra;
// pub mod fmt;
pub mod auxillary;
pub mod span;
// mod macros;
// pub mod multi;
// pub mod result;
pub mod scan;
// pub mod sequence;
mod spec;
// pub mod utf8;

// Re-exports
pub mod prelude {
    pub use super::auxillary::ResultExt;
}
pub use ast_toolkit_loc::{Loc, Located};
pub use span::*;
pub use spec::*;
