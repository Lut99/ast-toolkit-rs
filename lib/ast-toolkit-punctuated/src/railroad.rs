//  RAILROAD.rs
//    by Lut99
//
//  Description:
//!   Defines [`ToNode`]- and [`ToNonTerm`]-impls for the [`Punctuated`] list.
//

use ast_toolkit_railroad::{ToNode, ToNonTerm, railroad as rr};

use crate::Punctuated;


/***** IMPLS *****/
// Propagation for the Punctuated set
impl<V: ToNode, P: ToNode> ToNode for Punctuated<V, P> {
    type Node = rr::Repeat<V::Node, P::Node>;

    #[inline]
    fn railroad() -> Self::Node { rr::Repeat::new(V::railroad(), P::railroad()) }
}
impl<V: ToNonTerm, P: ToNode> ToNonTerm for Punctuated<V, P> {
    type NodeNonTerm = rr::Repeat<V::NodeNonTerm, P::Node>;

    #[inline]
    fn railroad_nonterm() -> Self::NodeNonTerm { rr::Repeat::new(V::railroad_nonterm(), P::railroad()) }
}
