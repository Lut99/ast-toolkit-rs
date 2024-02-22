//  RAILROAD.rs
//    by Lut99
//
//  Created:
//    22 Feb 2024, 12:00:18
//  Last edited:
//    22 Feb 2024, 14:39:23
//  Auto updated?
//    Yes
//
//  Description:
//!   Provides tools to generate railroad diagrams from ASTs.
//

pub use ::railroad::*;
#[cfg(feature = "derive")]
pub use ast_toolkit_derive::ToNode;


/***** LIBRARY *****/
/// Trait that generalizes over AST nodes such that they can hiearchically create [`railroad`] diagrams.
///
/// See the [`ToNode`](crate::procedural::ToNode)-derive macro to make implementing this trait for structs & enums very convenient.
///
/// # Example
/// ```rust
/// todo!();
/// ```
pub trait ToNode {
    /// The [`railroad::Node`]-type to which this AST node compiles.
    type Node: railroad::Node;


    /// Generates a [`railroad::Node`] for this AST node that is used to build a railroad diagram.
    ///
    /// # Returns
    /// An [`railroad::Node`] of type [Self::Node](ToNode::Node) that represents the railroad-counterpart to Self.
    fn railroad() -> Self::Node;
}



/// Extends [`ToNode`] with a function that is used to provide a different representation when referring to an AST node VS defining it.
///
/// A typical example of this are more complex AST nodes like expressions, which are nice to define separately and then refer to in parent nodes.
/// Conventionally, the normal [`ToNode`]-implementation returns a [`railroad::NonTerminal`] that only refers to it.
///
/// See the [`ToNonTerm`](crate::procedural::ToNonTerm)-derive macro to make implementing this trait for structs & enums very convenient.
///
/// # Example
/// ```rust
/// todo!();
/// ```
pub trait ToNonTerm: ToNode {
    /// The [`railroad::Node`]-type to which this AST node compiles when asked for its full representation.
    type NodeNonTerm: railroad::Node;


    /// Generates a [`railroad::Node`] for this AST node that represents it fully instead of only a reference to it.
    ///
    /// # Returns
    /// An [`railroad::Node`] of type [Self::NodeNonTerm](ToNode::NodeNonTerm) that represents the railroad-counterpart to Self in full.
    fn railroad_nonterm() -> Self::NodeNonTerm;
}

/// Extends [`ToNode`] with a separation between a start half of the AST node, and an end half.
///
/// A typical example of this are single AST nodes that represents delimiters (e.g., parenthesis `()`).
/// Conventionally, the normal [`ToNode`]-implementation returns the delimiters in one go.
///
/// See the [`ToDelimNode`](crate::procedural::ToDelimNode)-derive macro to make implementing this trait for structs & enums very convenient.
///
/// # Example
/// ```rust
/// todo!();
/// ```
pub trait ToDelimNode: ToNode {
    /// The [`railroad::Node`]-type to which this AST node compiles when asked for only its opening half.
    type NodeOpen: railroad::Node;
    /// The [`railroad::Node`]-type to which this AST node compiles when asked for only its closing half.
    type NodeClose: railroad::Node;


    /// Generates a [`railroad::Node`] for this AST node that represents its opening half.
    ///
    /// # Returns
    /// An [`railroad::Node`] of type [Self::NodeOpen](ToNode::NodeOpen) that represents the railroad-counterpart to Self's opening half.
    fn railroad_open() -> Self::NodeOpen;

    /// Generates a [`railroad::Node`] for this AST node that represents its closing half.
    ///
    /// # Returns
    /// An [`railroad::Node`] of type [Self::NodeClose](ToNode::NodeClose) that represents the railroad-counterpart to Self's closing half.
    fn railroad_close() -> Self::NodeClose;
}





/***** DEFAULT IMPLS *****/
// Simple propagation for `Box`
impl<T: ToNode> ToNode for Box<T> {
    type Node = T::Node;

    #[inline]
    fn railroad() -> Self::Node { T::railroad() }
}
impl<T: ToNonTerm> ToNonTerm for Box<T> {
    type NodeNonTerm = T::NodeNonTerm;

    #[inline]
    fn railroad_nonterm() -> Self::NodeNonTerm { T::railroad_nonterm() }
}
impl<T: ToDelimNode> ToDelimNode for Box<T> {
    type NodeClose = T::NodeClose;
    type NodeOpen = T::NodeOpen;

    #[inline]
    fn railroad_open() -> Self::NodeOpen { T::railroad_open() }

    #[inline]
    fn railroad_close() -> Self::NodeClose { T::railroad_close() }
}
