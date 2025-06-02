//  LIB.rs
//    by Lut99
//
//  Created:
//    25 Feb 2024, 11:05:34
//  Last edited:
//    28 Nov 2024, 13:45:57
//  Auto updated?
//    Yes
//
//  Description:
//!   Provides traits for recursively generating [railroad](https://crates.io/crates/railroad/0.2.0) diagrams.
//!
//!   Comes with a derive macro for convenience.
//

use std::cell::{Ref, RefMut};
use std::collections::HashSet;
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};

#[cfg(feature = "derive")]
pub use ast_toolkit_railroad_derive::{ToDelimNode, ToNode, ToNonTerm};
pub use railroad;


/***** HELPER MACROS *****/
/// Implements a default propagation for all three traits for some type.
macro_rules! propagate_impl {
    ($name:ty) => {
        impl<T: ToNode> ToNode for $name {
            type Node = T::Node;

            #[inline]
            fn railroad() -> Self::Node { T::railroad() }
        }
        impl<T: ToNonTerm> ToNonTerm for $name {
            type NodeNonTerm = T::NodeNonTerm;

            #[inline]
            fn railroad_nonterm() -> Self::NodeNonTerm { T::railroad_nonterm() }
        }
        impl<T: ToDelimNode> ToDelimNode for $name {
            type NodeClose = T::NodeClose;
            type NodeOpen = T::NodeOpen;

            #[inline]
            fn railroad_open() -> Self::NodeOpen { T::railroad_open() }

            #[inline]
            fn railroad_close() -> Self::NodeClose { T::railroad_close() }
        }
    };
    (lifetime $name:ty) => {
        impl<'a, T: ToNode> ToNode for $name {
            type Node = T::Node;

            #[inline]
            fn railroad() -> Self::Node { T::railroad() }
        }
        impl<'a, T: ToNonTerm> ToNonTerm for $name {
            type NodeNonTerm = T::NodeNonTerm;

            #[inline]
            fn railroad_nonterm() -> Self::NodeNonTerm { T::railroad_nonterm() }
        }
        impl<'a, T: ToDelimNode> ToDelimNode for $name {
            type NodeClose = T::NodeClose;
            type NodeOpen = T::NodeOpen;

            #[inline]
            fn railroad_open() -> Self::NodeOpen { T::railroad_open() }

            #[inline]
            fn railroad_close() -> Self::NodeClose { T::railroad_close() }
        }
    };
}





/***** LIBRARY MACROS *****/
/// Generates the implementation to create a diagram out of the given nodes.
///
/// Note that the nodes must implement [`NonTerm`] instead of just [`ToNode`].
///
/// Also note that this version returns a plain diagram that must be serialized further. See [`diagram_svg!`] or [`diagram_file!`] for more convenient methods.
///
/// # Arguments
/// A comma-separated list of types referring to the types to generates the toplevel of the diagram.
///
/// # Returns
/// A [`Diagram`](railroad::Diagram) that encodes the given nodes. You still have to make a graphic out of it yourself.
///
/// # Example
/// ```rust
/// use ast_toolkit_railroad::{ToNode, ToNonTerm, diagram, railroad as rr};
///
/// struct Example;
/// impl ToNode for Example {
///     type Node = rr::NonTerminal;
///
///     fn railroad() -> Self::Node { rr::NonTerminal::new("Example".into()) }
/// }
/// impl ToNonTerm for Example {
///     type NodeNonTerm = rr::Terminal;
///
///     fn railroad_nonterm() -> Self::NodeNonTerm { rr::Terminal::new("example".into()) }
/// }
///
/// // Generate a diagram
/// let mut diag: rr::Diagram<_> = diagram!(Example);
/// // Add an SVG serialization and write the HTML to stdout
/// diag.add_element(rr::svg::Element::new("style").set("type", "text/css").text(rr::DEFAULT_CSS));
/// println!("{diag}");
/// ```
#[macro_export]
macro_rules! diagram {
    ($($nodes:ty $(as $names:literal)?),+ $(,)?) => {
        {
            let mut items: ::std::vec::Vec<::std::boxed::Box<dyn $crate::railroad::Node>> = ::std::vec::Vec::new();
            $crate::_diagram!(items, $($nodes $(as $names)?),+);
            $crate::railroad::Diagram::new($crate::railroad::VerticalGrid::new(items))
        }
    };
}
/// Does recursive list unfolding for [`diagram!()`].
///
/// Not meant to be used directly.
#[doc(hidden)]
#[macro_export]
macro_rules! _diagram {
    ($list:ident, $node:ty as $name:literal) => {
        $list.push(::std::boxed::Box::new($crate::railroad::Sequence::<::std::boxed::Box<dyn $crate::railroad::Node>>::new(::std::vec! [
            ::std::boxed::Box::new($crate::railroad::Comment::new($name.into())),
            ::std::boxed::Box::new($crate::railroad::Start),
            ::std::boxed::Box::new(<$node as $crate::ToNonTerm>::railroad_nonterm()),
            ::std::boxed::Box::new($crate::railroad::End),
        ])))
    };
    ($list:ident, $node:ty) => {
        $list.push(::std::boxed::Box::new($crate::railroad::Sequence::<::std::boxed::Box<dyn $crate::railroad::Node>>::new(::std::vec! [
            ::std::boxed::Box::new($crate::railroad::Comment::new(::std::stringify!($node).into())),
            ::std::boxed::Box::new($crate::railroad::Start),
            ::std::boxed::Box::new(<$node as $crate::ToNonTerm>::railroad_nonterm()),
            ::std::boxed::Box::new($crate::railroad::End),
        ])))
    };
    ($list:ident, $node:ty $(as $name:literal)?, $($nodes:ty $(as $names:literal)?),+) => {
        $crate::_diagram!($list, $node $(as $name)?);
        $crate::_diagram!($list, $($nodes $(as $names)?),+);
    };
}

/// Generates the implementation to create a diagram out of the given nodes.
///
/// Note that the nodes must implement [`NonTerm`] instead of just [`ToNode`].
///
/// This version returns a diagram that already has an SVG serialization attached to it. See [`diagram!`] for a more flexible implementation, or [`diagram_file!`] for a more convenient method.
///
/// # Arguments
/// A comma-separated list of types referring to the types to generates the toplevel of the diagram.
///
/// # Returns
/// A [`Diagram`](railroad::Diagram) that encodes the given nodes, with SVG already attached
///
/// # Example
/// ```rust
/// use ast_toolkit_railroad::{ToNode, ToNonTerm, diagram_svg, railroad as rr};
///
/// struct Example;
/// impl ToNode for Example {
///     type Node = rr::NonTerminal;
///
///     fn railroad() -> Self::Node { rr::NonTerminal::new("Example".into()) }
/// }
/// impl ToNonTerm for Example {
///     type NodeNonTerm = rr::Terminal;
///
///     fn railroad_nonterm() -> Self::NodeNonTerm { rr::Terminal::new("example".into()) }
/// }
///
/// // Generate a diagram and write it as HTML to stdout
/// let diag: rr::Diagram<_> = diagram_svg!(Example);
/// println!("{diag}");
/// ```
#[macro_export]
macro_rules! diagram_svg {
    ($($nodes:tt)+) => {
        {
            let mut diag = $crate::diagram!($($nodes)+);
            diag.add_element($crate::railroad::svg::Element::new("style").set("type", "text/css").text($crate::railroad::DEFAULT_CSS));
            diag
        }
    };
}

/// Generates the implementation to create a diagram out of the given nodes, then writes it to the given path as an SVG.
///
/// Note that the nodes must implement [`NonTerm`] instead of just [`ToNode`].
///
/// This version writes the diagram immediately. See [`diagram!`] or [`diagram_svg!`] for more flexible methods.
///
/// # Arguments
/// A path, a comma, and then a comma-separated list of types referring to the types to generates the toplevel of the diagram.
///
/// # Returns
/// A [`Result<(), std::io::Error>`](std::io::Error) that encodes the success state of writing to the file.
///
/// # Example
/// ```rust
/// use ast_toolkit_railroad::{ToNode, ToNonTerm, diagram_svg_file, railroad as rr};
///
/// struct Example;
/// impl ToNode for Example {
///     type Node = rr::NonTerminal;
///
///     fn railroad() -> Self::Node { rr::NonTerminal::new("Example".into()) }
/// }
/// impl ToNonTerm for Example {
///     type NodeNonTerm = rr::Terminal;
///
///     fn railroad_nonterm() -> Self::NodeNonTerm { rr::Terminal::new("example".into()) }
/// }
///
/// // Generate a diagram and write it as HTML to stdout
/// if let Err(err) = diagram_svg_file!(std::env::temp_dir().join("test.svg"), Example) {
///     eprintln!("ERROR: {err}");
/// }
/// ```
#[macro_export]
macro_rules! diagram_svg_file {
    ($path:expr, $($nodes:tt)+) => {
        {
            let diag = $crate::diagram_svg!($($nodes)+);
            ::std::fs::write($path, diag.to_string())
        }
    };
}





/***** LIBRARY *****/
/// Trait that generalizes over AST nodes such that they can hiearchically create [`railroad`] diagrams.
///
/// See the [`ToNode`](crate::procedural::ToNode)-derive macro to make implementing this trait for structs & enums very convenient.
pub trait ToNode {
    /// The [`railroad::Node`]-type to which this AST node compiles.
    type Node: 'static + railroad::Node;


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
pub trait ToNonTerm: ToNode {
    /// The [`railroad::Node`]-type to which this AST node compiles when asked for its full representation.
    type NodeNonTerm: 'static + railroad::Node;


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
pub trait ToDelimNode: ToNode {
    /// The [`railroad::Node`]-type to which this AST node compiles when asked for only its opening half.
    type NodeOpen: 'static + railroad::Node;
    /// The [`railroad::Node`]-type to which this AST node compiles when asked for only its closing half.
    type NodeClose: 'static + railroad::Node;


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
// Simple propagation
propagate_impl!(&T);
propagate_impl!(&mut T);
propagate_impl!(Box<T>);
propagate_impl!(Rc<T>);
propagate_impl!(lifetime Ref<'a, T>);
propagate_impl!(lifetime RefMut<'a, T>);
propagate_impl!(Arc<T>);
propagate_impl!(lifetime MutexGuard<'a, T>);
propagate_impl!(lifetime RwLockReadGuard<'a, T>);
propagate_impl!(lifetime RwLockWriteGuard<'a, T>);
#[cfg(feature = "parking-lot")]
propagate_impl!(lifetime parking_lot::MutexGuard<'a, T>);
#[cfg(feature = "parking-lot")]
propagate_impl!(lifetime parking_lot::RwLockReadGuard<'a, T>);
#[cfg(feature = "parking-lot")]
propagate_impl!(lifetime parking_lot::RwLockWriteGuard<'a, T>);

// Propagation for `Option`, but with optional tracks around it.
impl<T: ToNode> ToNode for Option<T> {
    type Node = railroad::Optional<T::Node>;

    #[inline]
    fn railroad() -> Self::Node { railroad::Optional::new(T::railroad()) }
}
impl<T: ToNonTerm> ToNonTerm for Option<T> {
    type NodeNonTerm = railroad::Optional<T::NodeNonTerm>;

    #[inline]
    fn railroad_nonterm() -> Self::NodeNonTerm { railroad::Optional::new(T::railroad_nonterm()) }
}
impl<T: ToDelimNode> ToDelimNode for Option<T> {
    type NodeClose = railroad::Optional<T::NodeClose>;
    type NodeOpen = railroad::Optional<T::NodeOpen>;

    #[inline]
    fn railroad_open() -> Self::NodeOpen { railroad::Optional::new(T::railroad_open()) }

    #[inline]
    fn railroad_close() -> Self::NodeClose { railroad::Optional::new(T::railroad_close()) }
}

// Propagation for `Vec`, which repeats the element indefinitely.
// NOTE: No ToDelimNode, as this doesn't really make sense.
impl<T: ToNode> ToNode for Vec<T> {
    type Node = railroad::Repeat<T::Node, railroad::Empty>;

    #[inline]
    fn railroad() -> Self::Node { railroad::Repeat::new(T::railroad(), railroad::Empty) }
}
impl<T: ToNonTerm> ToNonTerm for Vec<T> {
    type NodeNonTerm = railroad::Repeat<T::NodeNonTerm, railroad::Empty>;

    #[inline]
    fn railroad_nonterm() -> Self::NodeNonTerm { railroad::Repeat::new(T::railroad_nonterm(), railroad::Empty) }
}
// Propagation for `HashSet`, which repeats the element indefinitely.
// NOTE: No ToDelimNode, as this doesn't really make sense.
impl<T: ToNode> ToNode for HashSet<T> {
    type Node = railroad::Repeat<T::Node, railroad::Empty>;

    #[inline]
    fn railroad() -> Self::Node { railroad::Repeat::new(T::railroad(), railroad::Empty) }
}
impl<T: ToNonTerm> ToNonTerm for HashSet<T> {
    type NodeNonTerm = railroad::Repeat<T::NodeNonTerm, railroad::Empty>;

    #[inline]
    fn railroad_nonterm() -> Self::NodeNonTerm { railroad::Repeat::new(T::railroad_nonterm(), railroad::Empty) }
}
