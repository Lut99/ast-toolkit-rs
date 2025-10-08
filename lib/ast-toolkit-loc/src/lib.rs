//  LIB.rs
//    by Lut99
//
//  Description:
//!   Defines the [`Loc`], a loving (but actually working) tribute to the
//!   now-deprecated `Span` from `ast-toolkit-span`.
//!
//!   # Rationale
//!   The main reason why `Span`s weren't working is that they were designed
//!   with two errors:
//!   - `Span`s carried source references in order to provide them at compile
//!     time (e.g., identifier names). This, while efficient, is actually
//!     absolutely HORRIBLE because it essentially prevents you to do any code
//!     generation.
//!   - `Span`s carried hard slices to source references so that there could be
//!     no confusion when they're generated to text. This is actually HORRIBLE,
//!     because the ENTIRE compiler is poluted with generics and lifetimes for
//!     a moment only occurring during error handling (which was screwed up
//!     too bc of the lifetimes). So yeah. Bad idea.
//!
//!   This will solve it by going for a more classic approach to source
//!   references: we just do it for debugging, so a [`Loc`] is just a location
//!   to a source text that can be printed in combination _with_ that source
//!   text.
//!
//!   This time, a lot of effort is being put into making them easy to work
//!   with during the bulk of the compiler. That is, any worry about there not
//!   being any location to point to or it being a merge of different sources
//!   is left internalized to the [`Loc`].
//

// Modules
mod range;

// Imports
use std::fmt::Debug;
use std::hash::{Hash, Hasher};

pub use range::Range;


/***** LIBRARY *****/
/// Representation of a contigious slice of source text.
///
/// It is the spiritual successor of a `Span`, which carries the same information but also
/// internalizes references to the source so that the area is actually accessible. This is not the
/// case for the Loc, which instead only becomes useful when combined with the original source
/// text.
///
/// Until then, the Loc provides the following while carrying it around:
/// - You can easily create new, (meaningless) Locs for e.g. code generation;
/// - You can easily compose them, whether they are from the same source or not; and
/// - They are cheap and easy to carry around. No [`Clone`]ing, no generics, no lifetimes!
///
/// # Comparing Locs
/// Note that [`Eq`], [`Hash`] and [`PartialEq`] are all implemented for Locs but do nothing (i.e.,
/// all Locs are reported to be the same). This to make e.g. deriving them on parent structs much easier.
#[derive(Clone, Copy, Debug)]
pub struct Loc {
    /// Some unique ID (e.g., a hash) of the source text this was from.
    pub source: Option<u64>,
    /// The range that does the slicing.
    pub range:  Range,
}

// Constructors
impl Default for Loc {
    #[inline]
    fn default() -> Self { Self::new() }
}
impl Loc {
    /// Creates a new Loc that points to nothing.
    ///
    /// # Returns
    /// A Loc that doesn't point to source text whatsoever.
    #[inline]
    pub const fn new() -> Self { Self { source: None, range: Range::Empty } }

    /// Creates a new Loc that points to a source with the given identifier.
    ///
    /// It will initially point to the whole source. See [`Loc::ranged()`] if you want to start
    /// with a specific subset of it instead.
    ///
    /// # Arguments
    /// - `id`: Some identifier (as a [`u64`]) that is **unique for this source.** This is used to
    ///   determine which sources are mergable (i.e., Locs with the same source ID are assumed to
    ///   have the same "range" or "namespace" they span).
    ///
    /// # Returns
    /// A Loc that points to a source with the given `id`, and spans it in its entirety.
    #[inline]
    pub const fn with_source(id: u64) -> Self { Self { source: Some(id), range: Range::Full } }

    /// Creates a new Loc that points to a source with the given identifier, and a specific subset
    /// of it.
    ///
    /// See [`Loc::with_source()`] if you are fine with this Loc spanning the entire source
    /// instead.
    ///
    /// # Arguments
    /// - `id`: Some identifier (as a [`u64`]) that is **unique for this source.** This is used to
    ///   determine which sources are mergable (i.e., Locs with the same source ID are assumed to
    ///   have the same "range" or "namespace" they span).
    /// - `range`: Some [`Range`]-like that determines which subset of the source to do. Note that
    ///   this isn't yet checked; it is just remembered until later.
    ///
    /// # Returns
    /// A Loc that points to a source with the given `id` and spans a subset in the given `range`
    /// of it.
    #[inline]
    pub fn ranged(id: u64, range: impl Into<Range>) -> Self { Self { source: Some(id), range: range.into() } }
}

// Ops
impl Eq for Loc {}
impl Hash for Loc {
    /// WARNING: Note that this function does nothing, as it considers all Locs to be equivalent
    /// from an AST perspective.
    ///
    /// It exists to make deriving this trait on a parent struct easier and harmless.
    #[inline]
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}
impl PartialEq for Loc {
    /// WARNING: Note that this function **always** returns true, as it considers all Locs to be
    /// equivalent from an AST perspective.
    ///
    /// It exists to make deriving this trait on a parent struct easier and harmless.
    #[inline]
    fn eq(&self, _other: &Self) -> bool { true }
    /// WARNING: Note that this function **always** returns false, as it considers all Locs to be
    /// equivalent from an AST perspective.
    ///
    /// It exists to make deriving this trait on a parent struct easier and harmless.
    #[inline]
    fn ne(&self, _other: &Self) -> bool { false }
}
