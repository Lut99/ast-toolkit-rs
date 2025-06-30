//  KEY.rs
//    by Lut99
//
//  Description:
//!   Implements the helper [`Key`] pointer wrapper.
//!
//!   It's not a complete coincidence this lives in its own module. The point
//!   is that creating one of the objects is an `unsafe` operation, because it
//!   requires making guarantees for the lifetime of an object.
//

use std::hash::{Hash, Hasher};


/***** LIBRARY *****/
/// A wrapper around an `I`-pointer to properly implement hash semantics.
///
/// # Safety
/// This struct relies on the fact that the pointer is valid upon use! So don't delete the
/// underlying memory until you deleted this wrapper - or promise to never call an impl until you
/// do.
///
/// It's therefore important that this struct _doesn't_ implement [`Clone`] or anything, as we
/// want to be aware where it lives.
pub(super) struct Key<I>(*const I);

// Constructors
impl<I> Key<I> {
    /// Constructor for the Key.
    ///
    /// # Safety
    /// This function is unsafe because we need to rely on safe functions in [`Hash`] and
    /// [`PartialEq`] to be compatible with a [`HashMap`]. Hence, for as long as the returned
    /// object exists, you MUST enforce that **the given pointer points to a valid instance of
    /// `I`.** There is no handholding in the usage of this struct.
    ///
    /// # Arguments
    /// - `id`: A pointer to the identifier we're wrapping.
    ///
    /// # Returns
    /// A new Key that wraps the pointer such that we can use it directly in a [`HashMap`].
    #[inline]
    pub const unsafe fn new(id: *const I) -> Self { Self(id) }

    /// Clones this Key into a new one.
    ///
    /// # Safety
    /// This function is unsafe because we need to rely on safe functions in [`Hash`] and
    /// [`PartialEq`] to be compatible with a [`HashMap`]. Hence, for as long as the returned
    /// object exists, you MUST enforce that **the given pointer points to a valid instance of
    /// `I`.** There is no handholding in the usage of this struct.
    ///
    /// # Returns
    /// A new Key that wraps the same pointer as `self`.
    #[inline]
    pub const unsafe fn duplicate(&self) -> Self { Self(self.0) }
}

// Key
impl<I> Key<I> {
    /// Returns the internal pointer.
    ///
    /// Note that this function is actually not unsafe, because we have already accepted the
    /// fact that it must be while this object exists.
    ///
    /// # Returns
    /// A `*mut I` referencing the internal pointer.
    #[inline]
    pub const fn as_ptr(&self) -> *const I { self.0 }

    /// Returns the internal pointer, giving up this struct.
    ///
    /// This function is actually not unsafe, because it encodes a _release_ of restrictions.
    /// After this, do with the underlying memory what you want :)
    ///
    /// # Returns
    /// A `*mut I` referencing the internal pointer.
    #[inline]
    pub const fn into_ptr(self) -> *const I { self.0 }
}

// Ops
impl<I: Eq> Eq for Key<I> {}
impl<I: Hash> Hash for Key<I> {
    #[inline]
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        // SAFETY: By using this struct, the user has already promised that `I` is valid. Sadly,
        // that's as fine-grained as we go in terms of upholding that guarantee.
        unsafe { &*self.0 }.hash(hasher);
    }
}
impl<I: PartialEq> PartialEq for Key<I> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        // SAFETY: By using this struct, the user has already promised that `I` is valid. Sadly,
        // that's as fine-grained as we go in terms of upholding that guarantee.
        unsafe { &*self.0 }.eq(unsafe { &*other.0 })
    }

    #[inline]
    fn ne(&self, other: &Self) -> bool {
        // SAFETY: By using this struct, the user has already promised that `I` is valid. Sadly,
        // that's as fine-grained as we go in terms of upholding that guarantee.
        unsafe { &*self.0 }.ne(unsafe { &*other.0 })
    }
}
