//  SPANNABLE.rs
//    by Lut99
//
//  Created:
//    06 May 2024, 16:19:49
//  Last edited:
//    27 May 2024, 13:24:09
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`Spannable`] trait, which abstracts over things that
//!   we can put as `S`ource string in Spans.
//

use std::borrow::Cow;
use std::rc::Rc;
use std::sync::Arc;

use crate::range::{index_range_bound, SpanRange};


/***** LIBRARY *****/
/// A helper trait for the [`Span`] that can be implemented for anything used as input.
pub trait Spannable {
    type Slice<'s>: 's
    where
        Self: 's;

    /// Checks if this Spannable is the same (for all intends and purposes) as another Spannable of the same type.
    ///
    /// While it suffices to show that two [`Span`]s are the same semantically (e.g., using a simple byte-wise compare), it's also allowed to compare by pointer equality if possible for performance. Implementations for [`&[u8]`] and [`&str`] do this, for example.
    ///
    /// # Arguments
    /// - `other`: Some other Spannable of type Self to check with.
    ///
    /// # Returns
    /// True if these Spannables are the same, or false otherwise.
    fn is_same(&self, other: &Self) -> bool;

    /// Slices this Spannable by raw index.
    ///
    /// # Arguments
    /// - `range`: The range that slices this Spannable. Can be anything implementing [`RangeBounds`].
    ///
    /// # Returns
    /// A new instance of type `Self::Slice`, that is self but sliced.
    ///
    /// # Panics
    /// This function panics if out-of-bounds.
    fn slice<'s>(&'s self, range: SpanRange) -> Self::Slice<'s>;

    /// Returns the number of currently spanned "raw" items (e.g., bytes).
    ///
    /// # Returns
    /// A [`usize`] with the total number of bytes or other elementary items as is stored on-disk.
    fn byte_len(&self) -> usize;
}

// Default binary impls for [`Spannable`]
impl Spannable for [u8] {
    type Slice<'s> = &'s [u8] where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        let ptr_eq: bool = std::ptr::eq(self, other);
        #[cfg(debug_assertions)]
        {
            if !ptr_eq && self == other {
                eprintln!(
                    "DEBUG ASSERTION WARNING: Two byte arrays do not share the same pointer but are semantically equal. The &[u8]-implementation \
                     for Spannable assumes comparing them by pointer equality is sufficient."
                );
            }
        }
        ptr_eq
    }

    #[inline]
    #[track_caller]
    fn slice<'s2>(&'s2 self, range: SpanRange) -> Self::Slice<'s2> { index_range_bound!(self, range) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl<const LEN: usize> Spannable for [u8; LEN] {
    type Slice<'s> = &'s [u8] where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        let ptr_eq: bool = std::ptr::eq(self, other);
        #[cfg(debug_assertions)]
        {
            if !ptr_eq && self == other {
                eprintln!(
                    "DEBUG ASSERTION WARNING: Two byte arrays do not share the same pointer but are semantically equal. The &[u8; \
                     LEN]-implementation for Spannable assumes comparing them by pointer equality is sufficient."
                );
            }
        }
        ptr_eq
    }

    #[inline]
    #[track_caller]
    fn slice<'s>(&'s self, range: SpanRange) -> Self::Slice<'s> { index_range_bound!(self, range) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl<'b> Spannable for Cow<'b, [u8]> {
    type Slice<'s> = Cow<'s, [u8]> where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        match (self, other) {
            // Compare by pointer if possible
            (Self::Borrowed(b1), Self::Borrowed(b2)) => {
                let ptr_eq: bool = std::ptr::eq(*b1, *b2);
                #[cfg(debug_assertions)]
                {
                    if !ptr_eq && self == other {
                        eprintln!(
                            "DEBUG ASSERTION WARNING: Two byte arrays do not share the same pointer but are semantically equal. The \
                             Cow<u8>-implementation for Spannable assumes comparing them by pointer equality if they're both borrowed is sufficient."
                        );
                    }
                }
                ptr_eq
            },
            // Otherwise, fall back to equality testing
            (o1, o2) => o1 == o2,
        }
    }

    #[inline]
    #[track_caller]
    fn slice<'s2>(&'s2 self, range: SpanRange) -> Self::Slice<'s2> { Cow::Borrowed(index_range_bound!(self, range)) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl Spannable for Vec<u8> {
    type Slice<'s> = &'s [u8] where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool { self == other }

    #[inline]
    #[track_caller]
    fn slice<'s2>(&'s2 self, range: SpanRange) -> Self::Slice<'s2> { index_range_bound!(self, range) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl Spannable for Rc<[u8]> {
    type Slice<'s> = &'s [u8] where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        let ptr_eq: bool = Rc::ptr_eq(self, other);
        #[cfg(debug_assertions)]
        {
            if !ptr_eq && self == other {
                eprintln!(
                    "DEBUG ASSERTION WARNING: Two reference-counter byte arrays do not share the same pointer but are semantically equal. The \
                     Rc<[u8]>-implementation for Spannable assumes comparing them by pointer equality is sufficient."
                );
            }
        }
        ptr_eq
    }

    #[inline]
    fn slice<'s>(&'s self, range: SpanRange) -> Self::Slice<'s> { index_range_bound!(self, range) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl Spannable for Arc<[u8]> {
    type Slice<'s> = &'s [u8] where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        let ptr_eq: bool = Arc::ptr_eq(self, other);
        #[cfg(debug_assertions)]
        {
            if !ptr_eq && self == other {
                eprintln!(
                    "DEBUG ASSERTION WARNING: Two reference-counter byte arrays do not share the same pointer but are semantically equal. The \
                     Arc<[u8]>-implementation for Spannable assumes comparing them by pointer equality is sufficient."
                );
            }
        }
        ptr_eq
    }

    #[inline]
    fn slice<'s>(&'s self, range: SpanRange) -> Self::Slice<'s> { index_range_bound!(self, range) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}

// Default string impls for [`Spannable`]
impl Spannable for str {
    type Slice<'s> = &'s str where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        let ptr_eq: bool = std::ptr::eq(self, other);
        #[cfg(debug_assertions)]
        {
            if !ptr_eq && self == other {
                eprintln!(
                    "DEBUG ASSERTION WARNING: Two string slices do not share the same pointer but are semantically equal. The &str-implementation \
                     for Spannable assumes comparing them by pointer equality is sufficient."
                );
            }
        }
        ptr_eq
    }

    #[inline]
    #[track_caller]
    fn slice<'s>(&'s self, range: SpanRange) -> Self::Slice<'s> { index_range_bound!(self, range) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl<'s> Spannable for Cow<'s, str> {
    type Slice<'s2> = Cow<'s2, str> where Self: 's2;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        match (self, other) {
            // Compare by pointer if possible
            (Self::Borrowed(b1), Self::Borrowed(b2)) => {
                let ptr_eq: bool = std::ptr::eq(*b1, *b2);
                #[cfg(debug_assertions)]
                {
                    if !ptr_eq && self == other {
                        eprintln!(
                            "DEBUG ASSERTION WARNING: Two string slices do not share the same pointer but are semantically equal. The \
                             Cow<str>-implementation for Spannable assumes comparing them by pointer equality if they're both borrowed is \
                             sufficient."
                        );
                    }
                }
                ptr_eq
            },
            // Otherwise, fall back to equality testing
            (o1, o2) => o1 == o2,
        }
    }

    #[inline]
    #[track_caller]
    fn slice<'s2>(&'s2 self, range: SpanRange) -> Self::Slice<'s2> { Cow::Borrowed(index_range_bound!(self, range)) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl Spannable for String {
    type Slice<'s> = &'s str where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool { self == other }

    #[inline]
    #[track_caller]
    fn slice<'s>(&'s self, range: SpanRange) -> Self::Slice<'s> { index_range_bound!(self, range) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl Spannable for Rc<str> {
    type Slice<'s> = &'s str where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        let ptr_eq: bool = Rc::ptr_eq(self, other);
        #[cfg(debug_assertions)]
        {
            if !ptr_eq && self == other {
                eprintln!(
                    "DEBUG ASSERTION WARNING: Two reference-counter byte arrays do not share the same pointer but are semantically equal. The \
                     Rc<str>-implementation for Spannable assumes comparing them by pointer equality is sufficient."
                );
            }
        }
        ptr_eq
    }

    #[inline]
    fn slice<'s>(&'s self, range: SpanRange) -> Self::Slice<'s> { index_range_bound!(self, range) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl Spannable for Arc<str> {
    type Slice<'s> = &'s str where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        let ptr_eq: bool = Arc::ptr_eq(self, other);
        #[cfg(debug_assertions)]
        {
            if !ptr_eq && self == other {
                eprintln!(
                    "DEBUG ASSERTION WARNING: Two reference-counter byte arrays do not share the same pointer but are semantically equal. The \
                     Arc<str>-implementation for Spannable assumes comparing them by pointer equality is sufficient."
                );
            }
        }
        ptr_eq
    }

    #[inline]
    fn slice<'s>(&'s self, range: SpanRange) -> Self::Slice<'s> { index_range_bound!(self, range) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}

// Default pointer-like impls
impl<'t, T: ?Sized + Spannable> Spannable for &'t T {
    type Slice<'s> = T::Slice<'s> where Self: 's;

    #[inline]
    #[track_caller]
    fn is_same(&self, other: &Self) -> bool { T::is_same(self, other) }

    #[inline]
    #[track_caller]
    fn slice<'s>(&'s self, range: SpanRange) -> Self::Slice<'s> { T::slice(self, range) }

    #[inline]
    #[track_caller]
    fn byte_len(&self) -> usize { T::byte_len(self) }
}
