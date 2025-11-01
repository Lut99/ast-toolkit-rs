//  INPUT.rs
//    by Lut99
//
//  Description:
//!   Provides an abstraction over ([`Located`]) inputs such that they may be parsed.
//

use std::borrow::Cow;
use std::cell::{Ref, RefMut};
use std::convert::Infallible;
use std::error::Error;
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};

use ast_toolkit_loc::{Length, Loc, Range};


/***** HELPER MACROS *****/
/// Does implementations of pointer-like types for [`Identifier`].
macro_rules! identifier_ptr_impl {
    ('a,Cow < 'a,T >) => {
        impl<'a, T: ?Sized + Identifier + ToOwned> Identifier for Cow<'a, T> {
            #[inline]
            #[track_caller]
            fn id(&self) -> u64 { <T as Identifier>::id(self) }
        }
    };
    ('a, $ty:ty) => {
        impl<'a, T: ?Sized + Identifier> Identifier for $ty {
            #[inline]
            #[track_caller]
            fn id(&self) -> u64 { <T as Identifier>::id(self) }
        }
    };
    ($ty:ty) => {
        impl<T: ?Sized + Identifier> Identifier for $ty {
            #[inline]
            #[track_caller]
            fn id(&self) -> u64 { <T as Identifier>::id(self) }
        }
    };
}

/// Does implementations of pointer-like types for [`Source`].
macro_rules! source_ptr_impl {
    /* Private interface */
    (__BODY) => {
        type Elem = <T as Source>::Elem;
        type Error = <T as Source>::Error;

        #[inline]
        #[track_caller]
        fn id(&self) -> u64 { <T as Source>::id(self) }

        #[inline]
        #[track_caller]
        fn count_in_slice_while<'s>(&'s self, range: Range, pred: impl FnMut(&'s Self::Elem) -> bool) -> Result<u64, Self::Error> {
            <T as Source>::count_in_slice_while(self, range, pred)
        }

        #[inline]
        #[track_caller]
        fn len(&self) -> u64 { <T as Source>::len(self) }
    };

    /* Public interface */
    ('a,Cow < 'a,T >) => {
        impl<'a, T: ?Sized + Source + ToOwned> Source for Cow<'a, T> {
            source_ptr_impl!(__BODY);
        }
    };
    ('a, $ty:ty) => {
        impl<'a, T: ?Sized + Source> Source for $ty {
            source_ptr_impl!(__BODY);
        }
    };
    ($ty:ty) => {
        impl<T: ?Sized + Source> Source for $ty {
            source_ptr_impl!(__BODY);
        }
    };
}





/***** INTERFACES *****/
/// Represents things acting as identifiers.
///
/// An identifier is something that uniquely points to something (in our case, a source text).
///
/// It's important that, per instance of your identifier, it returns a **unique value for your
/// scope.** Usually, this means that they must be unique within a single compiler run.
pub trait Identifier {
    /// Returns this identifier as a number.
    ///
    /// This is what is really passed around in the compiled.
    ///
    /// # Returns
    /// A [`u64`] encoding a hash or other unique representation of this identifier.
    fn id(&self) -> u64;
}

// Std impls
impl<T> Identifier for [T] {
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }
}
impl<T> Identifier for Vec<T> {
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }
}
impl Identifier for str {
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }
}
impl Identifier for String {
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }
}

// Pointer-like impls
identifier_ptr_impl!('a, &'a T);
identifier_ptr_impl!('a, &'a mut T);
identifier_ptr_impl!('a, Cow<'a, T>);
identifier_ptr_impl!(Box<T>);
identifier_ptr_impl!(Rc<T>);
identifier_ptr_impl!(Arc<T>);
identifier_ptr_impl!('a, Ref<'a, T>);
identifier_ptr_impl!('a, RefMut<'a, T>);
identifier_ptr_impl!('a, MutexGuard<'a, T>);
identifier_ptr_impl!('a, RwLockReadGuard<'a, T>);
identifier_ptr_impl!('a, RwLockWriteGuard<'a, T>);



/// Represents things that can be used as input.
///
/// This is like [`Read`], except that it is not necessarily byte-specific or sequential. It
/// represents the input as some array over some kind of arbitrary [element](Source::Elem), that
/// will be [indexed](Source::get()) based on the current parsing position.
///
/// If the entire source is _not_ loaded in memory, then it is expected that the implementation
/// dynamically loads- and unloads it as necessary indicated by the indices accessed. For
/// efficiency purposes, `snack` parsers may call [`Source::forget_up_to()`] to hint that they will
/// not access anything before the given index anymore.
pub trait Source {
    /// The type of items yielded by the parsable stream.
    type Elem;
    /// The type of errors thrown when [`Parsable::next()`]'ing the next item.
    type Error: Error;


    /// Returns some identifier that is used to mark [`Loc`]s with.
    ///
    /// This must be something unique within your run. E.g., different files should have different
    /// IDs.
    ///
    /// # Returns
    /// Some [`u64`] that represents this source's ID.
    fn id(&self) -> u64;

    /// Counts the number of elements from a given index onwards that match some predicate.
    ///
    /// This sounds super niche. However, it's the cornerstone of the `snack` library: a [`Span`]
    /// will refer to a specific position in the array, and then wants to parse elements as long as
    /// they meet certain conditions. This function will implement that logic for your array.
    ///
    /// Note, however, that any mutability you might like (e.g., buffers) would have to be
    /// interior. This because copies referring to your Source will be many and interleaved.
    /// Luckily, these are never accessed concurrently, so you should be able to use an e.g.
    /// [`RefCell`](std::cell::RefCell).
    ///
    /// # Arguments
    /// - `range`: Some [`Range`] slicing `self` to indicate the area in which to count. If this
    ///   area is empty or out-of-bounds, you should return `Ok(0)`.
    /// - `pred`: Some [`FnMut`] encoding a predicate for matching elements as long as possible.
    ///
    /// # Returns
    /// The number of elements for which `pred` yielded true.
    ///
    /// # Errors
    /// This function can error if something went horribly, horribly wrong while attempting to get
    /// an `index`ed element.
    fn count_in_slice_while<'s>(&'s self, range: Range, pred: impl FnMut(&'s Self::Elem) -> bool) -> Result<u64, Self::Error>;

    /// Returns the total length of the source text.
    ///
    /// # Returns
    /// A [`u64`] encoding the total number of elements.
    fn len(&self) -> u64;

    /// Alias for checking if the [`Source::len()`] is nothing.
    ///
    /// # Returns
    /// True when `Source::len() == 0`, false otherwise.
    #[inline]
    fn is_empty(&self) -> bool { self.len() == 0 }
}

// Std impls
impl<T> Source for [T] {
    type Elem = T;
    type Error = Infallible;

    /// NOTE: This implementation will just assume the pointer to the original object as the ID.
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }

    #[inline]
    fn count_in_slice_while<'s>(&'s self, range: Range, mut pred: impl FnMut(&'s Self::Elem) -> bool) -> Result<u64, Self::Error> {
        // Keep counting elements while 1) the current index is within range of the `source`, and
        // 2) it also does not exceed any length indicated by `range`.
        let mut count: u64 = 0;
        while range.pos + count < self.len() as u64
            && match range.len {
                Length::Fixed(len) => count <= len,
                Length::Indefinite => true,
            }
        {
            if !pred(&self[(range.pos + count) as usize]) {
                break;
            }
            count += 1;
        }
        Ok(count)
    }

    #[inline]
    fn len(&self) -> u64 { <[T]>::len(self) as u64 }
}
impl<T> Source for Vec<T> {
    type Elem = T;
    type Error = Infallible;

    /// NOTE: This implementation will just assume the pointer to the original object as the ID.
    #[inline]
    fn id(&self) -> u64 { <&Vec<T> as Source>::id(&self) }

    #[inline]
    fn count_in_slice_while<'s>(&'s self, range: Range, pred: impl FnMut(&'s Self::Elem) -> bool) -> Result<u64, Self::Error> {
        <[T]>::count_in_slice_while(self.as_slice(), range, pred)
    }

    #[inline]
    fn len(&self) -> u64 { <[T]>::len(self) as u64 }
}
impl Source for str {
    type Elem = u8;
    type Error = Infallible;

    /// NOTE: This implementation will just assume the pointer to the original object as the ID.
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }

    #[inline]
    fn count_in_slice_while<'s>(&'s self, range: Range, pred: impl FnMut(&'s Self::Elem) -> bool) -> Result<u64, Self::Error> {
        <[u8]>::count_in_slice_while(self.as_bytes(), range, pred)
    }

    #[inline]
    fn len(&self) -> u64 { <[u8]>::len(self.as_bytes()) as u64 }
}
impl Source for String {
    type Elem = u8;
    type Error = Infallible;

    /// NOTE: This implementation will just assume the pointer to the original object as the ID.
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }

    #[inline]
    fn count_in_slice_while<'s>(&'s self, range: Range, pred: impl FnMut(&'s Self::Elem) -> bool) -> Result<u64, Self::Error> {
        <[u8]>::count_in_slice_while(self.as_bytes(), range, pred)
    }

    #[inline]
    fn len(&self) -> u64 { <[u8]>::len(self.as_bytes()) as u64 }
}

// Source impls
impl<I: Identifier, S: Source> Source for (I, S) {
    type Elem = S::Elem;
    type Error = S::Error;

    #[inline]
    fn id(&self) -> u64 { <I as Identifier>::id(&self.0) }

    #[inline]
    fn count_in_slice_while<'s>(&'s self, range: Range, pred: impl FnMut(&'s Self::Elem) -> bool) -> Result<u64, Self::Error> {
        <S as Source>::count_in_slice_while(&self.1, range, pred)
    }

    #[inline]
    fn len(&self) -> u64 { <S as Source>::len(&self.1) as u64 }
}

// Pointer-like impls
source_ptr_impl!('a, &'a T);
source_ptr_impl!('a, &'a mut T);
source_ptr_impl!('a, Cow<'a, T>);
source_ptr_impl!(Box<T>);
source_ptr_impl!(Rc<T>);
source_ptr_impl!(Arc<T>);
source_ptr_impl!('a, Ref<'a, T>);
source_ptr_impl!('a, RefMut<'a, T>);
source_ptr_impl!('a, MutexGuard<'a, T>);
source_ptr_impl!('a, RwLockReadGuard<'a, T>);
source_ptr_impl!('a, RwLockWriteGuard<'a, T>);





/***** LIBRARY *****/
/// Provides an abstraction over a part of something we're parsing.
///
/// In particular, this encodes a "from here on onwards"-kind of slice. If you start parsing, then
/// the source element at [`Slice::pos()`] is the first one analyzed.
///
/// # Generics
/// - `S`: The type of [`Source`] text that we use as input.
#[derive(Debug)]
pub struct Span<'s, S: ?Sized> {
    /// The thing we're parsing.
    source: &'s S,
    /// The current position we're parsing.
    range:  Range,
}

// Constructors
impl<'s, S: ?Sized + Source> Span<'s, S> {
    /// Constructor for the Slice that initializes it with a new `S`ource to read from.
    ///
    /// This will put the input to the beginning of the stream.
    ///
    /// # Arguments
    /// - `source`: Some `S`ource to use as input.
    ///
    /// # Returns
    /// A new Slice that can be used to parse `source`.
    #[inline]
    pub fn new(source: &'s S) -> Self { Self { source, range: Range::full() } }
}

// Ops
impl<'s, S: ?Sized> Clone for Span<'s, S> {
    #[inline]
    fn clone(&self) -> Self { *self }
}
impl<'s, S: ?Sized> Copy for Span<'s, S> {}
impl<'s, S: ?Sized + Source> Eq for Span<'s, S> {}
impl<'s, S: ?Sized + Source> PartialEq for Span<'s, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.source.id() == other.source.id() && self.range == other.range }
}

// Parsing
impl<'s, S: ?Sized + Source> Span<'s, S> {
    /// Returns the identifier of the underlying source text.
    ///
    /// # Returns
    /// A unique [`u64`] for the underlying `S`ource text.
    #[inline]
    pub fn id(&self) -> u64 { self.source.id() }

    /// Returns the location that this Slice spans.
    ///
    /// # Returns
    /// A [`Loc`] encoding the area in the `S`ource being spanned.
    ///
    /// It is already loaded with the [`Source::id()`] of `S`.
    #[inline]
    pub fn loc(&self) -> Loc { Loc { source: Some(self.source.id()), range: self.range } }

    /// Returns whether  this `Span` is empty or not.
    ///
    /// Note that this takes the actual length of the underlying `S`ource into account.
    ///
    /// # Returns
    /// True if it is, false if it isn't.
    #[inline]
    pub fn is_empty(&self) -> bool {
        let real_len: u64 = self.source.len();
        match self.range.len {
            Length::Fixed(0) => true,
            Length::Fixed(_) | Length::Indefinite => self.range.pos >= real_len,
        }
    }



    /// Slices this slice further while the head matches some predicate.
    ///
    /// This is essentially the bread-and-butter of the `snack` parser. The rest is just
    /// convenience wrapper around this function to do it in a certain way.
    ///
    /// # Arguments
    /// - `pred`: Some [`FnMut`] that will be called for every element at the end of the slice
    ///   until it returns false.
    ///
    /// # Returns
    /// A tuple of two spans:
    /// - The first is the span of anything **after** the head matched by `pred`. I.e., it starts
    ///   at the first element in the current span for which `pred` returned false.
    /// - The second is the head matched by `pred` itself. I.e., it starts at the current span and
    ///   then extends to include every element for which `pred` returned true.
    ///
    /// (Note their reversed order! Done for consistency with
    /// [`Combinator`](crate::spec::Combinator)s).
    ///
    /// # Errors
    /// This function can error if a call to [`Source::get()`] failed.
    #[inline]
    pub fn slice_while(&self, pred: impl FnMut(&'s S::Elem) -> bool) -> Result<(Self, Self), S::Error> {
        let count: u64 = self.source.count_in_slice_while(self.range, pred)?;
        let head: Self = Self { source: self.source, range: Range { pos: self.range.pos, len: Length::Fixed(count) } };
        let rem: Self = Self {
            source: self.source,
            range:  Range {
                pos: self.range.pos + count,
                len: match self.range.len {
                    // SAFETY: This subtraction SHOULD be OK, because `count_in_slice_while()`
                    // should already bound itself to this length.
                    Length::Fixed(len) => Length::Fixed(len - count),
                    Length::Indefinite => Length::Indefinite,
                },
            },
        };
        // Note the reversal here!
        Ok((rem, head))
    }
}
impl<'s, S: ?Sized> Span<'s, S> {
    /// Returns the inner range into the source text.
    ///
    /// # Returns
    /// A [`Range`] encoding what area is spanned.
    #[inline]
    pub const fn range(&self) -> Range { self.range }



    /// Slices this Span manually.
    ///
    /// # Arguments
    /// - `range`: A [`Range`] that will slice ourselves. For details, see [`Range::slice()`].
    ///
    /// # Returns
    /// A new Span that is this but slices according to the given `range`.
    #[inline]
    pub fn slice(&self, range: impl Into<Range>) -> Self { Self { source: self.source, range: self.range.slice_range(range) } }
}
