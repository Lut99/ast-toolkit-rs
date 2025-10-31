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

use ast_toolkit_loc::Loc;


/***** HELPER MACROS *****/
/// Does implementations of pointer-like types for [`Identifier`].
macro_rules! identifier_ptr_impl {
    ('a,Cow < 'a,T >) => {
        impl<'a, T: Identifier + ToOwned> Identifier for Cow<'a, T> {
            #[inline]
            #[track_caller]
            fn id(&self) -> u64 { <T as Identifier>::id(self) }
        }
    };
    ('a, $ty:ty) => {
        impl<'a, T: Identifier> Identifier for $ty {
            #[inline]
            #[track_caller]
            fn id(&self) -> u64 { <T as Identifier>::id(self) }
        }
    };
    ($ty:ty) => {
        impl<T: Identifier> Identifier for $ty {
            #[inline]
            #[track_caller]
            fn id(&self) -> u64 { <T as Identifier>::id(self) }
        }
    };
}

/// Does implementations of pointer-like types for [`Source`].
macro_rules! source_ptr_impl {
    ('a,Cow < 'a,T >) => {
        impl<'a, T: Source + ToOwned> Source for Cow<'a, T> {
            type Elem = <T as Source>::Elem;
            type Error = <T as Source>::Error;

            #[inline]
            #[track_caller]
            fn id(&self) -> u64 { <T as Source>::id(self) }

            #[inline]
            #[track_caller]
            fn get(&self, index: u64) -> Result<Option<&Self::Elem>, Self::Error> { <T as Source>::get(self, index) }

            #[inline]
            #[track_caller]
            fn forget_up_to(&self, index: u64) { <T as Source>::forget_up_to(self, index) }
        }
    };
    ('a, $ty:ty) => {
        impl<'a, T: Source> Source for $ty {
            type Elem = <T as Source>::Elem;
            type Error = <T as Source>::Error;

            #[inline]
            #[track_caller]
            fn id(&self) -> u64 { <T as Source>::id(self) }

            #[inline]
            #[track_caller]
            fn get(&self, index: u64) -> Result<Option<&Self::Elem>, Self::Error> { <T as Source>::get(self, index) }

            #[inline]
            #[track_caller]
            fn forget_up_to(&self, index: u64) { <T as Source>::forget_up_to(self, index) }
        }
    };
    ($ty:ty) => {
        impl<T: Source> Source for $ty {
            type Elem = <T as Source>::Elem;
            type Error = <T as Source>::Error;

            #[inline]
            #[track_caller]
            fn id(&self) -> u64 { <T as Source>::id(self) }

            #[inline]
            #[track_caller]
            fn get(&self, index: u64) -> Result<Option<&Self::Elem>, Self::Error> { <T as Source>::get(self, index) }

            #[inline]
            #[track_caller]
            fn forget_up_to(&self, index: u64) { <T as Source>::forget_up_to(self, index) }
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

    /// Gets the next element in the list.
    ///
    /// Note that if you're implementing something complex that involves mutating internal buffers,
    /// then unfortunately, you will have to do this using interior mutability. It's important that
    /// the code can be cheaply sprinkled with references to your source text.
    ///
    /// However, this will _not_ be asynchronous in any way. So you can use cheap  mutability like
    /// [`RefCell`](std::cell::RefCell).
    ///
    /// # Returns
    /// The [`Source::Elem`] at the given position, or [`None`] if it is out-of-bounds.
    ///
    /// # Errors
    /// This function can error if something went horribly, horribly wrong while attempting to get
    /// the `index`ed element.
    fn get(&self, index: u64) -> Result<Option<&Self::Elem>, Self::Error>;

    /// Tells the implementation that the parser will not access anything before the given index
    /// anymore.
    ///
    /// This is useful when the implementation abstracts away dynamic loading of files or network
    /// streams. It can forget memory representing anything before the index if this is called.
    ///
    /// Note that you will probably need interior mutability in your type to do something useful
    /// here.
    ///
    /// # Arguments
    /// - `up_to`: The first index of the element that **may still be accessed**. I.e., anything
    ///   _before_ this can be discarded.
    fn forget_up_to(&self, index: u64);
}

// Std impls
// NOTE: Yes quite a bit repition. Don't mess too much with it, though, because we need to
// communicate that we can use the read-only references to slices etc as mutable in _this_ case
// (since we're not actually using the mutability).
impl<'a, T> Source for &'a [T] {
    type Elem = T;
    type Error = Infallible;

    /// NOTE: This implementation will just assume the pointer to the original object as the ID.
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }

    #[inline]
    fn get(&self, index: u64) -> Result<Option<&Self::Elem>, Self::Error> { Ok(<[T]>::get(self, index as usize)) }

    #[inline]
    fn forget_up_to(&self, _index: u64) {
        /* Nothing to do, we have the entire slice in memory already */
    }
}
impl<T> Source for [T] {
    type Elem = T;
    type Error = Infallible;

    /// NOTE: This implementation will just assume the pointer to the original object as the ID.
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }

    #[inline]
    fn get(&self, index: u64) -> Result<Option<&Self::Elem>, Self::Error> { Ok(<[T]>::get(self, index as usize)) }

    #[inline]
    fn forget_up_to(&self, _index: u64) {
        /* Nothing to do, we have the entire slice in memory already */
    }
}
impl<T> Source for Vec<T> {
    type Elem = T;
    type Error = Infallible;

    /// NOTE: This implementation will just assume the pointer to the original object as the ID.
    #[inline]
    fn id(&self) -> u64 { <&Vec<T> as Source>::id(&self) }

    #[inline]
    fn get(&self, index: u64) -> Result<Option<&Self::Elem>, Self::Error> { Ok(<[T]>::get(self, index as usize)) }

    #[inline]
    fn forget_up_to(&self, _index: u64) {
        /* Nothing to do, we have the entire slice in memory already */
    }
}
impl Source for str {
    type Elem = u8;
    type Error = Infallible;

    /// NOTE: This implementation will just assume the pointer to the original object as the ID.
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }

    #[inline]
    fn get(&self, index: u64) -> Result<Option<&Self::Elem>, Self::Error> { Ok(self.as_bytes().get(index as usize)) }

    #[inline]
    fn forget_up_to(&self, _index: u64) {
        /* Nothing to do, we have the entire slice in memory already */
    }
}
impl Source for String {
    type Elem = u8;
    type Error = Infallible;

    /// NOTE: This implementation will just assume the pointer to the original object as the ID.
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }

    #[inline]
    fn get(&self, index: u64) -> Result<Option<&Self::Elem>, Self::Error> { Ok(self.as_bytes().get(index as usize)) }

    #[inline]
    fn forget_up_to(&self, _index: u64) {
        /* Nothing to do, we have the entire slice in memory already */
    }
}

// Source impls
impl<I: Identifier, S: Source> Source for (I, S) {
    type Elem = S::Elem;
    type Error = S::Error;

    #[inline]
    fn id(&self) -> u64 { <I as Identifier>::id(&self.0) }

    #[inline]
    fn get(&self, index: u64) -> Result<Option<&Self::Elem>, Self::Error> { <S as Source>::get(&self.1, index) }

    #[inline]
    fn forget_up_to(&self, index: u64) { <S as Source>::forget_up_to(&self.1, index) }
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
#[derive(Clone, Copy, Debug)]
pub struct Slice<'s, S> {
    /// The thing we're parsing.
    source: &'s S,
    /// The current position we're parsing.
    loc:    Loc,
}

// Constructors
impl<'s, S: Source> Slice<'s, S> {
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
    pub fn new(source: &'s S) -> Self { Self { source, loc: Loc::encapsulate(source.id()) } }
}

// Parsing
impl<'s, S: Source> Slice<'s, S> {
    /// Returns the identifier of the underlying source text.
    ///
    /// # Returns
    /// A unique [`u64`] for the underlying `S`ource text.
    #[inline]
    pub fn id(&self) -> u64 { self.source.id() }



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
    /// A slice that is this one but advanced for every element where `pred` returned true, or one
    /// pointing beyond the end of the array (if all of them did).
    ///
    /// # Errors
    /// This function can error if a call to [`Source::get()`] failed.
    #[inline]
    pub fn slice_while(&self, mut pred: impl FnMut(&S::Elem) -> bool) -> Result<Self, S::Error> {
        let mut loc: Loc = self.loc;
        while let Some(elem) = self.source.get(loc.range.pos)? {
            if !pred(elem) {
                return Ok(Self { source: self.source, loc });
            }
            loc.range.pos += 1;
        }
        // Got `None`, i.e., this is out-of-bounds
        Ok(Self { source: self.source, loc })
    }
}
impl<'s, S> Slice<'s, S> {
    /// Returns the location that this Slice spans.
    ///
    /// # Returns
    /// A [`Loc`] encoding the area in the `S`ource being spanned.
    ///
    /// It is already loaded with the [`Source::id()`] of `S`.
    #[inline]
    pub const fn loc(&self) -> Loc { self.loc }
}
