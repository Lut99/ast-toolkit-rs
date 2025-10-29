//  INPUT.rs
//    by Lut99
//
//  Description:
//!   Provides an abstraction over ([`Located`]) inputs such that they may be parsed.
//

use std::cell::RefMut;
use std::convert::Infallible;
use std::error::Error;
use std::sync::{MutexGuard, RwLockWriteGuard};


/***** HELPER MACROS *****/
/// Does implementations of pointer-like types for [`Source`].
macro_rules! source_ptr_impl {
    ('a, $ty:ty) => {
        impl<'a, T: Source> Source for $ty {
            type Elem = <T as Source>::Elem;
            type Error = <T as Source>::Error;

            #[inline]
            #[track_caller]
            fn id(&self) -> u64 { <T as Source>::id(self) }

            #[inline]
            #[track_caller]
            fn get(&mut self, index: usize) -> Result<Option<&Self::Elem>, Self::Error> { <T as Source>::get(self, index) }

            #[inline]
            #[track_caller]
            fn forget_up_to(&mut self, index: usize) { <T as Source>::forget_up_to(self, index) }
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
            fn get(&mut self, index: usize) -> Result<Option<&Self::Elem>, Self::Error> { <T as Source>::get(self, index) }

            #[inline]
            #[track_caller]
            fn forget_up_to(&mut self, index: usize) { <T as Source>::forget_up_to(self, index) }
        }
    };
}





/***** INTERFACES *****/
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
    /// # Returns
    /// The [`Source::Elem`] at the given position, or [`None`] if it is out-of-bounds.
    ///
    /// # Errors
    /// This function can error if something went horribly, horribly wrong while attempting to get
    /// the `index`ed element.
    fn get(&mut self, index: usize) -> Result<Option<&Self::Elem>, Self::Error>;

    /// Tells the implementation that the parser will not access anything before the given index
    /// anymore.
    ///
    /// This is useful when the implementation abstracts away dynamic loading of files or network
    /// streams. It can forget memory representing anything before the index if this is called.
    ///
    /// # Arguments
    /// - `up_to`: The first index of the element that **may still be accessed**. I.e., anything
    ///   _before_ this can be discarded.
    fn forget_up_to(&mut self, index: usize);
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
    fn get(&mut self, index: usize) -> Result<Option<&Self::Elem>, Self::Error> { Ok(<[T]>::get(self, index)) }

    #[inline]
    fn forget_up_to(&mut self, _index: usize) {
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
    fn get(&mut self, index: usize) -> Result<Option<&Self::Elem>, Self::Error> { Ok(<[T]>::get(self, index)) }

    #[inline]
    fn forget_up_to(&mut self, _index: usize) {
        /* Nothing to do, we have the entire slice in memory already */
    }
}
impl<'a, T> Source for &'a Vec<T> {
    type Elem = T;
    type Error = Infallible;

    /// NOTE: This implementation will just assume the pointer to the original object as the ID.
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }

    #[inline]
    fn get(&mut self, index: usize) -> Result<Option<&Self::Elem>, Self::Error> { Ok(<[T]>::get(self, index)) }

    #[inline]
    fn forget_up_to(&mut self, _index: usize) {
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
    fn get(&mut self, index: usize) -> Result<Option<&Self::Elem>, Self::Error> { Ok(<[T]>::get(self, index)) }

    #[inline]
    fn forget_up_to(&mut self, _index: usize) {
        /* Nothing to do, we have the entire slice in memory already */
    }
}
impl<'a> Source for &'a str {
    type Elem = u8;
    type Error = Infallible;

    /// NOTE: This implementation will just assume the pointer to the original object as the ID.
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }

    #[inline]
    fn get(&mut self, index: usize) -> Result<Option<&Self::Elem>, Self::Error> { Ok(self.as_bytes().get(index)) }

    #[inline]
    fn forget_up_to(&mut self, _index: usize) {
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
    fn get(&mut self, index: usize) -> Result<Option<&Self::Elem>, Self::Error> { Ok(self.as_bytes().get(index)) }

    #[inline]
    fn forget_up_to(&mut self, _index: usize) {
        /* Nothing to do, we have the entire slice in memory already */
    }
}
impl<'a> Source for &'a String {
    type Elem = u8;
    type Error = Infallible;

    /// NOTE: This implementation will just assume the pointer to the original object as the ID.
    #[inline]
    fn id(&self) -> u64 { self.as_ptr() as u64 }

    #[inline]
    fn get(&mut self, index: usize) -> Result<Option<&Self::Elem>, Self::Error> { Ok(self.as_bytes().get(index)) }

    #[inline]
    fn forget_up_to(&mut self, _index: usize) {
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
    fn get(&mut self, index: usize) -> Result<Option<&Self::Elem>, Self::Error> { Ok(self.as_bytes().get(index)) }

    #[inline]
    fn forget_up_to(&mut self, _index: usize) {
        /* Nothing to do, we have the entire slice in memory already */
    }
}

// Pointer-like impls
// impl<'a, 'b, T> Source for &'a &'b T
// where
//     // This one's weird! We are observing that the user has successfully implemented `Source`
//     // under read-only conditions. Hence, let's try to propagate this to any arbitrary string of
//     // read-only references.
//     &'a T: Source,
// {
//     type Elem = <&'a T as Source>::Elem;
//     type Error = <&'a T as Source>::Error;

//     #[inline]
//     #[track_caller]
//     fn id(&self) -> u64 { <&'a T as Source>::id(self) }

//     #[inline]
//     #[track_caller]
//     fn get(&mut self, index: usize) -> Result<Option<&Self::Elem>, Self::Error> { <&'a T as Source>::get(self, index) }

//     #[inline]
//     #[track_caller]
//     fn forget_up_to(&mut self, index: usize) { <&'a T as Source>::forget_up_to(self, index) }
// }
source_ptr_impl!('a, &'a mut T);
source_ptr_impl!(Box<T>);
source_ptr_impl!('a, RefMut<'a, T>);
source_ptr_impl!('a, MutexGuard<'a, T>);
source_ptr_impl!('a, RwLockWriteGuard<'a, T>);





/***** LIBRARY *****/
/// Provides an abstraction over something that we're parsing.
///
/// The snack-library adopts a stream-like view of inputs, from which we pop slices of information
/// as we parse them.
///
/// This struct will buffer inputs to make sure we only need to go through the input once.
///
/// # Generics
/// - `S`: The type of [`Parsable`] source text that we use as input.
#[derive(Clone, Copy, Debug)]
pub struct Input<S> {
    /// The thing we're parsing.
    source: S,
    /// The current position we're parsing.
    pos:    u64,
}

// Constructors
impl<S> Input<S> {
    /// Constructor for the Input that initializes it with a new `S`ource to read from.
    ///
    /// This will put the input to the beginning of the stream.
    ///
    /// # Arguments
    /// - `source`: Some `S`ource to use as input.
    ///
    /// # Returns
    /// A new Input that can be used to parse `source`.
    #[inline]
    pub const fn new(source: S) -> Self { Self { source, pos: 0 } }
}

// Parsing
impl<S: Source> Input<S> {}
