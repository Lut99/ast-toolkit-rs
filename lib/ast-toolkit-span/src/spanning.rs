//  SPANNING.rs
//    by Lut99
//
//  Description:
//!   Implements the [`Spanning`] trait and friends, which abstracts over
//!   things that are somehow related to source text through a [`Span`].
//

use std::borrow::Cow;
use std::cell::{Ref, RefMut};
use std::convert::Infallible;
use std::hint::unreachable_unchecked;
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};

use crate::Span;


/***** HELPER MACROS *****/
/// Implements [`Spanning`] for pointer-like types with a reference.
///
/// Owned types can use pointer-specific optimizations to take ownership of `self` to call
/// [`Spanning::take_span()`] instead of [`Spanning::get_span()`].
macro_rules! spanning_ptr_impl {
    ($ty:ty) => {
        impl<'a, T: Spanning<S>, S: Clone> Spanning<S> for $ty {
            #[inline]
            #[track_caller]
            fn get_span(&self) -> Option<Cow<Span<S>>> { <T as Spanning<S>>::get_span(self) }

            // # A note on performance
            // This function cannot take ownership of `T` to call [`Spanning::take_span()`]; as
            // such, it calls [`Spanning::get_span()`] instead and then clones the resulting
            // [`Span`] using [`Cow::into_owned()`]. We deem it to be cheap to clone the span than
            // the original type `T`.
            #[inline]
            #[track_caller]
            fn take_span(self) -> Option<Span<S>> { <T as Spanning<S>>::get_span(&*self).map(Cow::into_owned) }
        }
    };
}

/// Implements [`SpanningInf`] for pointer-like types with a reference.
///
/// Owned types can use pointer-specific optimizations to take ownership of `self` to call
/// [`Spanning::take_span()`] instead of [`Spanning::get_span()`].
macro_rules! spanning_inv_ptr_impl {
    ($ty:ty) => {
        impl<'a, T: SpanningInf<S>, S: Clone> SpanningInf<S> for $ty {
            #[inline]
            #[track_caller]
            fn span(&self) -> Cow<Span<S>> { <T as SpanningInf<S>>::span(self) }

            // # A note on performance
            // This function cannot take ownership of `T` to call [`SpanningInf::into_span()`]; as
            // such, it calls [`SpanningInf::span()`] instead and then clones the resulting
            // [`Span`] using [`Cow::into_owned()`]. We deem it to be cheap to clone the span than
            // the original type `T`.
            #[inline]
            #[track_caller]
            fn into_span(self) -> Span<S> { <T as SpanningInf<S>>::span(&*self).into_owned() }
        }
    };
}

/// Implements [`SpanningRef`] for pointer-like types with a reference.
macro_rules! spanning_ref_ptr_impl {
    ($({$lt:lifetime $(+ $trt:ident)?},)? $ty:ty) => {
        impl<$($lt,)? T: $($($trt +)?)? SpanningRef<S>, S: Clone> SpanningRef<S> for $ty {
            #[inline]
            #[track_caller]
            fn span_ref(&self) -> &Span<S> { <T as SpanningRef<S>>::span_ref(self) }
        }
    };
}

/// Implements [`SpanningMut`] for pointer-like types with a reference.
macro_rules! spanning_mut_ptr_impl {
    ($({$lt:lifetime $(+ $trt:ident)?},)? $ty:ty) => {
        impl<$($lt,)? T: $($($trt +)?)? SpanningMut<S>, S: Clone> SpanningMut<S> for $ty {
            #[inline]
            #[track_caller]
            fn span_mut(&mut self) -> &mut Span<S> { <T as SpanningMut<S>>::span_mut(self) }
        }
    };
}





/***** LIBRARY *****/
/// Abstracts over nodes in an AST which are _usually_ [`Span`]ning a piece of `S`ource text.
///
/// This trait is the most permissive version of anything spanning. Specifically, it does _not_
/// guarantee that:
/// 1. There is always a [`Span`] present; and
/// 2. The [`Span`] already exists, i.e., can be shared by reference.
///
/// If you're working in a context where you can or want to statically guarantee any of the above,
/// refer to the [`SpanningInf`] and [`SpanningRef`] traits, respectively.
///
/// # Generics
/// - `S`: The type of source text we are spanning over.
pub trait Spanning<S: Clone> {
    /// Returns a span that represents this whole node in the source text.
    ///
    /// # Returns
    /// An [`Option`]al, [potentially borrowed](Cow) [`Span`] that represents the area of the
    /// `S`ource text where this node is linked to.
    ///
    /// If you don't feel like runtime checking the `Option` or deal with the `Cow`, then check
    /// [`SpanningInf::span()`] or [`SpanningRef::span()`], respectively.
    fn get_span(&self) -> Option<Cow<Span<S>>>;

    /// Returns a span that represents this whole node in the source text, consuming this node in
    /// order to return it directly.
    ///
    /// Compared to [`Spanning::get_span()`], this function can take advantage of ownership over
    /// any internal [`Span`] to return it more efficiently.
    ///
    /// # Returns
    /// An [`Option`]al [`Span`] that represents the area of the `S`ource text where this node is
    /// linked to.
    ///
    /// If you don't feel like runtime checking the `Option` then check
    /// [`SpanningInf::into_span()`] or [`SpanningRef::into_span()`].
    fn take_span(self) -> Option<Span<S>>;
}

// Default impls
impl<S: Clone> Spanning<S> for Infallible {
    /// Note: This is a dummy implementation. Calling it is undefined behaviour (and impossible).
    #[inline]
    fn get_span(&self) -> Option<Cow<Span<S>>> {
        // SAFETY: It is impossible to construct `Infallible`, so this function cannot be called
        // (it needs an instance `self` as argument).
        unsafe { unreachable_unchecked() }
    }

    /// Note: This is a dummy implementation. Calling it is undefined behaviour (and impossible).
    #[inline]
    fn take_span(self) -> Option<Span<S>> {
        // SAFETY: It is impossible to construct `Infallible`, so this function cannot be called
        // (it needs an instance `self` as argument).
        unsafe { unreachable_unchecked() }
    }
}
impl<T: Spanning<S>, S: Clone> Spanning<S> for Option<T> {
    #[inline]
    fn get_span(&self) -> Option<Cow<Span<S>>> { self.as_ref().and_then(Spanning::get_span) }

    #[inline]
    fn take_span(self) -> Option<Span<S>> { self.and_then(Spanning::take_span) }
}

// Pointer-like impls
spanning_ptr_impl!(&'a T);
spanning_ptr_impl!(&'a mut T);
spanning_ptr_impl!(Ref<'a, T>);
spanning_ptr_impl!(RefMut<'a, T>);
spanning_ptr_impl!(MutexGuard<'a, T>);
spanning_ptr_impl!(RwLockReadGuard<'a, T>);
spanning_ptr_impl!(RwLockWriteGuard<'a, T>);
#[cfg(feature = "parking_lot")]
spanning_ptr_impl!(parking_lot::MutexGuard<'a, T>);
#[cfg(feature = "parking_lot")]
spanning_ptr_impl!(parking_lot::RwLockReadGuard<'a, T>);
#[cfg(feature = "parking_lot")]
spanning_ptr_impl!(parking_lot::RwLockWriteGuard<'a, T>);
impl<'a, T: Clone + Spanning<S>, S: Clone> Spanning<S> for Cow<'a, T> {
    #[inline]
    #[track_caller]
    fn get_span(&self) -> Option<Cow<Span<S>>> { <T as Spanning<S>>::get_span(self) }

    /// # A note on performance
    /// This function takes advantage of ownership to call [`Spanning::take_span()`] in case
    /// the [`Cow`] owns its data (i.e., it is a [`Cow::Owned`]). Else, we call
    /// [`Spanning::get_span()`] and then clone the result using [`Cow::into_owned()`].
    #[inline]
    #[track_caller]
    fn take_span(self) -> Option<Span<S>> {
        match self {
            Self::Owned(inner) => <T as Spanning<S>>::take_span(inner),
            Self::Borrowed(inner) => <T as Spanning<S>>::get_span(inner).map(Cow::into_owned),
        }
    }
}
impl<T: Spanning<S>, S: Clone> Spanning<S> for Box<T> {
    #[inline]
    #[track_caller]
    fn get_span(&self) -> Option<Cow<Span<S>>> { <T as Spanning<S>>::get_span(self) }

    #[inline]
    #[track_caller]
    fn take_span(self) -> Option<Span<S>> { <T as Spanning<S>>::take_span(*self) }
}
impl<T: Spanning<S>, S: Clone> Spanning<S> for Rc<T> {
    #[inline]
    #[track_caller]
    fn get_span(&self) -> Option<Cow<Span<S>>> { <T as Spanning<S>>::get_span(self) }

    /// # A note on performance
    /// This function will take advantage of ownership to call [`Spanning::take_span()`] in case
    /// the shared pointer has unique ownership (attempted through [`Rc::try_unwrap()`]).
    /// Otherwise, we call [`Spanning::get_span()`] and then clone the result using
    /// [`Cow::into_owned()`].
    #[inline]
    #[track_caller]
    fn take_span(self) -> Option<Span<S>> {
        match Rc::try_unwrap(self) {
            Ok(inner) => <T as Spanning<S>>::take_span(inner),
            Err(rc) => <T as Spanning<S>>::get_span(&rc).map(Cow::into_owned),
        }
    }
}
impl<T: Spanning<S>, S: Clone> Spanning<S> for Arc<T> {
    #[inline]
    #[track_caller]
    fn get_span(&self) -> Option<Cow<Span<S>>> { <T as Spanning<S>>::get_span(self) }

    /// # A note on performance
    /// This function will take advantage of ownership to call [`Spanning::take_span()`] in case
    /// the shared pointer has unique ownership (attempted through [`Arc::try_unwrap()`]).
    /// Otherwise, we call [`Spanning::get_span()`] and then clone the result using
    /// [`Cow::into_owned()`].
    #[inline]
    #[track_caller]
    fn take_span(self) -> Option<Span<S>> {
        match Arc::try_unwrap(self) {
            Ok(inner) => <T as Spanning<S>>::take_span(inner),
            Err(rc) => <T as Spanning<S>>::get_span(&rc).map(Cow::into_owned),
        }
    }
}



/// Abstracts over nodes in an AST which are **always** [`Span`]ning a piece of `S`ource text.
///
/// This trait is more restrictive than [`Spanning`], in that it upholds (1) that there must always
/// be a link to the source text for this type. However, it is more permissive than
/// [`SpanningRef`], as it does _not_ guarantee (2) that the [`Span`] in question is stored in this
/// type itself.
///
/// # Generics
/// - `S`: The type of source text we are spanning over.
pub trait SpanningInf<S: Clone>: Spanning<S> {
    /// Returns a span that represents this whole node in the source text.
    ///
    /// # Returns
    /// A [potentially borrowed](Cow) [`Span`] that represents the area of the `S`ource text where
    /// this node is linked to.
    ///
    /// If you don't feel like dealing with the `Cow`, then check [`SpanningRef::span()`].
    fn span(&self) -> Cow<Span<S>>;

    /// Returns a span that represents this whole node in the source text, consuming this node in
    /// order to return it directly.
    ///
    /// Compared to [`Spanning::span()`], this function can take advantage of ownership over any
    /// internal [`Span`] to return it more efficiently.
    ///
    /// # Returns
    /// A [`Span`] that represents the area of the `S`ource text where this node is linked to.
    fn into_span(self) -> Span<S>;
}

// Default impls
impl<S: Clone> SpanningInf<S> for Infallible {
    /// Note: This is a dummy implementation. Calling it is undefined behaviour (and impossible).
    #[inline]
    fn span(&self) -> Cow<Span<S>> {
        // SAFETY: It is impossible to construct `Infallible`, so this function cannot be called
        // (it needs an instance `self` as argument).
        unsafe { unreachable_unchecked() }
    }

    /// Note: This is a dummy implementation. Calling it is undefined behaviour (and impossible).
    #[inline]
    fn into_span(self) -> Span<S> {
        // SAFETY: It is impossible to construct `Infallible`, so this function cannot be called
        // (it needs an instance `self` as argument).
        unsafe { unreachable_unchecked() }
    }
}

// Pointer-like impls
spanning_inv_ptr_impl!(&'a T);
spanning_inv_ptr_impl!(&'a mut T);
spanning_inv_ptr_impl!(Ref<'a, T>);
spanning_inv_ptr_impl!(RefMut<'a, T>);
spanning_inv_ptr_impl!(MutexGuard<'a, T>);
spanning_inv_ptr_impl!(RwLockReadGuard<'a, T>);
spanning_inv_ptr_impl!(RwLockWriteGuard<'a, T>);
#[cfg(feature = "parking_lot")]
spanning_inv_ptr_impl!(parking_lot::MutexGuard<'a, T>);
#[cfg(feature = "parking_lot")]
spanning_inv_ptr_impl!(parking_lot::RwLockReadGuard<'a, T>);
#[cfg(feature = "parking_lot")]
spanning_inv_ptr_impl!(parking_lot::RwLockWriteGuard<'a, T>);
impl<'a, T: Clone + SpanningInf<S>, S: Clone> SpanningInf<S> for Cow<'a, T> {
    #[inline]
    #[track_caller]
    fn span(&self) -> Cow<Span<S>> { <T as SpanningInf<S>>::span(self) }

    /// # A note on performance
    /// This function takes advantage of ownership to call [`SpanningInf::take_span()`] in case
    /// the [`Cow`] owns its data (i.e., it is a [`Cow::Owned`]). Else, we call
    /// [`SpanningInf::span()`] and then clone the result using [`Cow::into_owned()`].
    #[inline]
    #[track_caller]
    fn into_span(self) -> Span<S> {
        match self {
            Self::Owned(inner) => <T as SpanningInf<S>>::into_span(inner),
            Self::Borrowed(inner) => <T as SpanningInf<S>>::span(inner).into_owned(),
        }
    }
}
impl<T: SpanningInf<S>, S: Clone> SpanningInf<S> for Box<T> {
    #[inline]
    #[track_caller]
    fn span(&self) -> Cow<Span<S>> { <T as SpanningInf<S>>::span(self) }

    #[inline]
    #[track_caller]
    fn into_span(self) -> Span<S> { <T as SpanningInf<S>>::into_span(*self) }
}
impl<T: SpanningInf<S>, S: Clone> SpanningInf<S> for Rc<T> {
    #[inline]
    #[track_caller]
    fn span(&self) -> Cow<Span<S>> { <T as SpanningInf<S>>::span(self) }

    /// # A note on performance
    /// This function will take advantage of ownership to call [`SpanningInf::into_span()`] in case
    /// the shared pointer has unique ownership (attempted through [`Rc::try_unwrap()`]).
    /// Otherwise, we call [`SpanningInf::span()`] and then clone the result using
    /// [`Cow::into_owned()`].
    #[inline]
    #[track_caller]
    fn into_span(self) -> Span<S> {
        match Rc::try_unwrap(self) {
            Ok(inner) => <T as SpanningInf<S>>::into_span(inner),
            Err(rc) => <T as SpanningInf<S>>::span(&rc).into_owned(),
        }
    }
}
impl<T: SpanningInf<S>, S: Clone> SpanningInf<S> for Arc<T> {
    #[inline]
    #[track_caller]
    fn span(&self) -> Cow<Span<S>> { <T as SpanningInf<S>>::span(self) }

    /// # A note on performance
    /// This function will take advantage of ownership to call [`SpanningInf::into_span()`] in case
    /// the shared pointer has unique ownership (attempted through [`Arc::try_unwrap()`]).
    /// Otherwise, we call [`SpanningInf::span()`] and then clone the result using
    /// [`Cow::into_owned()`].
    #[inline]
    #[track_caller]
    fn into_span(self) -> Span<S> {
        match Arc::try_unwrap(self) {
            Ok(inner) => <T as SpanningInf<S>>::into_span(inner),
            Err(rc) => <T as SpanningInf<S>>::span(&rc).into_owned(),
        }
    }
}



/// Abstracts over nodes in an AST which are **always** [`Span`]ning a piece of `S`ource text.
///
/// This trait is more restrictive than both [`Spanning`] and [`SpanningInf`]: it upholds (1) that
/// there must always be a link to the source text for this type, _and_ it upholds (2) that the
/// [`Span`] in question is stored in this type itself (i.e., it can be returned by reference).
///
/// # Generics
/// - `S`: The type of source text we are spanning over.
pub trait SpanningRef<S: Clone>: SpanningInf<S> {
    /// Returns a span that represents this whole node in the source text.
    ///
    /// # Returns
    /// A reference to a [`Span`] that represents the area of the `S`ource text where this node is
    /// linked to.
    fn span_ref(&self) -> &Span<S>;
}

// Default impls
impl<S: Clone> SpanningRef<S> for Infallible {
    /// Note: This is a dummy implementation. Calling it is undefined behaviour (and impossible).
    #[inline]
    fn span_ref(&self) -> &Span<S> {
        // SAFETY: It is impossible to construct `Infallible`, so this function cannot be called
        // (it needs an instance `self` as argument).
        unsafe { unreachable_unchecked() }
    }
}

// Pointer-like impls
spanning_ref_ptr_impl!({'a}, &'a T);
spanning_ref_ptr_impl!({'a}, &'a mut T);
spanning_ref_ptr_impl!({'a + Clone}, Cow<'a, T>);
spanning_ref_ptr_impl!(Box<T>);
spanning_ref_ptr_impl!(Rc<T>);
spanning_ref_ptr_impl!(Arc<T>);
spanning_ref_ptr_impl!({'a}, Ref<'a, T>);
spanning_ref_ptr_impl!({'a}, RefMut<'a, T>);
spanning_ref_ptr_impl!({'a}, MutexGuard<'a, T>);
spanning_ref_ptr_impl!({'a}, RwLockReadGuard<'a, T>);
spanning_ref_ptr_impl!({'a}, RwLockWriteGuard<'a, T>);
#[cfg(feature = "parking_lot")]
spanning_ref_ptr_impl!({'a}, parking_lot::MutexGuard<'a, T>);
#[cfg(feature = "parking_lot")]
spanning_ref_ptr_impl!({'a}, parking_lot::RwLockReadGuard<'a, T>);
#[cfg(feature = "parking_lot")]
spanning_ref_ptr_impl!({'a}, parking_lot::RwLockWriteGuard<'a, T>);



/// Abstracts over nodes in an AST which are **always** [`Span`]ning a piece of `S`ource text,
/// _and_ can do so mutably.
///
/// This trait is more restrictive than both [`Spanning`], [`SpanningInf`] and [`SpanningRef`]: it
/// upholds (1) that there must always be a link to the source text for this type; _and_ it upholds
/// (2) that the [`Span`] in question is stored in this type itself (i.e., it can be returned by
/// reference); _and_ it upholds the secret bonus (3) that the type for which it is implemented can
/// be accessed mutably (unlike e.g. `&T`).
///
/// # Generics
/// - `S`: The type of source text we are spanning over.
pub trait SpanningMut<S: Clone>: SpanningRef<S> {
    /// Returns a (mutable reference to a) span that represents this whole node in the source text.
    ///
    /// # Returns
    /// A mutable reference to a [`Span`] that represents the area of the `S`ource text where
    /// this node is linked to.
    fn span_mut(&mut self) -> &mut Span<S>;
}

// Default impls
impl<S: Clone> SpanningMut<S> for Infallible {
    /// Note: This is a dummy implementation. Calling it is undefined behaviour (and impossible).
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> {
        // SAFETY: It is impossible to construct `Infallible`, so this function cannot be called
        // (it needs an instance `self` as argument).
        unsafe { unreachable_unchecked() }
    }
}

// Pointer-like impls
spanning_mut_ptr_impl!({'a}, &'a mut T);
spanning_mut_ptr_impl!(Box<T>);
spanning_mut_ptr_impl!({'a}, RefMut<'a, T>);
spanning_mut_ptr_impl!({'a}, MutexGuard<'a, T>);
spanning_mut_ptr_impl!({'a}, RwLockWriteGuard<'a, T>);
#[cfg(feature = "parking_lot")]
spanning_mut_ptr_impl!({'a}, parking_lot::MutexGuard<'a, T>);
#[cfg(feature = "parking_lot")]
spanning_mut_ptr_impl!({'a}, parking_lot::RwLockWriteGuard<'a, T>);
impl<'a, T: Clone + SpanningMut<S>, S: Clone> SpanningMut<S> for Cow<'a, T> {
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> { <T as SpanningMut<S>>::span_mut(self.to_mut()) }
}
