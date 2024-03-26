//  TRAILING.rs
//    by Lut99
//
//  Created:
//    26 Feb 2024, 14:08:18
//  Last edited:
//    26 Mar 2024, 16:44:01
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the "trailing" punctuated list, which has mandatory punctuation
//!   and optional trailing whitespace.
//

use std::fmt::{Debug, DebugList, Formatter, Result as FResult};
use std::hash::{Hash, Hasher};
use std::ops::{Index, IndexMut};

use enum_debug::EnumDebug;

use crate::common::{PunctIndex, ValueIndex};


/***** MACROS *****/
/// Builds a [`PunctuatedTrailing`] conveniently from a list.
///
/// Your items must match the order required by the punctuated. I.e., start with a value, then alternate commas and values, optionally ending in a comma:
/// ```rust
/// use ast_toolkit_punctuated::{punct_trail, PunctIndex, PunctuatedTrailing};
///
/// #[derive(Debug, PartialEq)]
/// struct Value;
/// #[derive(Debug, PartialEq)]
/// struct Punct;
///
/// let punct: PunctuatedTrailing<Value, Punct> = punct_trail![];
/// assert!(punct.is_empty());
///
/// let punct: PunctuatedTrailing<Value, Punct> = punct_trail![v => Value];
/// assert_eq!(punct.len(), 1);
/// assert_eq!(punct[0], Value);
///
/// let punct: PunctuatedTrailing<Value, Punct> = punct_trail![v => Value, p => Punct];
/// assert_eq!(punct.len(), 1);
/// assert_eq!(punct[0], Value);
/// assert_eq!(punct[PunctIndex(0)], Punct);
///
/// let punct: PunctuatedTrailing<Value, Punct> = punct_trail![v => Value, p => Punct, v => Value];
/// assert_eq!(punct.len(), 2);
/// assert_eq!(punct[0], Value);
/// assert_eq!(punct[PunctIndex(0)], Punct);
/// assert_eq!(punct[1], Value);
/// ```
#[macro_export]
macro_rules! punct_trail {
    // Nothing to be done if nothing is given
    (__recursion $list:ident) => {};
    // Pop values
    (__recursion $list:ident v => $value:expr $(, $($items:tt)+)?) => {
        $list.push_value($value);
        ::ast_toolkit_punctuated::punct_trail!(__recursion $list $($($items)+)?);
    };
    // Pop punctuation
    (__recursion $list:ident p => $punct:expr $(, $($items:tt)+)?) => {
        $list.push_punct($punct);
        ::ast_toolkit_punctuated::punct_trail!(__recursion $list $($($items)+)?);
    };

    [$($items:tt)*] => {{
        // Call the macro
        let mut punct = ::ast_toolkit_punctuated::trailing::PunctuatedTrailing::new();
        ::ast_toolkit_punctuated::punct_trail!(__recursion punct $($items)*);
        punct
    }};
}





/***** ITERATORS *****/
/// Iterates over the values in a [`Punctuated`] by reference.
#[derive(Debug)]
pub struct Values<'p, V, P> {
    /// A value buffer from the last time around.
    prev: Option<&'p V>,
    /// An iterator over the rest of the elements.
    data: std::slice::Iter<'p, (P, V)>,
}
impl<'p, V, P> Iterator for Values<'p, V, P> {
    type Item = &'p V;

    #[inline]
    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next element in the list
        if let Some((_, v)) = self.data.next() {
            let ret = self.prev.take().unwrap_or_else(|| panic!("Empty prev mid-iteration"));
            self.prev = Some(v);
            Some(ret)
        } else if let Some(prev) = self.prev.take() {
            // One more pair to go!
            Some(prev)
        } else {
            // Done
            None
        }
    }
}

/// Iterates over the values in a [`Punctuated`] by mutable reference.
#[derive(Debug)]
pub struct ValuesMut<'p, V, P> {
    /// A value buffer from the last time around.
    prev: Option<&'p mut V>,
    /// An iterator over the rest of the elements.
    data: std::slice::IterMut<'p, (P, V)>,
}
impl<'p, V, P> Iterator for ValuesMut<'p, V, P> {
    type Item = &'p mut V;

    #[inline]
    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next element in the list
        if let Some((_, v)) = self.data.next() {
            let ret = self.prev.take().unwrap_or_else(|| panic!("Empty prev mid-iteration"));
            self.prev = Some(v);
            Some(ret)
        } else if let Some(prev) = self.prev.take() {
            // One more pair to go!
            Some(prev)
        } else {
            // Done
            None
        }
    }
}

/// Iterates over the values in a [`Punctuated`] by ownership.
#[derive(Debug)]
pub struct IntoValues<V, P> {
    /// A value buffer from the last time around.
    prev: Option<V>,
    /// An iterator over the rest of the elements.
    data: std::vec::IntoIter<(P, V)>,
}
impl<V, P> Iterator for IntoValues<V, P> {
    type Item = V;

    #[inline]
    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next element in the list
        if let Some((_, v)) = self.data.next() {
            let ret = self.prev.take().unwrap_or_else(|| panic!("Empty prev mid-iteration"));
            self.prev = Some(v);
            Some(ret)
        } else if let Some(prev) = self.prev.take() {
            // One more pair to go!
            Some(prev)
        } else {
            // Done
            None
        }
    }
}



/// Iterates over the punctuation in a [`Punctuated`] by reference.
#[derive(Debug)]
pub struct Puncts<'p, V, P> {
    /// An iterator over the rest of the elements.
    data: std::slice::Iter<'p, (P, V)>,
    /// The final punctuation to maybe include.
    next: Option<&'p P>,
}
impl<'p, V, P> Iterator for Puncts<'p, V, P> {
    type Item = &'p P;

    #[inline]
    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next element in the list
        if let Some((p, _)) = self.data.next() {
            Some(p)
        } else if let Some(next) = self.next.take() {
            // One more pair to go!
            Some(next)
        } else {
            // Done
            None
        }
    }
}

/// Iterates over the punctuation in a [`Punctuated`] by mutable reference.
#[derive(Debug)]
pub struct PunctsMut<'p, V, P> {
    /// An iterator over the rest of the elements.
    data: std::slice::IterMut<'p, (P, V)>,
    /// The final punctuation to maybe include.
    next: Option<&'p mut P>,
}
impl<'p, V, P> Iterator for PunctsMut<'p, V, P> {
    type Item = &'p mut P;

    #[inline]
    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next element in the list
        if let Some((p, _)) = self.data.next() {
            Some(p)
        } else if let Some(next) = self.next.take() {
            // One more pair to go!
            Some(next)
        } else {
            // Done
            None
        }
    }
}

/// Iterates over the punctuation in a [`Punctuated`] by ownership.
#[derive(Debug)]
pub struct IntoPuncts<V, P> {
    /// An iterator over the rest of the elements.
    data: std::vec::IntoIter<(P, V)>,
    /// The final punctuation to maybe include.
    next: Option<P>,
}
impl<V, P> Iterator for IntoPuncts<V, P> {
    type Item = P;

    #[inline]
    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next element in the list
        if let Some((p, _)) = self.data.next() {
            Some(p)
        } else if let Some(next) = self.next.take() {
            // One more pair to go!
            Some(next)
        } else {
            // Done
            None
        }
    }
}



/// Iterates over the pairs in a [`PunctuatedTrailing`] by reference.
#[derive(Debug)]
pub struct Iter<'p, V, P> {
    /// A value buffer from the last time around.
    prev: Option<&'p V>,
    /// An iterator over the rest of the elements.
    data: std::slice::Iter<'p, (P, V)>,
    /// The final punctuation to maybe include.
    next: Option<&'p P>,
}
impl<'p, V, P> Iterator for Iter<'p, V, P> {
    type Item = (&'p V, Option<&'p P>);

    #[inline]
    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next element in the list
        if let Some((p, v)) = self.data.next() {
            let ret = (self.prev.take().unwrap_or_else(|| panic!("Empty prev mid-iteration")), Some(p));
            self.prev = Some(v);
            Some(ret)
        } else if let (Some(prev), next) = (self.prev.take(), self.next.take()) {
            // One more pair to go!
            Some((prev, next))
        } else {
            // Done
            None
        }
    }
}

/// Iterates over the pairs in a [`PunctuatedTrailing`] by mutable reference.
#[derive(Debug)]
pub struct IterMut<'p, V, P> {
    /// A value buffer from the last time around.
    prev: Option<&'p mut V>,
    /// An iterator over the rest of the elements.
    data: std::slice::IterMut<'p, (P, V)>,
    /// The final punctuation to maybe include.
    next: Option<&'p mut P>,
}
impl<'p, V, P> Iterator for IterMut<'p, V, P> {
    type Item = (&'p mut V, Option<&'p mut P>);

    #[inline]
    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next element in the list
        if let Some((p, v)) = self.data.next() {
            let ret = (self.prev.take().unwrap_or_else(|| panic!("Empty prev mid-iteration")), Some(p));
            self.prev = Some(v);
            Some(ret)
        } else if let (Some(prev), next) = (self.prev.take(), self.next.take()) {
            // One more pair to go!
            Some((prev, next))
        } else {
            // Done
            None
        }
    }
}

/// Iterates over the pairs in a [`PunctuatedTrailing`] by ownership.
#[derive(Debug)]
pub struct IntoIter<V, P> {
    /// A value buffer from the last time around.
    prev: Option<V>,
    /// An iterator over the rest of the elements.
    data: std::vec::IntoIter<(P, V)>,
    /// The final punctuation to maybe include.
    next: Option<P>,
}
impl<V, P> Iterator for IntoIter<V, P> {
    type Item = (V, Option<P>);

    #[inline]
    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next element in the list
        if let Some((p, v)) = self.data.next() {
            let ret = (self.prev.take().unwrap_or_else(|| panic!("Empty prev mid-iteration")), Some(p));
            self.prev = Some(v);
            Some(ret)
        } else if let (Some(prev), next) = (self.prev.take(), self.next.take()) {
            // One more pair to go!
            Some((prev, next))
        } else {
            // Done
            None
        }
    }
}





/***** AUXILLARY *****/
/// Represents either a value or a punctuation, to determine what to give next.
#[derive(Clone, Copy, Debug, EnumDebug, Eq, Hash, PartialEq)]
pub enum NextValue {
    /// The next value should be a value.
    Value,
    /// The next value should be a punctuation.
    Punctuation,
}





/***** LIBRARY *****/
/// Defines a wrapper around a [`Vec`] that makes it punctuated.
///
/// This version represents mandatory punctuation in-between elements, and optional trailing at the end.
///
/// See the list of features to find other versions.
///
/// # Generics
/// - `V`: The type of the value that is found in the list.
/// - `P`: The type of the punctuation/separator.
#[derive(Clone)]
pub struct PunctuatedTrailing<V, P> {
    /// The first element in the list, that hasn't got any punctuation.
    first: Option<Box<V>>,
    /// The list of elements stored within.
    data:  Vec<(P, V)>,
    /// Any buffered punctuation. Also acts as trailing comma if present.
    trail: Option<Box<P>>,
}
impl<V, P> Default for PunctuatedTrailing<V, P> {
    #[inline]
    fn default() -> Self { Self::new() }
}
impl<V, P> PunctuatedTrailing<V, P> {
    /// Constructor for the PunctuatedTrailing that initializes it as an empty list.
    ///
    /// # Returns
    /// A new PunctuatedTrailing with no elements or punctuation.
    #[inline]
    pub fn new() -> Self { Self { first: None, data: Vec::new(), trail: None } }

    /// Constructor for the PunctuatedTrailing that initializes it as an empty list, but with space pre-allocated.
    ///
    /// # Arguments
    /// - `capacity`: The minimum number of elements to allocate for. May be bigger if that's more efficient / better aligned.
    ///
    /// # Returns
    /// A new PunctuatedTrailing with no elements or punctuation.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self { Self { first: None, data: Vec::with_capacity(capacity.saturating_sub(1)), trail: None } }

    /// Adds a new value to the end of the list.
    ///
    /// # Arguments
    /// - `value`: The value of type `V` to add to the list.
    ///
    /// # Panics
    /// This function may panic if a punctuation wasn't pushed before.
    #[track_caller]
    pub fn push_value(&mut self, value: impl Into<V>) {
        // Add it to first if not given already
        if self.first.is_none() {
            self.first = Some(Box::new(value.into()));
        } else if let Some(p) = self.trail.take() {
            self.data.push((*p, value.into()));
        } else {
            panic!("Push a punctuation first before adding the next value");
        }
    }

    /// Adds a new punctuation to the end of the list.
    ///
    /// # Arguments
    /// - `punct`: The punctuation of type `P` to add to the list.
    ///
    /// # Panics
    /// This function may panic if there was already a punctuation pushed before.
    #[track_caller]
    pub fn push_punct(&mut self, punct: impl Into<P>) {
        // Add it to first if not given already
        if self.accepts_punct() {
            self.trail = Some(Box::new(punct.into()));
        } else {
            panic!("Push a value first before adding the next punctuation");
        }
    }

    /// Removes the most recent value from the list and returns it.
    ///
    /// This function skips punctuation, discarding it. Use [`Self::pop()`](PunctuatedTrailing::pop()) to get it.
    ///
    /// # Returns
    /// A value of type `V` that was removed, or [`None`] if the list is empty.
    pub fn pop_value(&mut self) -> Option<V> {
        // Always discard the trailing punctuation
        self.trail = None;
        // Get the most recent one from the list
        if let Some((_, v)) = self.data.pop() { Some(v) } else { self.first.take().map(|v| *v) }
    }

    /// Removes the most recent punctuation from the list and returns it.
    ///
    /// This function skips values, discarding it. Use [`Self::pop()`](PunctuatedTrailing::pop()) to get it.  
    /// This behaviour may also happen if there is only a value left in the list (despite [`None`] being returned).
    ///
    /// # Returns
    /// A punctuation of type `P` that was removed, or [`None`] if the list is empty.
    pub fn pop_punct(&mut self) -> Option<P> {
        // Get the most recent one from the list
        if let Some(p) = self.trail.take() {
            Some(*p)
        } else if let Some((p, _)) = self.data.pop() {
            Some(p)
        } else {
            self.first = None;
            None
        }
    }

    /// Removes the most recent (value, punctuation) pair from the list and returns it.
    ///
    /// The punctuation is [`None`] if there is no trailing one.
    ///
    /// # Returns
    /// A tuple of a value of type `V` and a(n optional) punctuation of type `P` that was removed, or [`None`] if the list is empty.
    pub fn pop(&mut self) -> Option<(V, Option<P>)> {
        // Get the trailing punctuation
        let punct: Option<P> = self.trail.take().map(|p| *p);
        // Get the most recent one from the list
        if let Some((p, v)) = self.data.pop() {
            // Set the new trail
            self.trail = Some(Box::new(p));
            Some((v, punct))
        } else {
            self.first.take().map(|v| (*v, punct))
        }
    }

    /// Checks whether there should a value next.
    ///
    /// # Returns
    /// True if nothing or a punctuation was pushed last; or false otherwise.
    ///
    /// # Example
    /// ```rust
    /// use ast_toolkit_punctuated::PunctuatedTrailing;
    ///
    /// let mut list = PunctuatedTrailing::<i32, char>::new();
    /// assert_eq!(list.accepts_value(), true);
    ///
    /// list.push_value(1);
    /// assert_eq!(list.accepts_value(), false);
    ///
    /// list.push_punct(',');
    /// assert_eq!(list.accepts_value(), true);
    ///
    /// list.push_value(2);
    /// assert_eq!(list.accepts_value(), false);
    /// ```
    #[inline]
    pub fn accepts_value(&self) -> bool { self.first.is_none() || self.trail.is_some() }

    /// Checks whether there should be a punctuation next.
    ///
    /// # Returns
    /// True if a value was pushed last; or false otherwise.
    ///
    /// # Example
    /// ```rust
    /// use ast_toolkit_punctuated::PunctuatedTrailing;
    ///
    /// let mut list = PunctuatedTrailing::<i32, char>::new();
    /// assert_eq!(list.accepts_punct(), false);
    ///
    /// list.push_value(1);
    /// assert_eq!(list.accepts_punct(), true);
    ///
    /// list.push_punct(',');
    /// assert_eq!(list.accepts_punct(), false);
    ///
    /// list.push_value(2);
    /// assert_eq!(list.accepts_punct(), true);
    /// ```
    #[inline]
    pub fn accepts_punct(&self) -> bool { self.first.is_some() && self.trail.is_none() }

    /// Checks what the next value should be.
    ///
    /// # Returns
    /// [`NextValue::Value`] if a value should be given next (i.e., [`Self::accepts_value()`](PunctuatedTrailing::accepts_value()) is true);  
    /// or [`NextValue::Punctuation`] if a punctuation should be given next (i.e., [`Self::accepts_punct()`](PunctuatedTrailing::accepts_punct()) is true)
    ///
    /// # Example
    /// ```rust
    /// use ast_toolkit_punctuated::{NextValue, PunctuatedTrailing};
    ///
    /// let mut list = PunctuatedTrailing::<i32, char>::new();
    /// assert_eq!(list.accepts(), NextValue::Value);
    ///
    /// list.push_value(1);
    /// assert_eq!(list.accepts(), NextValue::Punctuation);
    ///
    /// list.push_punct(',');
    /// assert_eq!(list.accepts(), NextValue::Value);
    ///
    /// list.push_value(2);
    /// assert_eq!(list.accepts(), NextValue::Punctuation);
    /// ```
    #[inline]
    pub fn accepts(&self) -> NextValue { if self.accepts_value() { NextValue::Value } else { NextValue::Punctuation } }

    /// Returns an iterator over references to the values in this Puncuated.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values by reference.
    #[inline]
    pub fn values(&self) -> Values<V, P> { Values { prev: self.first.as_ref().map(|v| v.as_ref()), data: self.data.iter() } }

    /// Returns an iterator over mutable references to the values in this Puncuated.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values by mutable reference.
    #[inline]
    pub fn values_mut(&mut self) -> ValuesMut<V, P> { ValuesMut { prev: self.first.as_mut().map(|v| v.as_mut()), data: self.data.iter_mut() } }

    /// Returns an iterator the values in this Puncuated.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values by ownership.
    #[inline]
    pub fn into_values(self) -> IntoValues<V, P> { IntoValues { prev: self.first.map(|v| *v), data: self.data.into_iter() } }

    /// Returns an iterator over references to the punctuation in this Puncuated.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the punctuation by reference.
    #[inline]
    pub fn puncts(&self) -> Puncts<V, P> { Puncts { data: self.data.iter(), next: self.trail.as_ref().map(|p| p.as_ref()) } }

    /// Returns an iterator over mutable references to the values in this Puncuated.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values by mutable reference.
    #[inline]
    pub fn puncts_mut(&mut self) -> PunctsMut<V, P> { PunctsMut { data: self.data.iter_mut(), next: self.trail.as_mut().map(|p| p.as_mut()) } }

    /// Returns an iterator the values in this Puncuated.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values by ownership.
    #[inline]
    pub fn into_puncts(self) -> IntoPuncts<V, P> { IntoPuncts { data: self.data.into_iter(), next: self.trail.map(|p| *p) } }

    /// Returns an iterator over references to pairs of value and punctuation in this Puncuated.
    ///
    /// It is guaranteed that the punctuation-half will only ever be [`None`] on the last element if a trailing comma is not given.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values & punctuation by reference.
    #[inline]
    pub fn pairs(&self) -> Iter<V, P> {
        Iter { prev: self.first.as_ref().map(|v| v.as_ref()), data: self.data.iter(), next: self.trail.as_ref().map(|p| p.as_ref()) }
    }

    /// Returns an iterator over mutable references to pairs of value and punctuation in this Puncuated.
    ///
    /// It is guaranteed that the punctuation-half will only ever be [`None`] on the last element if a trailing comma is not given.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values & punctuation by mutable reference.
    #[inline]
    pub fn pairs_mut(&mut self) -> IterMut<V, P> {
        IterMut { prev: self.first.as_mut().map(|v| v.as_mut()), data: self.data.iter_mut(), next: self.trail.as_mut().map(|p| p.as_mut()) }
    }

    /// Returns an iterator over pairs of value and punctuation in this Puncuated.
    ///
    /// It is guaranteed that the punctuation-half will only ever be [`None`] on the last element if a trailing comma is not given.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values & punctuation by ownership.
    #[inline]
    pub fn into_pairs(self) -> IntoIter<V, P> { IntoIter { prev: self.first.map(|v| *v), data: self.data.into_iter(), next: self.trail.map(|p| *p) } }

    /// Gets the total number of elements in the list.
    ///
    /// Note: this counts values, not punctuations.
    ///
    /// # Returns
    /// The number of values in this PunctuatedTrailing.
    #[inline]
    pub fn len(&self) -> usize { self.data.len() + if self.first.is_some() { 1 } else { 0 } }

    /// Gets the reserved space for elements in the list.
    ///
    /// Note: this counts values, not punctuations.
    ///
    /// Be aware, do not use this to assume whether (re)allocations happen or not. The list buffers trailing commas, which are allocated upon addition.
    ///
    /// # Returns
    /// The number of values for which we have allocated space in this PunctuatedTrailing.
    #[inline]
    pub fn capacity(&self) -> usize { 1 + self.data.capacity() }

    /// Returns whether there are any _values_ in this list.
    ///
    /// If there's only punctuation, this is ignored.
    ///
    /// # Returns
    /// True if it's empty, or false otherwise.
    #[inline]
    pub fn is_empty(&self) -> bool {
        // NOTE: This _should_ be enough. Else there's bugs elsewhere in the program.
        self.first.is_none()
    }
}

impl<V: Debug, P: Debug> Debug for PunctuatedTrailing<V, P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut list: DebugList = f.debug_list();
        if let Some(first) = &self.first {
            list.entry(first);
            for (p, v) in &self.data {
                list.entry(p);
                list.entry(v);
            }
            if let Some(trail) = &self.trail {
                list.entry(trail);
            }
        }
        list.finish()
    }
}
impl<V, P> Index<usize> for PunctuatedTrailing<V, P> {
    type Output = V;

    #[inline]
    #[track_caller]
    fn index(&self, index: usize) -> &Self::Output {
        if index == 0 {
            match &self.first {
                Some(v) => v,
                None => panic!("Index {index} is out-of-bounds for a Punctuated with 0 values"),
            }
        } else {
            match self.data.get(index - 1) {
                Some((_, v)) => v,
                None => panic!("Index {} is out-of-bounds for a Punctuated with {} values", index, self.len()),
            }
        }
    }
}
impl<V, P> IndexMut<usize> for PunctuatedTrailing<V, P> {
    #[inline]
    #[track_caller]
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if index == 0 {
            match &mut self.first {
                Some(v) => v,
                None => panic!("Index {index} is out-of-bounds for a Punctuated with 0 values"),
            }
        } else {
            let self_len: usize = self.len();
            match self.data.get_mut(index - 1) {
                Some((_, v)) => v,
                None => panic!("Index {index} is out-of-bounds for a Punctuated with {self_len} values"),
            }
        }
    }
}
impl<V, P> Index<ValueIndex> for PunctuatedTrailing<V, P> {
    type Output = V;

    #[inline]
    #[track_caller]
    fn index(&self, index: ValueIndex) -> &Self::Output { &self[*index] }
}
impl<V, P> IndexMut<ValueIndex> for PunctuatedTrailing<V, P> {
    #[inline]
    #[track_caller]
    fn index_mut(&mut self, index: ValueIndex) -> &mut Self::Output { &mut self[*index] }
}
impl<V, P> Index<PunctIndex> for PunctuatedTrailing<V, P> {
    type Output = P;

    #[inline]
    #[track_caller]
    fn index(&self, index: PunctIndex) -> &Self::Output {
        let self_len: usize = self.len();
        if *index == self_len - 1 {
            // It's the last one
            match &self.trail {
                Some(p) => p,
                None => panic!("Index {} is out-of-bounds for a Punctuated with {} punctuations", *index, self_len),
            }
        } else {
            match self.data.get(*index) {
                Some((p, _)) => p,
                None => panic!("Index {} is out-of-bounds for a Punctuated with {} punctuations", *index, self_len),
            }
        }
    }
}
impl<V, P> IndexMut<PunctIndex> for PunctuatedTrailing<V, P> {
    #[inline]
    #[track_caller]
    fn index_mut(&mut self, index: PunctIndex) -> &mut Self::Output {
        let self_len: usize = self.len();
        if *index == self_len - 1 {
            // It's the last one
            match &mut self.trail {
                Some(p) => p,
                None => panic!("Index {} is out-of-bounds for a Punctuated with {} punctuations", *index, self_len),
            }
        } else {
            match self.data.get_mut(*index) {
                Some((p, _)) => p,
                None => panic!("Index {} is out-of-bounds for a Punctuated with {} punctuations", *index, self_len),
            }
        }
    }
}

impl<V, P> IntoIterator for PunctuatedTrailing<V, P> {
    type IntoIter = IntoIter<V, P>;
    type Item = (V, Option<P>);

    #[inline]
    fn into_iter(self) -> Self::IntoIter { self.into_pairs() }
}
impl<'p, V, P> IntoIterator for &'p PunctuatedTrailing<V, P> {
    type IntoIter = Iter<'p, V, P>;
    type Item = (&'p V, Option<&'p P>);

    #[inline]
    fn into_iter(self) -> Self::IntoIter { self.pairs() }
}
impl<'p, V, P> IntoIterator for &'p mut PunctuatedTrailing<V, P> {
    type IntoIter = IterMut<'p, V, P>;
    type Item = (&'p mut V, Option<&'p mut P>);

    #[inline]
    fn into_iter(self) -> Self::IntoIter { self.pairs_mut() }
}

impl<V: Eq, P> Eq for PunctuatedTrailing<V, P> {}
impl<V: Hash, P> Hash for PunctuatedTrailing<V, P> {
    /// Implements hashing for the Punctuated.
    ///
    /// Note that this ignores the punctuation!
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        for v in self.values() {
            v.hash(state);
        }
    }
}
impl<V: PartialEq, P> PartialEq for PunctuatedTrailing<V, P> {
    /// Implements equality for the Punctuated.
    ///
    /// Note that this ignores equality of the punctuation!
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (lhs, rhs) in self.values().zip(other.values()) {
            if lhs != rhs {
                return false;
            }
        }
        true
    }

    /// Implements inequality for the Punctuated.
    ///
    /// Note that this ignores equality of the punctuation!
    #[inline]
    fn ne(&self, other: &Self) -> bool {
        if self.len() == other.len() {
            return false;
        }
        for (lhs, rhs) in self.values().zip(other.values()) {
            if lhs == rhs {
                return false;
            }
        }
        true
    }
}
