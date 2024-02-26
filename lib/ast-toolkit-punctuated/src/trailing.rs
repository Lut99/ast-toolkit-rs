//  TRAILING.rs
//    by Lut99
//
//  Created:
//    26 Feb 2024, 14:08:18
//  Last edited:
//    26 Feb 2024, 16:03:40
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the "trailing" punctuated list, which has mandatory punctuation
//!   and optional trailing whitespace.
//

use std::fmt::{Debug, DebugList, Formatter, Result as FResult};

use enum_debug::EnumDebug;


/***** ITERATORS *****/
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
    pub fn values<'p>(
        &'p self,
    ) -> std::iter::Chain<std::option::IntoIter<&'p V>, std::iter::Map<std::slice::Iter<'p, (P, V)>, impl FnMut(&'p (P, V)) -> &'p V>> {
        self.first.as_ref().map(|v| v.as_ref()).into_iter().chain(self.data.iter().map(|(_, v)| v))
    }

    /// Returns an iterator over mutable references to the values in this Puncuated.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values by mutable reference.
    #[inline]
    pub fn values_mut<'p>(
        &'p mut self,
    ) -> std::iter::Chain<std::option::IntoIter<&'p mut V>, std::iter::Map<std::slice::IterMut<'p, (P, V)>, impl FnMut(&'p mut (P, V)) -> &'p mut V>>
    {
        self.first.as_mut().map(|v| v.as_mut()).into_iter().chain(self.data.iter_mut().map(|(_, v)| v))
    }

    /// Returns an iterator the values in this Puncuated.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values by ownership.
    #[inline]
    pub fn into_values(self) -> std::iter::Chain<std::option::IntoIter<V>, std::iter::Map<std::vec::IntoIter<(P, V)>, impl FnMut((P, V)) -> V>> {
        self.first.map(|v| *v).into_iter().chain(self.data.into_iter().map(|(_, v)| v))
    }

    /// Returns an iterator over references to the punctuation in this Puncuated.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the punctuation by reference.
    #[inline]
    pub fn puncts<'p>(
        &'p self,
    ) -> std::iter::Chain<std::iter::Map<std::slice::Iter<'p, (P, V)>, impl FnMut(&'p (P, V)) -> &'p P>, std::option::IntoIter<&'p P>> {
        self.data.iter().map(|(p, _)| p).chain(self.trail.as_ref().map(|p| p.as_ref()))
    }

    /// Returns an iterator over mutable references to the values in this Puncuated.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values by mutable reference.
    #[inline]
    pub fn puncts_mut<'p>(
        &'p mut self,
    ) -> std::iter::Chain<std::iter::Map<std::slice::IterMut<'p, (P, V)>, impl FnMut(&'p mut (P, V)) -> &'p mut P>, std::option::IntoIter<&'p mut P>>
    {
        self.data.iter_mut().map(|(p, _)| p).chain(self.trail.as_mut().map(|p| p.as_mut()))
    }

    /// Returns an iterator the values in this Puncuated.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values by ownership.
    #[inline]
    pub fn into_puncts(self) -> std::iter::Chain<std::iter::Map<std::vec::IntoIter<(P, V)>, impl FnMut((P, V)) -> P>, std::option::IntoIter<P>> {
        self.data.into_iter().map(|(p, _)| p).chain(self.trail.map(|p| *p))
    }

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
