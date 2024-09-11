//  NORMAL.rs
//    by Lut99
//
//  Created:
//    26 Feb 2024, 16:00:14
//  Last edited:
//    26 Mar 2024, 18:05:24
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the "normal" punctuated list, which has mandatory
//!   punctuation and optional trailing whitespace.
//

use std::fmt::{Debug, DebugList, Formatter, Result as FResult};
use std::hash::{Hash, Hasher};
use std::ops::{Index, IndexMut};

use crate::common::{PunctIndex, ValueIndex};


/***** MACROS *****/
/// Builds a [`Punctuated`] conveniently from a list.
///
/// Your items must match the order required by the punctuated. I.e., start with a value, then alternate commas and values:
/// ```rust
/// use ast_toolkit_punctuated::{punct, PunctIndex, Punctuated};
///
/// #[derive(Debug, PartialEq)]
/// struct Value;
/// #[derive(Debug, PartialEq)]
/// struct Punct;
///
/// let punct: Punctuated<Value, Punct> = punct![];
/// assert!(punct.is_empty());
///
/// let punct: Punctuated<Value, Punct> = punct![v => Value];
/// assert_eq!(punct.len(), 1);
/// assert_eq!(punct[0], Value);
///
/// let punct: Punctuated<Value, Punct> = punct![v => Value, p => Punct, v => Value];
/// assert_eq!(punct.len(), 2);
/// assert_eq!(punct[0], Value);
/// assert_eq!(punct[PunctIndex(0)], Punct);
/// assert_eq!(punct[1], Value);
/// ```
#[macro_export]
macro_rules! punct {
    // Nothing to be done if nothing is given
    (__recursion $list:ident) => {};
    // Pop first value
    (__recursion $list:ident v => $value:expr $(, $($items:tt)+)?) => {
        $list.push_first($value);
        ::ast_toolkit_punctuated::punct!(__recursion $list $($($items)+)?);
    };
    // Pop punctuation, value pairs
    (__recursion $list:ident p => $punct:expr, v => $value:expr $(, $($items:tt)+)?) => {
        $list.push($punct, $value);
        ::ast_toolkit_punctuated::punct!(__recursion $list $($($items)+)?);
    };

    [$($items:tt)*] => {{
        // Call the macro
        let mut punct = ::ast_toolkit_punctuated::normal::Punctuated::new();
        ::ast_toolkit_punctuated::punct!(__recursion punct $($items)*);
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
}
impl<'p, V, P> Iterator for Puncts<'p, V, P> {
    type Item = &'p P;

    #[inline]
    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next element in the list
        self.data.next().map(|(p, _)| p)
    }
}

/// Iterates over the punctuation in a [`Punctuated`] by mutable reference.
#[derive(Debug)]
pub struct PunctsMut<'p, V, P> {
    /// An iterator over the rest of the elements.
    data: std::slice::IterMut<'p, (P, V)>,
}
impl<'p, V, P> Iterator for PunctsMut<'p, V, P> {
    type Item = &'p mut P;

    #[inline]
    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next element in the list
        self.data.next().map(|(p, _)| p)
    }
}

/// Iterates over the punctuation in a [`Punctuated`] by ownership.
#[derive(Debug)]
pub struct IntoPuncts<V, P> {
    /// An iterator over the rest of the elements.
    data: std::vec::IntoIter<(P, V)>,
}
impl<V, P> Iterator for IntoPuncts<V, P> {
    type Item = P;

    #[inline]
    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> { self.data.next().map(|(p, _)| p) }
}



/// Iterates over the pairs in a [`Punctuated`] by reference.
#[derive(Debug)]
pub struct Iter<'p, V, P> {
    /// A value buffer from the last time around.
    prev: Option<&'p V>,
    /// An iterator over the rest of the elements.
    data: std::slice::Iter<'p, (P, V)>,
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
        } else if let Some(prev) = self.prev.take() {
            // One more pair to go!
            Some((prev, None))
        } else {
            // Done
            None
        }
    }
}

/// Iterates over the pairs in a [`Punctuated`] by mutable reference.
#[derive(Debug)]
pub struct IterMut<'p, V, P> {
    /// A value buffer from the last time around.
    prev: Option<&'p mut V>,
    /// An iterator over the rest of the elements.
    data: std::slice::IterMut<'p, (P, V)>,
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
        } else if let Some(prev) = self.prev.take() {
            // One more pair to go!
            Some((prev, None))
        } else {
            // Done
            None
        }
    }
}

/// Iterates over the pairs in a [`Punctuated`] by ownership.
#[derive(Debug)]
pub struct IntoIter<V, P> {
    /// A value buffer from the last time around.
    prev: Option<V>,
    /// An iterator over the rest of the elements.
    data: std::vec::IntoIter<(P, V)>,
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
        } else if let Some(prev) = self.prev.take() {
            // One more pair to go!
            Some((prev, None))
        } else {
            // Done
            None
        }
    }
}





/***** LIBRARY *****/
/// Defines a wrapper around a [`Vec`] that makes it punctuated.
///
/// This version represents mandatory punctuation in-between elements and no trailing at the end.
///
/// See the list of features to find other versions.
///
/// # Generics
/// - `V`: The type of the value that is found in the list.
/// - `P`: The type of the punctuation/separator.
#[derive(Clone)]
pub struct Punctuated<V, P> {
    /// The first element in the list, that hasn't got any punctuation.
    first: Option<Box<V>>,
    /// The list of elements stored within.
    data:  Vec<(P, V)>,
}
impl<V, P> Punctuated<V, P> {
    /// Constructor for the Punctuated that initializes it as an as-empty-as-possible list.
    ///
    /// # Returns
    /// A new Punctuated with no elements or punctuation.
    #[inline]
    pub fn new() -> Self { Self { first: None, data: Vec::new() } }

    /// Constructor for the Punctuated that initializes it as an empty list, but with space pre-allocated.
    ///
    /// # Arguments
    /// - `capacity`: The minimum number of elements to allocate for. May be bigger if that's more efficient / better aligned.
    ///
    /// # Returns
    /// A new Punctuated with no elements or punctuation.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self { Self { first: None, data: Vec::with_capacity(capacity.saturating_sub(1)) } }

    /// Sets the first element in the list.
    ///
    /// # Arguments
    /// - `value`: The value of type `V` to add to the list.
    ///
    /// # Panics
    /// This function may panic if another element already exists.
    #[track_caller]
    pub fn push_first(&mut self, value: impl Into<V>) {
        if self.first.is_none() {
            self.first = Some(Box::new(value.into()));
        } else {
            panic!("A first value has already been pushed");
        }
    }

    /// Adds a new (punctuation, value) pair to the end of the list.
    ///
    /// # Arguments
    /// - `punct`: The punctuation of type `P` to add to the list.
    /// - `value`: The value of type `V` to add to the list.
    ///
    /// # Panics
    /// This function may panic no first value has been given.
    #[track_caller]
    pub fn push(&mut self, punct: impl Into<P>, value: impl Into<V>) {
        // Assert a first exists
        if self.first.is_none() {
            panic!("No first value given.")
        }

        // Add it to first if not given already
        self.data.push((punct.into(), value.into()));
    }

    /// Removes the most recent value from the list and returns it.
    ///
    /// This function skips punctuation, discarding it. Use [`Self::pop()`](Punctuated::pop()) to get it.
    ///
    /// # Returns
    /// A value of type `V` that was removed, or [`None`] if the list is empty.
    pub fn pop_value(&mut self) -> Option<V> {
        // Get the most recent one from the list
        if let Some((_, v)) = self.data.pop() { Some(v) } else { self.first.take().map(|v| *v) }
    }

    /// Removes the most recent punctuation from the list and returns it.
    ///
    /// This function skips values, discarding it. Use [`Self::pop()`](Punctuated::pop()) to get it.  
    /// This behaviour may also happen if there is only a value left in the list (despite [`None`] being returned).
    ///
    /// # Returns
    /// A punctuation of type `P` that was removed, or [`None`] if the list is empty.
    pub fn pop_punct(&mut self) -> Option<P> {
        // Get the most recent one from the list
        if let Some((p, _)) = self.data.pop() {
            Some(p)
        } else {
            self.first = None;
            None
        }
    }

    /// Removes the most recent (value, punctuation) pair from the list and returns it.
    ///
    /// The punctuation is only ever [`None`] if the first value is popped.
    ///
    /// # Returns
    /// A tuple of a value of type `V` and a(n optional) punctuation of type `P` that was removed, or [`None`] if the list is empty.
    pub fn pop(&mut self) -> Option<(V, Option<P>)> {
        // Get the most recent one from the list
        if let Some((p, v)) = self.data.pop() {
            // Set the new trail
            Some((v, Some(p)))
        } else {
            self.first.take().map(|v| (*v, None))
        }
    }

    /// Checks whether a first value can be given.
    ///
    /// # Returns
    /// True if the list is empty; or false otherwise.
    ///
    /// # Example
    /// ```rust
    /// use ast_toolkit_punctuated::Punctuated;
    ///
    /// let mut list = Punctuated::<i32, char>::new();
    /// assert_eq!(list.accepts_first(), true);
    ///
    /// list.push_first(1);
    /// assert_eq!(list.accepts_first(), false);
    ///
    /// list.push(',', 2);
    /// assert_eq!(list.accepts_first(), false);
    /// ```
    #[inline]
    pub fn accepts_first(&self) -> bool { self.first.is_none() }

    /// Checks whether a non-first value can be given.
    ///
    /// # Returns
    /// True if a value was pushed; or false otherwise.
    ///
    /// # Example
    /// ```rust
    /// use ast_toolkit_punctuated::Punctuated;
    ///
    /// let mut list = Punctuated::<i32, char>::new();
    /// assert_eq!(list.accepts_next(), false);
    ///
    /// list.push_first(1);
    /// assert_eq!(list.accepts_next(), true);
    ///
    /// list.push(',', 2);
    /// assert_eq!(list.accepts_next(), true);
    /// ```
    #[inline]
    pub fn accepts_next(&self) -> bool { self.first.is_some() }

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
    pub fn puncts(&self) -> Puncts<V, P> { Puncts { data: self.data.iter() } }

    /// Returns an iterator over mutable references to the values in this Puncuated.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values by mutable reference.
    #[inline]
    pub fn puncts_mut<'p>(&'p mut self) -> PunctsMut<V, P> { PunctsMut { data: self.data.iter_mut() } }

    /// Returns an iterator the values in this Puncuated.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values by ownership.
    #[inline]
    pub fn into_puncts(self) -> IntoPuncts<V, P> { IntoPuncts { data: self.data.into_iter() } }

    /// Returns an iterator over references to pairs of value and punctuation in this Puncuated.
    ///
    /// It is guaranteed that the punctuation-half will only ever be [`None`] on the last element if a trailing comma is not given.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values & punctuation by reference.
    #[inline]
    pub fn pairs(&self) -> Iter<V, P> { Iter { prev: self.first.as_ref().map(|v| v.as_ref()), data: self.data.iter() } }

    /// Returns an iterator over mutable references to pairs of value and punctuation in this Puncuated.
    ///
    /// It is guaranteed that the punctuation-half will only ever be [`None`] on the last element if a trailing comma is not given.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values & punctuation by mutable reference.
    #[inline]
    pub fn pairs_mut(&mut self) -> IterMut<V, P> { IterMut { prev: self.first.as_mut().map(|v| v.as_mut()), data: self.data.iter_mut() } }

    /// Returns an iterator over pairs of value and punctuation in this Puncuated.
    ///
    /// It is guaranteed that the punctuation-half will only ever be [`None`] on the last element if a trailing comma is not given.
    ///
    /// # Returns
    /// An [`Iterator`] that iterates over the values & punctuation by ownership.
    #[inline]
    pub fn into_pairs(self) -> IntoIter<V, P> { IntoIter { prev: self.first.map(|v| *v), data: self.data.into_iter() } }

    /// Clears all elements from this vector.
    ///
    /// Doesn't de-allocate the vector, so the capacity does not change.
    #[inline]
    pub fn clear(&mut self) {
        self.first = None;
        self.data.clear();
    }

    /// Reserves more space for values in this iterator.
    ///
    /// Note this may allocate for more elements than ask if deemed efficient or due to alignment or whatever, but not less.
    ///
    /// # Arguments
    /// - `additional`: The additional number of elements to reserve space for.
    #[inline]
    pub fn reserve(&mut self, additional: usize) { self.data.reserve(additional) }

    /// Gets the total number of elements in the list.
    ///
    /// Note: this counts values, not punctuations.
    ///
    /// # Returns
    /// The number of values in this Punctuated.
    #[inline]
    pub fn len(&self) -> usize { self.data.len() + if self.first.is_some() { 1 } else { 0 } }

    /// Gets the reserved space for elements in the list.
    ///
    /// Note: this counts values, not punctuations.
    ///
    /// Be aware, do not use this to assume whether (re)allocations happen or not. The list buffers trailing commas, which are allocated upon addition.
    ///
    /// # Returns
    /// The number of values for which we have allocated space in this Punctuated.
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

impl<V: Debug, P: Debug> Debug for Punctuated<V, P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut list: DebugList = f.debug_list();
        if let Some(first) = &self.first {
            list.entry(first);
            for (p, v) in &self.data {
                list.entry(p);
                list.entry(v);
            }
        }
        list.finish()
    }
}
impl<V, P> Index<usize> for Punctuated<V, P> {
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
impl<V, P> IndexMut<usize> for Punctuated<V, P> {
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
impl<V, P> Index<ValueIndex> for Punctuated<V, P> {
    type Output = V;

    #[inline]
    #[track_caller]
    fn index(&self, index: ValueIndex) -> &Self::Output { &self[*index] }
}
impl<V, P> IndexMut<ValueIndex> for Punctuated<V, P> {
    #[inline]
    #[track_caller]
    fn index_mut(&mut self, index: ValueIndex) -> &mut Self::Output { &mut self[*index] }
}
impl<V, P> Index<PunctIndex> for Punctuated<V, P> {
    type Output = P;

    #[inline]
    #[track_caller]
    fn index(&self, index: PunctIndex) -> &Self::Output {
        match self.data.get(*index) {
            Some((p, _)) => p,
            None => panic!("Index {} is out-of-bounds for a Punctuated with {} punctuations", *index, self.data.len()),
        }
    }
}
impl<V, P> IndexMut<PunctIndex> for Punctuated<V, P> {
    #[inline]
    #[track_caller]
    fn index_mut(&mut self, index: PunctIndex) -> &mut Self::Output {
        let data_len: usize = self.data.len();
        match self.data.get_mut(*index) {
            Some((p, _)) => p,
            None => panic!("Index {} is out-of-bounds for a Punctuated with {} punctuations", *index, data_len),
        }
    }
}

impl<V, P> IntoIterator for Punctuated<V, P> {
    type IntoIter = IntoIter<V, P>;
    type Item = (V, Option<P>);

    #[inline]
    fn into_iter(self) -> Self::IntoIter { self.into_pairs() }
}
impl<'p, V, P> IntoIterator for &'p Punctuated<V, P> {
    type IntoIter = Iter<'p, V, P>;
    type Item = (&'p V, Option<&'p P>);

    #[inline]
    fn into_iter(self) -> Self::IntoIter { self.pairs() }
}
impl<'p, V, P> IntoIterator for &'p mut Punctuated<V, P> {
    type IntoIter = IterMut<'p, V, P>;
    type Item = (&'p mut V, Option<&'p mut P>);

    #[inline]
    fn into_iter(self) -> Self::IntoIter { self.pairs_mut() }
}

impl<V: Eq, P> Eq for Punctuated<V, P> {}
impl<V: Hash, P> Hash for Punctuated<V, P> {
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
impl<V: PartialEq, P> PartialEq for Punctuated<V, P> {
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
