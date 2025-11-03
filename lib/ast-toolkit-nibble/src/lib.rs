//  LIB.rs
//    by Lut99
//
//  Description:
//!   A re-imagining of the legendary `snack` library, which is based on
//!   [`nom`](https://docs.rs/nom).
//!
//!   This library completely forgoes the original `snack` view of combinators
//!   and instead adopts a more Rust-like approach: view inputs as streams
//!   instead of slices, and then replace backtracking with lookaheading in a
//!   breath-first fashion using Rust's [`Future`](std::future::Future)s.

use std::error::Error;


/***** LIBRARY *****/
#[derive(Debug)]
pub enum NibbleError<E, IE> {
    /// The input was ill-formed.
    Parse(E),
    /// The reader failed.
    Input(IE),
}

impl<E, IE> From<IE> for NibbleError<E, IE> {
    #[inline]
    fn from(value: IE) -> Self { Self::Input(value) }
}



pub trait InputStream: Clone {
    type Elem;
    type Error: Error;

    fn next(&self) -> impl Future<Output = Result<Option<Self::Elem>, Self::Error>>;

    #[inline]
    fn match_head<'a>(&self, needle: impl IntoIterator<Item = &'a Self::Elem>) -> impl Future<Output = Result<bool, Self::Error>>
    where
        Self::Elem: 'a + PartialEq,
    {
        async move {
            let mut elems = needle.into_iter();
            while let (Some(elem), Some(needle)) = (self.next().await?, elems.next()) {
                if &elem != needle {
                    return Ok(false);
                }
            }
            Ok(true)
        }
    }
}



pub trait Parser<I: InputStream>: Sized {
    type Error: Error;

    fn parse(input: I) -> impl Future<Output = Result<Self, NibbleError<Self::Error, I::Error>>>;
}



pub trait AtLeastOneBranch {
    fn at_least_one_branch(&self) -> bool;
}
impl<T1, T2, T3> AtLeastOneBranch for (Option<T1>, Option<T2>, Option<T3>) {
    fn at_least_one_branch(&self) -> bool { self.0.is_some() || self.1.is_some() || self.2.is_some() }
}



#[macro_export]
macro_rules! branch {
    ($($branches:expr),*) => {{
        fn at_least_one_branch<T: ::ast_toolkit_nibble::AtLeastOneBranch>(branches: &T) -> bool {
            <T as ::ast_toolkit_nibble::AtLeastOneBranch>::at_least_one_branch(branches)
        }

        let mut branches = ($(::std::option::Option::Some(Box::into_pin($branches)),)*);
        while at_least_one_branch(&branches) {
            if let Some(branch) = &mut branches.0 {
                let res = branch.poll(ctx);
            }
        }
    }};
}
