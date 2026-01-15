//  LIB.rs
//    by Lut99
//
//  Description:
//!   A re-imagining of the legendary `snack` library, which was based on
//!   [`nom`](https://docs.rs/nom).
//!
//!   This library completely forgoes the original `snack` view of combinators
//!   and instead adopts a more Rust-like approach: view inputs as streams
//!   instead of slices, and then replace backtracking with lookaheading in a
//!   breadth-first fashion using Rust's [`Future`](std::future::Future)s.

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
    type Output;
    type Error: Error;

    fn parse(input: I) -> impl Future<Output = Result<Self::Output, NibbleError<Self::Error, I::Error>>>;
}



// pub trait BranchingParser<I: InputStream>: Sized {
//     type Error: Error;

//     fn parse(input: I) -> impl Future<Output = Result<Self, NibbleError<Self::Error, I::Error>>>;
// }

// impl<I: InputStream, F> BranchingParser<I> for (F,) {
//     type Error = std::convert::Infallible;

//     #[inline]
//     async fn parse(input: I) -> Result<Self, NibbleError<Self::Error, <I as InputStream>::Error>> {
//         struct Brancher<F>(Option<::std::pin::Pin<Box<F>>>);
//         impl<F> Future for Brancher<F>
//         where
//             F: Future<Output = Result<Self, NibbleError<std::convert::Infallible, std::convert::Infallible>>> + Unpin,
//         {
//             type Output = Result<Self, NibbleError<std::convert::Infallible, std::convert::Infallible>>;

//             #[inline]
//             fn poll(mut self: ::std::pin::Pin<&mut Self>, ctx: &mut ::std::task::Context<'_>) -> ::std::task::Poll<Self::Output> {
//                 // Poll them all one-by-one
//                 let mut res: Option<_> = None;
//                 while self.0.is_some() {
//                     if let Some(branch) = &mut self.0 {
//                         if let ::std::task::Poll::Ready(value) = (*branch).as_mut().poll(ctx) {
//                             if res.is_some() {
//                                 todo!()
//                             }
//                             res = Some(value);
//                             self.0 = None;
//                         }
//                     }
//                     // If we got here, we've done an iteration; time for more input
//                     return ::std::task::Poll::Pending;
//                 }
//                 ::std::task::Poll::Ready(res.unwrap())
//             }
//         }

//         // OH NO we don't have the future instances here. Need `self`. Looks like it'll need be
//         // another trait...
//         // Brancher(Some(Box::new(F::)))
//         todo!()
//     }
// }



// pub trait AtLeastOneBranch {
//     fn at_least_one_branch(&self) -> bool;
// }
// impl<T1, T2, T3> AtLeastOneBranch for (Option<T1>, Option<T2>, Option<T3>) {
//     fn at_least_one_branch(&self) -> bool { self.0.is_some() || self.1.is_some() || self.2.is_some() }
// }



pub struct Brancher<I, T>(pub I, pub ::std::pin::Pin<Box<T>>);
impl<T1: Parser<I, Output = O>, T2: Parser<I, Output = O>, T3: Parser<I, Output = O>, I: InputStream, O> Future
    for Brancher<I, (Option<T1>, Option<T2>, Option<T3>)>
{
    type Output = O;

    #[inline]
    fn poll(mut self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
        // Try the branches one-by-one
        if let Some(branch) = &mut self.1.0 {
            let branch1 = unsafe { std::pin::Pin::map_unchecked_mut(self.1.as_mut(), |(branch, _, _)| branch) };
            if let ::std::task::Poll::Ready(value) = <T1 as Future>::poll(branch1, cx) {
                match value {}
            }
        }

        // All of them have been had
    }
}



#[macro_export]
macro_rules! branch {
    ($($branches:expr),* $(,)?) => {
        $crate::Brancher(::std::boxed::Box::pin(($($branches,)*))).await
    };
}
