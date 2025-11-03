//  HELLO WORLD.rs
//    by Lut99
//
//  Description:
//!   An example showing the basics of the parsing library.
//

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_nibble::{InputStream, NibbleError, Parser, branch};


/***** LIBRARY *****/
struct NotMatched<T>(PhantomData<T>);
impl<T> Debug for NotMatched<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut f = f.debug_tuple("NotMatched");
        f.field(&std::any::type_name::<T>());
        f.finish()
    }
}
impl<T> Display for NotMatched<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "Did not find {}", std::any::type_name::<T>()) }
}
impl<T> Error for NotMatched<T> {}



struct GreetingHelloWorld;
impl<I: InputStream<Elem = u8>> Parser<I> for GreetingHelloWorld {
    type Error = NotMatched<Self>;

    #[inline]
    async fn parse(input: I) -> Result<Self, NibbleError<Self::Error, <I as InputStream>::Error>> {
        if input.match_head(b"Hello, world!").await? { Ok(Self) } else { Err(NibbleError::Parse(NotMatched(PhantomData))) }
    }
}



struct GreetingHelloThere;
impl<I: InputStream<Elem = u8>> Parser<I> for GreetingHelloThere {
    type Error = NotMatched<Self>;

    #[inline]
    async fn parse(input: I) -> Result<Self, NibbleError<Self::Error, <I as InputStream>::Error>> {
        if input.match_head(b"Hello there!").await? { Ok(Self) } else { Err(NibbleError::Parse(NotMatched(PhantomData))) }
    }
}



struct GreetingGoodbyeWorld;
impl<I: InputStream<Elem = u8>> Parser<I> for GreetingGoodbyeWorld {
    type Error = NotMatched<Self>;

    #[inline]
    async fn parse(input: I) -> Result<Self, NibbleError<Self::Error, <I as InputStream>::Error>> {
        if input.match_head(b"Goodbye, world!").await? { Ok(Self) } else { Err(NibbleError::Parse(NotMatched(PhantomData))) }
    }
}



enum Greeting {
    HelloWorld(GreetingHelloWorld),
    HelloThere(GreetingHelloThere),
    GoodbyeWorld(GreetingGoodbyeWorld),
}
impl<I: InputStream<Elem = u8>> Parser<I> for Greeting {
    type Error = Infallible;

    #[inline]
    async fn parse(input: I) -> Result<Self, NibbleError<Self::Error, I::Error>> {
        branch!(GreetingHelloWorld::parse(input.clone()), GreetingHelloThere::parse(input.clone()), GreetingGoodbyeWorld::parse(input));
        todo!()
    }
}





/***** ENTRYPOINT *****/
fn main() {}
