//  PROCEDURAL.rs
//    by Lut99
//
//  Created:
//    02 May 2024, 18:25:42
//  Last edited:
//    07 Mar 2025, 18:25:50
//  Auto updated?
//    Yes
//
//  Description:
//!   This module documents the uses of snack's procedural macros.
//!   
//!   # The `comb`-attribute macro
//!   The `comb`-attribute macro is the main macro contributed by the crate. It can be used to
//!   reduce a lot of boilerplate while implementing snack combinators.
//!   
//!   ## Usage
//!   You can use the macro by attaching it to a function of the following shape:
//!   ```rust
//!   use ast_toolkit_snack::comb;
//!   use ast_toolkit_span::Span;
//!
//!   #[comb(
//!   #   prefix = ast_toolkit_snack,
//!       expected = "some input"
//!   )]
//!   fn combinator<F, S>(input: Span<F, S>) -> _ {
//!       // ...
//!   #   todo!()
//!   }
//!   ```
//!   This allows you to write [nom](https://github.com/rust-bakery/nom)-style function combinators
//!   while still expressing a string for encoding what is expected by the input macro.
//!   
//!   ## Specifying output and errors
//!   By default, the macro assumes your combinator will yield [`Span`](ast_toolkit_span::Span)s
//!   and never fail (i.e., error types are [`Infallible`](std::convert::Infallible)). However,
//!   this is probably not accurate. You can control which types are returned using the `Output`,
//!   `Recoverable` and `Fatal`-attributes:
//!   ```rust
//!   use ast_toolkit_snack::span::MatchBytes;
//!   use ast_toolkit_snack::combinator::map;
//!   use ast_toolkit_snack::utf8::complete::tag;
//!   use ast_toolkit_snack::comb;
//!   use ast_toolkit_span::Span;
//!   
//!   struct Hello;
//!
//!   #[comb(
//!   #   prefix = ast_toolkit_snack,
//!       expected = "some input",
//!       output = Hello,
//!       recoverable = tag::Recoverable<'static, F, S>
//!   )]
//!   fn parse_and_map<F, S>(input: Span<F, S>) -> _
//!   where
//!       F: Clone,
//!       S: Clone + MatchBytes,
//!   {
//!       map(tag("hello"), |_| Hello).parse(input)
//!   }
//!   ```
//!
//!   ## Attributes
//!   The following attributes are supported after the `comb`-attribute:
//!   - `prefix = ...`: Changes the prefix used for accessing snack's traits. If omitted, the
//!     default prefix is `::ast_toolkit::snack`.
//!     
//!     For example:
//!     ```rust
//!     use ast_toolkit_snack::comb;
//!     use ast_toolkit_span::Span;
//!
//!     #[comb(
//!         prefix = ast_toolkit_snack,
//!         expected = "some input"
//!     )]
//!     fn combinator<F, S>(input: Span<F, S>) -> _ {
//!         // ...
//!     #   todo!()
//!     }
//!     ```
//!   - `module = ...`: Changes the name of the generated module in which the combinator struct and
//!     expects formatter are placed. If omitted, defaults to the name of the function itself.
//!     
//!     For example:
//!     ```rust
//!     use ast_toolkit_snack::comb;
//!     use ast_toolkit_span::Span;
//!
//!     #[comb(
//!     #   prefix = ast_toolkit_snack,
//!         expected = "some input"
//!     )]
//!     fn comb1<F, S>(input: Span<F, S>) -> _ {
//!         // ...
//!     #   todo!()
//!     }
//!
//!     #[comb(
//!     #   prefix = ast_toolkit_snack,
//!         module = foo,
//!         expected = "some other input"
//!     )]
//!     fn comb2<F, S>(input: Span<F, S>) -> _ {
//!         // ...
//!     #   todo!()
//!     }
//!     
//!     println!("{}", std::any::type_name::<comb1::Combinator<(), ()>>());
//!     println!("{}", std::any::type_name::<foo::Combinator<(), ()>>());
//!     ```
//!     
//!     For context, the generated code by the macro for `comb1` roughly looks as follows:
//!     ```ignore
//!     mod comb1 {
//!         struct ExpectsFormatter { /* ... */ }
//!         struct Combinator { /* ... */ }
//!     }
//!     fn comb1<F, S>(input: Span<F, S>) -> /* ... */ { /* ... */ }
//!     ```
//!     The name in `mod comb1` is what is being changed here.
//!   - `comb = ...` or `combinator = ...`: Changes the name of the generated combinator struct
//!     that actually implements [`Combinator`](crate::Combinator). If omitted, defaults to
//!     `Combinator`.
//!     
//!     For example:
//!     ```rust
//!     use ast_toolkit_snack::comb;
//!     use ast_toolkit_span::Span;
//!
//!     #[comb(
//!     #   prefix = ast_toolkit_snack,
//!         expected = "some input"
//!     )]
//!     fn comb1<F, S>(input: Span<F, S>) -> _ {
//!         // ...
//!     #   todo!()
//!     }
//!
//!     #[comb(
//!     #   prefix = ast_toolkit_snack,
//!         comb = Foo,
//!         expected = "some other input"
//!     )]
//!     fn comb2<F, S>(input: Span<F, S>) -> _ {
//!         // ...
//!     #   todo!()
//!     }
//!     
//!     println!("{}", std::any::type_name::<comb1::Combinator<(), ()>>());
//!     println!("{}", std::any::type_name::<comb2::Foo<(), ()>>());
//!     ```
//!     
//!     For context, the generated code by the macro for `comb1` roughly looks as follows:
//!     ```ignore
//!     mod comb1 {
//!         struct ExpectsFormatter { /* ... */ }
//!         struct Combinator { /* ... */ }
//!     }
//!     fn comb1<F, S>(input: Span<F, S>) -> /* ... */ { /* ... */ }
//!     ```
//!     The name in `struct Combinator` is what is being changed here.
//!   - `fmt = ...` or `formatter = ...`: Changes the name of the generated expects formatter that
//!     implements [`ExpectsFormatter`](crate::ExpectsFormatter). If omitted, defaults to
//!     `ExpectsForamtter`.
//!     
//!     For example:
//!     ```rust
//!     use ast_toolkit_snack::comb;
//!     use ast_toolkit_span::Span;
//!
//!     #[comb(
//!     #   prefix = ast_toolkit_snack,
//!         expected = "some input"
//!     )]
//!     fn comb1<F, S>(input: Span<F, S>) -> _ {
//!         // ...
//!     #   todo!()
//!     }
//!
//!     #[comb(
//!     #   prefix = ast_toolkit_snack,
//!         fmt = Foo,
//!         expected = "some other input"
//!     )]
//!     fn comb2<F, S>(input: Span<F, S>) -> _ {
//!         // ...
//!     #   todo!()
//!     }
//!     
//!     println!("{}", std::any::type_name::<comb1::ExpectsFormatter>());
//!     println!("{}", std::any::type_name::<comb2::Foo>());
//!     ```
//!     
//!     For context, the generated code by the macro for `comb1` roughly looks as follows:
//!     ```ignore
//!     mod comb1 {
//!         struct ExpectsFormatter { /* ... */ }
//!         struct Combinator { /* ... */ }
//!     }
//!     fn comb1<F, S>(input: Span<F, S>) -> /* ... */ { /* ... */ }
//!     ```
//!     The name in `struct ExpectsFormatter` is what is being changed here.
//!   - `expected = ...`: Describes what the combinator expected as input when it fails
//!     (recoverably). There are multiple ways to give arguments:
//!     - A string literal.
//!       ```rust
//!       use ast_toolkit_snack::comb;
//!       use ast_toolkit_span::Span;
//!    
//!       #[comb(
//!       #   prefix = ast_toolkit_snack,
//!           expected = "some input"
//!       )]
//!       fn combinator<F, S>(input: Span<F, S>) -> _ {
//!           // ...
//!       #   todo!()
//!       }
//!       ```
//!     - A formatter string in a tuple.
//!       ```rust
//!       use ast_toolkit_snack::comb;
//!       use ast_toolkit_span::Span;
//!  
//!       const NAME: &'static str = "thing";
//!    
//!       #[comb(
//!       #   prefix = ast_toolkit_snack,
//!           expected = ("a {NAME}")
//!       )]
//!       fn combinator<F, S>(input: Span<F, S>) -> _ {
//!           // ...
//!       #   todo!()
//!       }
//!       ```
//!   - `output = ...`: Changes the output type of the combinator. If omitted, defaults to a
//!     [`Span`](ast_toolkit_span::Span).
//!     
//!     For example:
//!     ```rust
//!     use ast_toolkit_snack::span::MatchBytes;
//!     use ast_toolkit_snack::combinator::map;
//!     use ast_toolkit_snack::utf8::complete::tag;
//!     use ast_toolkit_snack::comb;
//!     use ast_toolkit_span::Span;
//!     
//!     struct Hello;
//!   
//!     #[comb(
//!     #   prefix = ast_toolkit_snack,
//!         expected = "some input",
//!         output = Hello,
//!     )]
//!     fn hello<F, S>(input: Span<F, S>) -> _
//!     where
//!         F: Clone,
//!         S: Clone + MatchBytes,
//!     {
//!         Ok(map(tag("hello"), |_| Hello).parse(input).unwrap())
//!     }
//!     ```
//!   - `recoverable = ...`: Changes the recoverable error type for this combinator. If omitted,
//!     defaults to [`std::convert::Infallible`].
//!     
//!     > Tip: take a look at [`Expected`](crate::result::Expected) to be able to convert the
//!     > expects formatter of your closure into a recoverable error!
//!     
//!     For example:
//!     ```rust
//!     use ast_toolkit_snack::span::MatchBytes;
//!     use ast_toolkit_snack::combinator::map_recoverable;
//!     use ast_toolkit_snack::utf8::complete::tag;
//!     use ast_toolkit_snack::comb;
//!     use ast_toolkit_span::Span;
//!   
//!     #[comb(
//!     #   prefix = ast_toolkit_snack,
//!         expected = "some input",
//!         recoverable = &'static str,
//!     )]
//!     fn hello<F, S>(input: Span<F, S>) -> _
//!     where
//!         F: Clone,
//!         S: Clone + MatchBytes,
//!     {
//!         map_recoverable(tag("hello"), |_| "oops").parse(input)
//!     }
//!     ```
//!   - `fatal = ...`: Changes the fatal error type for this combinator. If omitted, defaults to
//!     [`std::convert::Infallible`].
//!     
//!     For example:
//!     ```rust
//!     use ast_toolkit_snack::comb;
//!     use ast_toolkit_span::Span;
//!     use ast_toolkit_snack::result::SnackError;
//!   
//!     #[comb(
//!     #   prefix = ast_toolkit_snack,
//!         expected = "some input",
//!         fatal = &'static str,
//!     )]
//!     fn always_fails<F, S>(_input: Span<F, S>) -> _ {
//!         Err(SnackError::Fatal("oops"))
//!     }
//!     ```
//!
//!   > Note that all attributes can be capitalized (e.g., `Output = ...` instead of
//!     `output = ...`).
