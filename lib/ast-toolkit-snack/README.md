# ast-toolkit-snack
Provides a parser-combinator framework heavily inspired by [nom](https://github.com/rust-bakery/nom), except that it gives up a little bit of performance over a much more human-friendly error experience.


## Philosophy
[Nom](https://github.com/rust-bakery/nom) is a great library, make no mistake; however, its parser errors can be a little bit unreadable by default. For example, the [BraneScript language](https://github.com/epi-project/brane)'s parser gives cryptic error messages when something as simple as a missing dot is encountered:
```plain
// TODO
```

Instead, this library aims to provide helpful error messages by annotating every combinator with what is expected:
```plain
// TODO
```

While this incurs a little more development work, the library's combinators have all been designed from the ground-up to be as elaborate as possible in what they expect, resulting in much more friendly error messages.

In addition to the friendlier error messages, its tight integration with the [`ast-toolkit-diagnostic`](../ast-toolkit-diagnostic/README.md), [`ast-toolkit-punctuated`](../ast-toolkit-punctuated/README.md) and [`ast-toolkit-span`](../ast-toolkit-span/README.md) libraries make it more convenient to work with the existing libraries aimed to improve user compiler experience with minimal additional development effort.


## Usage
<!-- TODO -->


## Installation
<!-- TODO -->

## A note on bounds
Throughout this library, you will see this pattern a lot:
```rust
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'s, S>, bound = (S: Spannable<'s>))]
struct Foo<S> {
    /* ...fields */
    span: Span<S>,
}
```

We use this library because `Span` has special bound requirements on `S`, which aren't supported by
the default derivation macros. Specifically, it has no requirements on `S` itself other than that
it implements `Spannable`, and the rest is required by it. It doesn't actually require `Debug` or
`Eq` or something on `S` itself. As such, we use this to specifically indicate that we have this
special bound.

Further, also note that `better_derive` has the default behaviour of implementing
```rust
impl<S> Debug for Foo<S>
where
    Span<S>: Debug,
{
    /* ... */
}
```
(i.e., derive based on field types, not generics)

However, for large projects like parsers of complicated languages, this bloats compile times to
unacceptable sizes. As such, we implement this to make it perform a little better.
