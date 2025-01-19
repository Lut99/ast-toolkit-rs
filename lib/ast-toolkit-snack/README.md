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
