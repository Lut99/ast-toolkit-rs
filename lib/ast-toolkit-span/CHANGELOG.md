# CHANGELOG for the `ast-toolkit-span` crate
This file keeps track of changes to the `ast-toolkit-span` crate.

The crate uses [semantic versioning](https://semver.org), and as such, breaking changes are indicated by **\[breaking\]**.


## v0.1.0 - ???
Initial release!

### Added
- The `Span`-struct, which can span over some kind of source to only select a subset of it.
- A `Spannable`-trait, which is used to determine what can be `Span`ned.
- A `Spanning`-trait, which is for AST nodes or other objects to return some `Span` they are wrapping.
