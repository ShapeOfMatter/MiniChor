[MultiChor](https://github.com/ShapeOfMatter/MultiChor) is a library for writing choreographic programs in Haskell.
This is a fork of it for exploring the ramifications of more minimal foundational APIs. 

## Goals

- [x] Remove the freer monad system
  - Why? I'm not sure this actually offers any "research" advantage...
- [x] Derive `flatten` and `othersForget`.
- [x] Replace `congruently` with `naked` (and simplify `locally` and remove `Unwrap(s)`).
- [x] Make `Located` a derived thing.
- [x] Demote `Located` to a type alias.
- [ ] Shim the core to provide the best possible user-facing API.
- [ ] Do a real usability study to compare MiniChor with MultiChor.
- [ ] Do performance profiling.
