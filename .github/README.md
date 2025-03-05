[MultiChor](https://github.com/ShapeOfMatter/MultiChor) is a library for writing choreographic programs in Haskell.
This is a fork of it for exploring the ramifications of more minimal foundational APIs. 

## Goals

- [x] Remove the freer monad system
  - Why? I'm not sure this actually offers any "research" advantage...
- [x] Derive `flatten` and `othersForget`.
- [ ] Replace `congruently` with `naked` (and simplify `locally` and remove `Unwrap(s)`).
- [ ] Make `Located` a derived thing.
