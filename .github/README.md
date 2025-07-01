[MultiChor](https://github.com/ShapeOfMatter/MultiChor) is a library for writing choreographic programs in Haskell.
This is a fork of it for exploring the ramifications of more minimal foundational APIs. 

MiniChor differs from MultiChor in a couple ways.
Removing the freer monad system may or may not have performance implicaitons, good tooling for measuring that has not yet been identified or developed. 
The more interesting difference is that in MiniChor "located" values are actually just an alias for choreographies!
In other words, MiniChor is a choreographic programming system in which computations can be ascribed to lists of participants, and there is no distinct notion of a "located value".

MiniChor is a research prototype; it's theoretical insights will affect future versions of MultiChor, but no future work is expected in this repository.
