# Experiments with Type-based Reflection

A repo to experiment with type-based reflection in Haskell. 

The following files are adapted from lecture notes by Stephanie Weirich ([UPenn CIS 552, Fall 2012](https://www.seas.upenn.edu/~cis5520/12fa/schedule.html)):
- [Types.hs](./src/Types.hs) (Reifying types using GADTs)
- [Types2.hs](./src/Types2.hs) (a variant of the `Types` module that uses the `Typeable` library)
- [TDP.hs](./src/TDP.hs) (type-directed programming, datatype-generic equality)
- [State.hs](./src/State.hs) (a simple state monad)

(Note: I've omitted the Template Haskell code from the lecture notes)

This project compiles with `stack build` (GHC 8.10.7). A REPL can be started with `stack ghci`.
