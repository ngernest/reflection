# Experiments with Type-based Reflection

A repo to experiment with type-based reflection in Haskell. 

The following files are adapted from lecture notes by Stephanie Weirich ([UPenn CIS 552, Fall 2012](https://www.seas.upenn.edu/~cis5520/12fa/schedule.html)):
- [Types.hs](./src/Types.hs) (Reifying types using GADTs)
- [TDP.hs](./src/TDP.hs) (type-directed programming)
- [State.hs](./src/State.hs) (a simple state monad)

[Gen.hs](./src/Gen.hs) contains a QuickCheck generator for intrinsically-typed
symbolic expressions (à la Mica), written by Stephanie.

- Note: [TDP2.hs](./src/TDP2.hs) and [Types2.hs](./src/Types2.hs) have been commneted out for now due to compilation issues with TemplateHaskell.

(Note: I've omitted the Template Haskell code from the lecture notes)

This project compiles with `stack build` (GHC 8.10.7). A REPL can be started with `stack ghci`.
