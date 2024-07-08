Implement a small ("van Laarhoven" style) lens library including lenses, prisms, traversals, folds, and isos.

Lenses, or "functional references" are a toolkit for "focusing" on inner parts of values. By constructing a set of lenses which provide focus on the basic elementary parts of any type you define you can compose these into more sophisticated lenses which probe into deeply nested types.

Generalizations of lenses include

    Traversals: lenses which focus on 0, 1, or many inner pieces of the same type
    Folds: computations which "get" from 0, 1, or many inner pieces and summarize them somehow
    Prisms: lenses which focus on 0 or 1 inner pieces, lenses which can fail
    Isos: lenses which witness a subpart which is equal to the whole---witnesses of isomorphisms between subparts and wholes

This challenge asks that you produce a small, but comprehensive, lens library.

Irrelevant hints: Add {-# LANGUAGE TupleSections #-} so you can use (, a) instead of \x -> (x, a) in your code.

Relevant hint: you can implement set and to by using over and coerce respectively.
