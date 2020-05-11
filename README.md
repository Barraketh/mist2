# MIST
A language for ergonomic type-level programming

## Top level concept
Every typed language has embedded inside it two languages: a language for manipulating values, and another for manipulating types.  The language of types is generally a DSL, responsible for defining types and asserting that they match function inputs / outputs. Its semantics are defined by the typechecker, and as such are generally very different than the semantics of the value language. This often makes complex type-level computations difficult to write and understand

Mist attempts to address this difficulty by using the same language for both types and values, and treating the typecheck as running a program that was compiled from the original source (more on that in the following section).
