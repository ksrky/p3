# P3: Programmable Pratt Parser

P3 is based on Pratt parser, but it is extended to parse almost any kind of expressions.
Since no distinction is made between built-in and user-defined operators, P3 is, in that sense, “programmable”.

# Examples

See the examples in the `test` directory. [Example2](https://github.com/ksrky/p3/blob/master/test/P3/Example2.hs) is currently the most expressive example.

## Using repl

You can try the examples interactively using `cabal repl`.

```command
$ cabal repl p3-test
$ :m + *P3.Example2
$ parseInput "let double = \x -> x * 2 in double 5"
```
