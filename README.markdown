# parser-combinators

This is an experimental implementation of [parser combinators](http://en.wikipedia.org/wiki/Parser_combinators) for Common Lisp. This is mostly inspired by Parsec, but with limited optimizations. Parser combinators are, in theory, an elegant form of constructing parsers for context-free grammars.

The name of the system is `parser-combinators`, despite the name of the GitHub repository. The library is fairly lightly tested. Any comments or questions on the functionality or performance are welcome.

# Dependencies

- iterate
- alexandria

The test suite has some additional dependencies, all of which are in [quicklisp](http://www.quicklisp.org/)

# Documentation

There is some documentation in the `doc` directory.
