# parser-combinators

This is an experimental implementation of [parser combinators](http://en.wikipedia.org/wiki/Parser_combinators) for Common Lisp. This is mostly inspired by Parsec, but without any optimizations. Parser combinators are, in theory, an elegant form of constructing parsers for context-free grammars.

It more or less works, but was not tested for any but the simplest cases, some parts of the interface are clunky, it is quite slow in general and probably has a tendency for exponential explosions. That said, it could probably be used to parse small amounts of data, especially if applied hierarchically. Comments welcome.

I have found this more convenient than regular expressions in some cases, especially where maximum performace is not required.

# Dependencies

- iterate
- alexandria

# Usage

Parsers combinators combine parsers. In the specific context of this library a parser is a function from `context` object to a thunk returning `parser-possibility` objects or `nil`, if there are no more possible parses. This parsers can be combined using parser combinators to create more complex parsers.

### contexts

Contexts are immutable objects pointing into some underlying linear data. Their identity is used for memoization, so the objects are cached per position.

There are contexts for lists and vectors. Contexts must implement `context-peek`, `context-next` and `make-context` for appropriate data type. Context length is only used to curtail left recursion, so it can be an upper length bound rather than exact length.

### primitive parsers

This are the building blocks for most parsers. It is also possible to construct a thunk directly for efficiency. All parsers are obtainable only though their generators. Macros `def-cached-parser` and `def-cached-arg-parser` can be used to avoid recreating a parser every time.

`(zero)` is a parser generator for a parser which represents a parsing failure

`(result v)` is a parser generator for a parser which doesn't modify the input and returns `v`

`(item)` is a parser generator for a parser which consumes and returns one item from the input

### automatic parsers

Most places which expect a parser will also accept any non-function object (a function object will be treated as a parser). Sequences are matched against the input element by element with `eql`, and atoms are matched themselves.

### primitive combinators

`bind parser parser-generator` takes a parser, a function of one argument which generates a parser and returns a parser which applies a first component parsers on input, constructs a second parser by giving a return value of the first to the generator function and then applies that second parser to the input, for every possible parses of both parsers. Component parsers are only called as necessary. This is not a very good explanation, but this is more simple that it sounds.

`mdo &rest specs` is a wrapper over bind, loosely based on Haskell `do` syntax sugar. It can be used to compose a series of parsers, optionally binding their results to some names. Those results can be used when constructing all following parsers. The result of parser such constructed is the result of the final parser, which in most cases will be a `result` primitive parser using names established previously. In cases where the parsers do not depends on results of previous parsers (other than position in the input) `named-seq?/*` are more appropriate.

`choice parser1 parser2` returns all results of the first parser and then all results of the second parser. `choices &rest parsers` does the same for any number of parsers.

`choice1 parser1 parser2` returns the first parse of two parsers and then stops. `choices1 &rest parsers` does the same for any number of parsers.

### parser modifiers

`force? parser` makes a parser which is identical to its argument, but is fully executed, that is, does not perform further parsing lazily.

`delayed? parser` creates a parser which will be constructed only when called, once. This is useful only for left-recursive parsers, which otherwhise would cause infinite recursion.

`memoize? parser &optional label` creates a parser which will be memoized using label. Label makes it possible to memoize the same parser from multiple places.

`curtail? parser &optional label` creates a curtailed parser. That is, recursive calls to this parser will fail if recursion depth is greater than remaining input length, since at that point success is no longer possible. This is mostly useful, together with `delayed?` modifier to handle left-recursive grammars. Obviously, this incurs a performance penalty.

### basic parsers

There is a number of predefined parsers in `parsers.lisp` and `greedy.lisp`. They are documented by their docstrings. Non-backtrakcing parsers, indicated by star replacing the question mark in their names, discard all but the first result. This decreases memory consumption since backtracking information is not kept, but may lead to parser failures if there is ambiguity which can only be resolved from higher context.

### top-level interface

`parse-string parser string` will parse a string using parser, and return a `parse-result` object. From that object results can be retrieved using `current-result`, `next-result`, `nth-result`, `gather-results`. Note that for a fresh objects `current-result` and `next-result` are equivalent. `gather-results` will return remaining results. `copy-parse-result` will return a `parse-result` object with reset result list, ie. `next-result` will return first result. Note that due to laziness results will be computed only as needed.

The results are `parser-possibility` objects. Accessor `tree-of` will access the actual result, and `suffix-of` the remaining input context, of type `end-context` if an entire input was parsed. Function `end-context-p` will check if a context is an `end-context`.

Utility function `parse-string* parser string &key (complete nil)` will discard all results after the first, which will be returned as multiple values of: the tree of the result or `nil`, suffix context or `nil` when the input was exhausted, `t` for success and `nil` for failure in case `nil` was a valid result.

With the key argument `complete` value `t` first result which results from total consumption of input will be returned. If it is equal `:first` only the first result will be computed, and then returned if it is complete, otherwise the parse being unsuccessful.

### error detection
Since the parsers are fully backtracking it is difficult to differentiate normal backtracking from parsing failure. There is one available approximation: second return value of `parse-string` and fourth of `parse-string*` will be, if the parse was a failure or incomplete, a `context-front` object.

It has two interesting accessors, `position-of` which indicates a position to which the input has been consumed, and `tags-of` which returns a list of lists of tags, which indicate what parsers were active when the element was consumed, from most specific first in the inner list, with the outer list having one sublist per time the element has been consumed.

Most predefined parsers have no tags. Tags can be added with `tag? parser format-control &rest format-arguments` and `cut-tag? parser format-control &rest format-arguments`. The latter suppresses all tags assigned by the `parser`.

As mentioned above, this is only an approximation, and there is no guarantee that the most advanced input fragment is the source of the parse failure, or that the tags at that point are relevant or even correct.

### example

`test-arithmetic.lisp` contains an obligatory four expression infix arithmetic example. On my system it can parse 100000 nodes long randomly generated string in less than two seconds with rather simple grammar. In `test-expression.lisp` there is an example of a arithmetic parser using generalized expression parser, with precedence and subexpressions.
