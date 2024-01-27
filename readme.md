
# Pratt Parsing (precedence climbing) for toy language

The file `parser.odin` that implements this parser, is full of commentaries how how it's done and why it works.

I try to make it as through as possible, everything that was necessary for me to understand and explain to my self is there for future reference. 

I intend to simply copy this repo into any language i try to write a parser for.

The file `test.odin`, shows all the tests for the language, including correct parenthesizing (precedence), and correct interpretation.


# Quick Startup
`odin run src > log.txt`

Now open ``log.txt``, it should pretty print the abstract syntax tree (AST) and tokens for all tests, and the results

You can infer precedence from the tests in `test.odin`, and change some inputs. Or you can look at the produced AST. Or look at the `precedence()` function in `parser.odin` where it's defined all precedence for all operators including postfix, prefix, infix and mixfix types. Same goes for `associativity()` function.

Once I'm in the mood I'll put the precedence table in markdown right here. For now, that's all folks :P, it has been fun.


