
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

```py
    epsilon = 0.001; # Close by this much

    abs = fn(a) a lt 0 or a lt -0 ? -a : a;

    float_close = fn(a, b) 
        abs(a - b) lt epsilon ? 
            1 : 0;

    # Classical fibonacci
    fib = fn(n)  
        float_close(n, 0) ? 
            0
        :float_close(n, 1) ?
            1
        :fib(n-1)  + fib(n-2);

    fib(10) # last statement is the return expression from main program
```


```c
    fact = fn(a) a eq 1 ? 1 : a*fact(a-1);
    fact(10)
```


```py
        a = 1;
        b = 2;
        c = 3;
        d = 4;

        val = fn(a,b,c,d) d-c-b-a; # we have access to outter scope, that is global
        val(a,b,c,d, 2, 4 ,4) # we allow to pass more

```

```py
    cos = fn(a) a!;  # postfix '!' means cossine
    sin = fn(a) !a;  # prefix '!' means sine
    tan = fn(a) sin(a)/cos(a); # we have access to functions in outter scope
    val = tan(98);    
    val 
```


# Resources I haven't listed on the comments

Great `C` compiler codebase for learning, it uses a recursive descent pratt parser. Code is simply written and fill with comments:

- https://github.com/rui314/chibicc/blob/main/parse.c


Great Jonathan Blow talk about how simple it is to make a parser that understand precedence:

- https://youtu.be/fIPO4G42wYE?si=u0dneDYUvYSE-Rz1

Eli precedence parsing:

- https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing/

Hydrogren

- https://www.youtube.com/watch?v=pupdmHjzrTI&list=PLUDlas_Zy_qC7c5tCgTMYq2idyyT241qs&index=2
- https://github.com/orosmatthew/hydrogen-cpp



# Less related but useful for compiler enthusiasts

Paper ``An Incremental Approach to Compiler Construction``, it parser and compile `Scheme` like language

- http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf


Multiple Return value implementation 

- https://guenchi.github.io/Scheme/doc/An%20Efficient%20Implementation%20of%20Multiple%20Return%20Values%20in%20Scheme.pdf

Tree Shaking (dead code elimination)

- https://webpack.js.org/guides/tree-shaking/

Complete spec draft for c++, thousand plus pages, kekw:

- https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2011/n3242.pdf

