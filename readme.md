
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

```c
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


```c
        a = 1;
        b = 2;
        c = 3;
        d = 4;

        val = fn(a,b,c,d) d-c-b-a; # we have access to outter scope, that is global
        val(a,b,c,d, 2, 4 ,4) # we allow to pass more

```

```c
    cos = fn(a) a!;  # postfix '!' means cossine
    sin = fn(a) !a;  # prefix '!' means sine
    tan = fn(a) sin(a)/cos(a); # we have access to functions in outter scope
    val = tan(98);    
    val 
```


