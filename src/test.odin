package pratt

/*-------------------------------------------------------------------------------------------------*
 *---------------------------------------- Tests --------------------------------------------------*
 *-------------------------------------------------------------------------------------------------*
*/

tests_passed := 0
tests_total  := 0 
tests_failed := [dynamic]string{}

test :: proc{test_interp, test_paren}

test_all :: proc() {
    using fmt

    // Recursion
    for num in 1..<10 {
        fib :: proc (n :f64) -> f64 {
            if n <= 1 do return n;
            return fib(n-1) + fib (n-2)
        }

        test(tprintf(`
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

            fib(%v)
        `, num),
            fib(f64(num))
        )
    }

    for num in 1..<21 {
        test(tprintf(`
            fact = fn(a) a eq 1 ? 1 : a*fact(a-1);
            fact(%v) `, num),
            f64(math.factorial(auto_cast (num)))
        )
    }

    // Assignments and interpretation
    test(`
        a = 1;
        b = 2;
        c = 3;
        d = 4;

        val = fn(a,b,c,d) d-c-b-a; # we have access to outter scope, that is global
        val(a,b,c,d, 2, 4 ,4) # we allow to pass more
    `, 4-3-2-1)

    // Simple Math 
    test(`
        cos = fn(a) a!;  # postfix '!' means cossine
        sin = fn(a) !a;  # prefix '!' means sine
        tan = fn(a) sin(a)/cos(a); # we have access to functions in outter scope

        val = tan(98);    
        val 
    `, math.tan(f64(98)))


    // Relational operator 
    /* `and` has priority over `or` */
    test("1 or 2 and 3 or 4", "((1 or (2 and 3)) or 4)")
    /* `lt, gt` has priority over `eq` */
    test("1 gt 2  eq 1 lt 2", "((1 gt 2) eq (1 lt 2))")

    /* `lt, gt` has priority over `and, or` */
    test("1 gt 2  or 1 lt 2 and 2", "((1 gt 2) or ((1 lt 2) and 2))")

    /* `lt, gt` has priority over `and, or` which they have more prio then `eq` */
    test("1 gt 2 or 2 lt 1  eq 3 lt 4 and 4 gt 2 ", "(((1 gt 2) or (2 lt 1)) eq ((3 lt 4) and (4 gt 2)))" )

    /* binary operators has bigger prio than `lt, gt` */
    test("1 + 2 gt 2 * 3 ", "((1 + 2) gt (2 * 3))")

    /*  Ternary `?` has even lower prio than `eq`*/
    test("1 eq 2 ? 3: 4", "((1 eq 2) ? 3 : 4)")

    // Function call.
    test("a()", "a()")
    test("a(b)", "a(b)")
    test("a(b, c)", "a(b, c)")
    test("a(b)(c)", "a(b)(c)")
    test("a(b) + c(d)", "(a(b) + c(d))")

    // Ternary
    test("b ? c : d", "(b ? c : d)")
    test("b ? c : d ? e : f", "(b ? c : (d ? e : f))")
    test("b ? c : d ? e : f ? g : h", "(b ? c : (d ? e : (f ? g : h)))")
    test("a(b ? c : d, e + f)", "a((b ? c : d), (e + f))")
    //
    //
    // Unary precedence.
    test("~!-+a", "(~(!(-(+a))))")
    test("a!!!", "(((a!)!)!)")

    // Unary and binary predecence.
    test("!a + b", "((!a) + b)")
    test("-a * b", "((-a) * b)")
    test("~a ^ b", "((~a) ^ b)")
    test("-a!", "(-(a!))")
    test("!a!", "(!(a!))")

    // Binary precedence.
    test("a - b + c * d ^ e - f / g", "(((a - b) + (c * (d ^ e))) - (f / g))")
    // test("a = b + c * d ^ e - f / g", "(a = ((b + (c * (d ^ e))) - (f / g)))")

    /* first fo the power '^' operator then do the  unary '-' as in math */
    test("-1^2", "(-(1 ^ 2))") 

    // Binary associativity.
    // test("a = b = c", "(a = (b = c))")
    test("a + b - c", "((a + b) - c)")
    test("a * b / c", "((a * b) / c)")
    test("a ^ b ^ c", "(a ^ (b ^ c))")

    // Conditional operator.
    test("a ? b : c ? d : e", "(a ? b : (c ? d : e))")
    test("a ? b ? c : d : e", "(a ? (b ? c : d) : e)")
    test("a + b ? c * d : e / f", "((a + b) ? (c * d) : (e / f))")

    // Grouping.
    test("a + (b + c) + d", "((a + (b + c)) + d)")
    test("a ^ (b + c)", "(a ^ (b + c))")
    test("(!a)!", "((!a)!)")
}

test_paren :: proc(input, expected: string, should_error:=false) -> ^Ast{
    reset()
    tests_total += 1 

    src := transmute([]u8)input

    tokens = lex(src)
    fmt.println()
    print_tokens(tokens)

    ast := parse()
    walker_print(ast)
    output := walker_paren(ast)
    if output == expected {
        tests_passed += 1
    } else {
        if should_error { 
            tests_passed += 1
        } else {
            add_test_error(input, expected, output)
        }
    }
    return ast
}

test_interp :: proc(input: string, expected: f64) -> ^Ast{
    reset()
    tests_total += 1 

    src := transmute([]u8)input
    tokens = lex(src)
    print_tokens(tokens)

    ast := parse()
    walker_print(ast)

    output := walker_interp(ast)

    if  output == expected {
        tests_passed += 1
    } else {
        add_test_error(input, fmt.tprint(expected), fmt.tprint(output))
    }
    return ast
}



add_test_error :: proc(input, expected, output: string) {
    append(&tests_failed, fmt.tprintf("input:\n%v\nexpected:\n%v\ngot:\n%v\n", input, expected, output))
}

result :: proc() {
    using fmt

    total := tests_total
    passed := tests_passed
    failed := total - passed
    // Show the results.
    if passed == total {
        if passed == 1 {
            println("Passed ", passed, "test.")
        } else {
            println("Passed all", passed, "tests.")
        }
    } else {

        println("----")
        println("Failed", failed, "out of", total,  "tests.")
        for s in tests_failed {
            println(s)
        }
        os.exit(69)
    }
}


import "core:math"
import "core:os"
import fmt "core:fmt"

