package pratt
/*
 . This file implementes the Pratt Parser, which is a recursive descent parser 
 . that parses with correct abritrary operator precedence for anyfix operators (prefix, infix, postfix, mixfix). 
 . Awesome for handwritten parsers, and efficient because the amouth of precedence levels has no bearing in the amount calling depth
 . Think about your usual textbook solutions for ambigous grammar and precedence
 .     exp() -> exp_or() -> exp_and() -> exp_equality()
 .     -> exp_relational() -> exp_relational() -> exp_additive()
 .     -> exp_multiplicative() -> exp_unary -> exp_ternary()
 .     -> exp_atomic()
 . That is a real not ambigous gramar that I wrote for a toy language called elipses, if I ware to make a a recursive parser for that
 . I would have to make 11 calls just to parser a atomic such as the number 2.
 . Using pratt parsing, that goes away, we only need 3 functions and the precedence table.
 .
 .
 . The code is structured to be as simple and understandable as possible.
 . I'll come back to this to add more comments explaining something if I feel it's necessary
 . Resources used such as article, papers and code is going to be listed in the readme description
 .
 . We're gonna parse the following Context Free Grammar, single quotes and uppercase means a Token
 . lower case means a nonterminal variable
 . 
 .   start = assign* expr ;
 .
 .   expr = prefix expr 
 .          | expr postfix  
 .          | expr infix expr 
 .          | expr '?' expr ':' expr
 .          | call
 .          | NUMBER
 .          | IDENTIFIER
 .          ;
 .
 .   assign = IDENTIFIER '=' expr ';'
 .          | IDENTIFIER '=' 'fn' (' params ')' expr ';'
 .          ; 
 .
 .   call = expr '(' args ')';
 .   args = expr
 .          | expr ',' args
 .          | Ɛ 
 .          ;
 .   params = IDENTIFIER
 .          | IDENTIFIER ',' params
 .          | Ɛ 
 .          ;
 .
 .   postfix = '+' | '-' | '~' | '!';
 .   prefix  = '+' | '-' | '~' | '!';
 .   infix   = '+' | '-' | '*' | '/' | '^'
               | 'eq' | 'lt' | 'gt' | 'or' | 'and'
               ;
 .
 . This grammar has all sorts of operator,  after we build the parser, we're gonna
 . walk the tree produced and parenthesize all the expression produced to make
 . sure it follows the expected precedence
 . 
 . Associativy and Precedence of these operators are found in the
 . functions  `associativity` `precedence`, as opposed to a table
 . 
 .
*/


KEYWORDS := map[string]Token{
    "fn"  = Token{keyword = "fn",  kind='k'},
    "eq"  = Token{keyword = "eq",  kind='k'}, // Equality test a eq 2 iff a == 2
    "lt"  = Token{keyword = "lt",  kind='k'}, // Less than
    "gt"  = Token{keyword = "gt",  kind='k'}, // Greater than
    "or"  = Token{keyword = "or",  kind='k'}, // Greater than
    "and" = Token{keyword = "and", kind='k'}, // Greater than
}

Token:: struct {

    using _: struct #raw_union {
        /* 
         .  Only one of this is possible at a time:
         .      number: stores a i64 integer for a Expr_Number 
         .      identifier: stores the string for an identifier Expr_Identifier
        */
        number: union{i64,f64},
        identifier: string,
        keyword: string,
    },
    
    /*
     .  line and colum in the source string,
     .  we only store  the end line and col position for simplicity
     */
    line, col: i64,

    /* 
     .   kind:
     .       'n'  number 
     .       'i'  identifier
     .       'k'  keyword  'fn' for defining function, 'eq' for equality test
     .       ';'  end of statement
     . 
     .       '*'  mul infix, left associative
     .       '/'  div infix, left associative 
     .       '^'  power infix, right associative
     .       '+'  sum infix and identity as prefix, left associative
     .       '-'  sub infix nd negation as prefix
     .       '?'  conditional, mixfix right associative
     .                ex: `1?2:3?4:5 == (1?2:(3?4:5))`
     .                ex: `1 ? 2 : 3 == 2;  0 ? 2 : 3  == 3`
     .                more information: https://stackoverflow.com/questions/36319740/ternary-operator-associativity
     .       '!'  prefix operator !a == sin(a), but also as postfix a! == cos(a)
     .       '~'  prefix operator ~a == a-1, but also as postfix a~ == a+1 
     .       '='  assignment 
     .       '('  prefix for grouping expression like a * (b + c). Also '(' is infix for function calls like a(b).
     .       ')'  closing parenthesis
    */
    kind : rune,
}

/* "Pretty" print tokens */
print_tokens::proc (tokens: []Token) {
    using fmt
    {
        print('[')
        for t, idx in tokens {

            print("'", sep="")
            if t.kind == 'i' {
                print(t.identifier)
            } else if t.kind == 'n' {
                print(t.number)
            } else {
                print(t.kind)
            }
            print("'", sep="")

            if idx == len(tokens)-1 {
                break
            }

            print(", ", sep="")
        }
        println(']')
    }
}

/* 
 . associativity:
 .    Questions arrrived if you think about
 .    associativity of prefix operators.
 .
 .    The expression ---a as right associative is -(-(-a)) which make sense
 .    for conventions reasons. but what if we wanned to be be left associative,
 .    that would mean ---a would be ((--)-)a, resolving '--' first could result in '+',
 .    that doesnt affect the result, but other operators might have different rules. 
 .    
 .    What I question is if that somehow introduces any ambiguity to semantics or syntax
 .    and if is possible to parse that with no confusion.
 . 
 .    2024-01-25 Everton Jr
 . 
*/
associativity :: proc(token: Token, is_prefix: bool = false) -> i64 {
    LEFT, RIGHT  :: 0 , -1
    assoc : i64 = LEFT  
    switch token.kind {
        case '^', '?': assoc = RIGHT
    }
    return assoc
}

/* precedence: precedence number, higher is done first */
PRECEDENCE_LOWEST :: -1
precedence :: proc(token: Token, is_prefix: bool = false) -> i64 {
    prec : i64 = PRECEDENCE_LOWEST
    if is_prefix {
        switch token.kind {
            case '+': prec = 25
            case '-': prec = 25
            case '(': ;;
            case ':': ;;
            case '*': prec = 100
            case '~': prec = 200
            case '!': prec = 300
        }
    }
    else {
        switch token.kind {
            case '(': prec = 500 // func_call
            case 'k':
                switch token.keyword {
                    /* 
                     . We want this a `a gt 1 or b lt 2 == c gt 3 and d lt 4`
                     . to be parsed like this `(((a gt 1) or (b lt 2)) == ((c gt 3) and (d lt 4)))`
                     */
                    case "eq":
                        prec = 2;
                    case "or":
                        prec = 3
                    case  "and":
                        prec = 4
                    case "lt", "gt":
                        prec = 5
                }
            case '+': prec = 10  // infix
            case '-': prec = 10  // infix
            case '*': prec = 20  // infix
            case '/': prec = 20  // infix
            case '^': prec = 30  // infix

            /*
             . if '?' has priority 50 we would have 1 + 2 ? 3 : 4 == 1 + (2 ? 3 : 4)
             . but we want to first resolve binary operators then use that as conditional
             . like this 1 + 2 ? 3 : 4 == (1 + 2) ? 3 : 4
             . to achieve that '?' must have lower prioritty than all binary operators
            */
            case '?': prec = 1  // mixfix

            case '~': prec = 50  // postfix
            case '!': prec = 400 // postfix
        }
    }


    /*
     . The code below is redundant, as long as the start parse_expr precedence 
     . is never lower than de default prec, for example token end 
     . will never pass the check that prec(TOKEN_END) > prec(literally any other token)

    if token.kind == TOKEN_END_KIND  {
        prec = -9000000000
    }
     .
    */

    return prec
}

/* lex: generate all tokens at once, you probably would not wanna do this in a real compiler */
lex :: proc(input :[]u8) -> []Token {
    tokens: [dynamic]Token
    c : u8
    cursor: i64 = 0
    line, col: i64 = 1, 1 

    for true {
        token: Token
        col += 1
        col =+ 1
        if cursor >= auto_cast len(input) {
            return tokens[:]
        }

        c = input[cursor]
        switch c {
        case '#': // comments
            for c != '\n' {
                cursor += 1
                if int(cursor) >=  len(input) {
                    break
                }
                c = input[cursor]
                col += 1
            }
        case '\n', '\r', ' ', '\t', '\v': // whitspace
            cursor += 1
            col += 1
            if c == '\n' {
                /* New line found, reset column and increase line count */
                line += 1 
                col = 1
            }
        case '^', '*', '/', '+', '-', '?', '=', '~', '(', ')','!', ',', ';', ':':  // single char tokens
            cursor += 1
            token.kind = auto_cast c
            token.col = col
            token.line = line
            append(&tokens, token)

        case '0'..='9': // number
            start := cursor
            found_dot := false
            for c >= '0' &&  c <= '9' {
                cursor += 1
                col += 1
                if int(cursor) >=  len(input) {
                    break
                }
                c = input[cursor]
                if c == '.'  {
                    if !found_dot {

                        found_dot = true
                        cursor += 1
                        col += 1
                        if int(cursor) >=  len(input) {
                            break
                        }
                        c = input[cursor]
                    } else {
                        error(Token{line=line, col=col}, "Multiple dots '.' in fractional number")

                    }
                }
            }


            str_num := string(input[start:cursor])
            num: union{i64, f64} = ---
            ok := false
            if found_dot {
                num, ok = str.parse_f64(str_num)
            }
            else {
                num, ok = str.parse_i64(str_num)
            }

            if !ok {
                panic(fmt.tprint("Conversion of", str_num, "to i64 failed") )
            }

            token.number = num
            token.kind = 'n'
            token.col = col
            token.line = line
            append(&tokens, token)

        case:
            if !(c == '_') && !is_letter(c) do error(Token{line=line, col=col}, fmt.tprintf("Not allowed character `%v`", rune(c)) )
            start := cursor
            /* identifiers cant contain numbers */
            for c == '_' || is_letter(c)  {
                cursor += 1
                if int(cursor) >=  len(input) {
                    break
                }
                c = input[cursor]
                if(is_digit(c)) {
                    error(Token{line=line, col=col}, fmt.tprintf("Not allowed character number in as identifier `%v` ", rune(c)) )
                }
            }
            token.identifier = string(input[start:cursor])
            /* Check if it is keyword by hashmap */
            if token.identifier in KEYWORDS {
                token = KEYWORDS[token.identifier]
            } else {
                token.kind = 'i'
            }
            token.col = col
            token.line = line
            append(&tokens, token)

        }
    }
    return  tokens[:]

}

/* Ast Nodes */
Ast :: struct {
    kind: typeid
}

Start :: struct {
    using _:     Ast,
    assignments: []^Assign,
    expr:        ^Expr
}

Assign :: struct {
    using _:    Ast,
    identifier: Token,
    assign:     Token,
    expr:      ^Expr,
    semicolon: Token,
} 

Assign_Function :: struct {
    using _:   Assign,
    params :   []Token, // all must be identifiers
} 

Assign_Identifier :: struct {
    using _: Assign
} 

Expr :: struct {
    using _: Ast
} 

Expr_Identifier :: struct {
    using _: Expr,
    identifier:  Token,
} 

Expr_Number :: struct {
    using _: Expr,
    number:  Token,
} 

Expr_Grouped :: struct {
    using _: Expr,
    open:    Token,
    expr:    ^Expr,
    close:   Token,
} 

Expr_Prefix :: struct {
    using _: Expr,
    op:      Token,
    right:   ^Expr,
} 

Expr_Infix :: struct {
    using _: Expr,
    left:    ^Expr,
    op:      Token,
    right:   ^Expr,
} 

Expr_Postfix :: struct {
    using _: Expr,
    left:    ^Expr,
    op:      Token,
}

Expr_Mixfix :: struct {
    using _: Expr,
    left:    ^Expr,
    op1:     Token,
    mid:     ^Expr,
    op2:     Token,
    right:   ^Expr,
}

Expr_Function_Call :: struct {
    using _: Expr,
    left:    ^Expr,
    open:    Token,
    exprs:   []^Expr,
    close:   Token,
}


new :: proc($kind: typeid) -> ^kind {
    node := builtin.new(kind)
    node.kind = kind
    return node
}

error :: proc(previous_or_current:Token, msg:string) {
    curr := previous_or_current
    msg := fmt.tprintf("(%v:%v) Error: %v.", curr.line, curr.col, msg)
    fmt.eprintln(msg)
    os.exit(1)
}

expect :: proc (expects:[]rune, msg:="") {
    using fmt

    curr := peek(0)

    /* 
     * Inform User if curr token is not in the expected tokens.
     * Panic if necessary, displaying line and column
    */
    ok: if expects != nil {
        any_found := false
        for e in expects {
            if e == curr.kind  {
                break ok;
            }
        }

        got: string
        if curr.kind == 'k' {
            got  = tprintf("keyword `%v`", curr.keyword)
        } else if curr.kind == 'i' {
            got  = tprintf("identifier `%v`", curr.identifier)
        } else if curr.kind == 'n' {
            got  = tprintf("number `%v`", curr.number)
        } else if curr.kind == 'e' {
            got  = tprintf("end of file")
            curr.line = peek(-1).line 
            curr.col = peek(-1).col 
        } else {
            got  = tprintf("symbol `%v`", curr.kind)
        }

        expect_msg: string
        if len(expects) == 1  {
            expect_msg = tprint('`', expects[0],'`', sep="")
        } else {
            expect_msg = tprint("one of the following: ")
            for e, idx in expects {
                expect_msg  = tprint(expect_msg, '`', e,'`', sep="") 
                if idx < len(expects)-1 {
                    expect_msg  = tprint(expect_msg, ',',sep="") 
                }
            }
        }

        msg := fmt.tprintf("%v Expected %v, but got %v instead", msg, expect_msg, got)
        error(curr, msg )
    }
}

next :: proc(expects:[]rune = nil) -> Token {
    curr := peek(0)
    expect(expects)
    token_cursor += 1
    return curr
}

TOKEN_END_KIND := rune('e') /* e in 'E'ND */
TOKEN_END := Token{kind=TOKEN_END_KIND}

peek :: proc(offset:=0) -> Token {
    if token_cursor+offset >= len(tokens)  || token_cursor+offset < 0 {
        return TOKEN_END
    }
    return tokens[token_cursor+offset]
}


/*
 .
 .	 Null Denotation (meaning it requires nothing to be parsed) or Prefix Expression
 . 
 .	 We DO NOT get the prefix parsing functions by mapping the current token
 .	 to a function pointer handler as other pratt parsers do. We much prefer alway having one function
 .   that parses a indivisable expression always and switch by token in one place
 .	 that make the parsing more understandable and probably faster as I commented on the `parse_left_denotations`
 .	
*/
parse_null_denotations :: proc () -> ^Expr {
    token := peek()

    expr : ^Expr = nil
    prec := precedence(token, is_prefix=true)
    assoc := associativity(token)

    switch token.kind {
    case 'i':
        ast := new(Expr_Identifier)
        ast.identifier = next({'i'})
        expr = ast

    case 'n':
        ast := new(Expr_Number)
        ast.number = next({'n'})
        expr = ast

    case '~', '!', '-', '+':
        ast := new(Expr_Prefix)
        ast.op = next({'~', '!', '-', '+'})
        ast.right = parse_expr(prec)
        expr = ast

    case '(':
        ast := new(Expr_Grouped)
        /* Don't need to expect anything because, we already checked on this `case '(':` */
        ast.open  = next() 
        /* But, now we do */
        ast.expr  = parse_expr(prec)
        ast.close = next({')'})
        expr = ast

    case TOKEN_END_KIND:
        error(peek(-1), fmt.tprint("Expected an expression, but got end of file"))
    case:
        error(peek(-1), fmt.tprint("Expected an expression, but got token that can't make a expresion", token))
    }

    assert (expr != nil, fmt.tprint("Could not switch on the token. Got", token))
    return expr
}


/* 
 .  Left Denotation (meaning it receives a `left` to be parsed)
 . 
 .  This one is indirectly recursive (parse_expr call this and this calls parse_expr).
 .  Because of that it produces a Right Leaning Tree
 .
 .  Right associative needs a right leaning tree, that means last (from left to right) must be resolved first 
 . 
 .  Left associative operators need left leaning tree 
 . 
 .  Again, we do prefer switching on the current operator token in one function, instead of doing
 .  a map from a table. This way it's all in one place. It's also faster than a map, because
 .  it's a switch (I think thats faster than a hash to retrieve a function pointer? it must be)
 .  and because MOST tokens will end it up have the same infix handler, such as all binary operators 
 . 
*/
parse_left_denotations :: proc (left: ^Expr) -> ^Expr {
    token := peek()
    prec  := precedence(token, is_prefix=false)
    assoc := associativity(token)
    expr : ^Expr = nil
    switch token.kind {
    case '+', '-', '*', '/', '^', 'k': /* infix */
        ast := new(Expr_Infix)
        ast.left  = left
        ast.op    = next({'+', '-', '*', '/', '^', 'k'})
        ast.right = parse_expr(prec)
        expr = ast

    case '!', '~': /* postfix, much like prefix, just wraps the `left` */
        ast := new(Expr_Postfix)
        ast.left  = left
        ast.op    = next({'!', '~'})
        expr = ast

    case '?': /* mixfix */
        ast := new(Expr_Mixfix)
        ast.left  = left
        ast.op1   = next({'?'})
        /*
         . We pass lowest precedence to start all over, goes into the expression `blackhole`
         . oblivious to the fact that it is inside another expression,
        */
        ast.mid   = parse_expr(PRECEDENCE_LOWEST) 
        prec2    := precedence(peek())
        /*
         . in the grammar, if it is right recursive is it right associative
         . if is left recursive than it is left associative
        */
        ast.op2   = next({':'})
        ast.right = parse_expr(prec2)
        expr = ast

    case '(': /* func call */
        ast := new(Expr_Function_Call)
        ast.left   = left
        ast.open   = next()

        exprs := [dynamic]^Expr{}
        for peek().kind != ')' {
        /*
         . We pass lowest precedence to start all over, a new expression that doesnt know
         . it's inside a function call
        */
            append(&exprs, parse_expr(PRECEDENCE_LOWEST))
            if peek().kind != ',' {
                break
            }
            comma := next({','})
        }

        ast.exprs  = exprs[:]
        ast.close   = next({')'})
        expr = ast
    
    }

    assert (expr != nil)
    return expr
}




/*
 .  If next (right) operator in the token stream is higher precedence, if most be resolved first
 .  despite being on the right (after), that means a right leaning sub-tree 
 .  Ex: 1+2*3 , + is on the left, comes first, * is on the right, after
 .  despite being on the right we must create a right leaning that way it gets resolved first

     +
    / \
   1   *
      / \
     2   3
 . To crete a right leaning tree if must give the `left` operand  found by + to the * 
 . we can achieve that with loop
 .
 . Writting the grammar as
 .  expr = expr ('+' term)*;
 .  instead of
 .  expr = term | term '+' expr;
 . the first notation is akin to loop the second one is akin to rescursive call
 . the first notation creates a left leaning tree, the second a right
 . 
 . Now if the previous token has higher precende than the next, that means a left leaning tree
 . we would use recursion instead of while loop
 . 
 . Turns out that's all you need consider that a operator token owns the expressions:
 . Then: Give the "your" `left` to the the next token if next token has higher precende than "yourself", continue in the loop
 . if not than proceed to recurse 
 . 

         expression(prec_earlier:=0):
            token := next()
            left  := token.prefix()
            while prec_earlier < token.prec: 
                token = next()
                left = token.infix(left)
            return left
 .
 .
 .  `token.infix` means a fuction that parses an expression associated
 .  with that token and takes a `left` expr as input (infix, postfix, mixfix)
 .  because of that we will now call a `left_denotation`, following Pratt's terminology,
 .  since it takes a `left`.
 .
 . 
 . 
 .  `token.prefix` means a fuction that parses an expression associated
 .   with that token and takes a nothing expr as input. Because of that 
 .   we'll call it `null_denotation`. Idk what denotation means, I would have to take
 .   another look at Pratt's paper, but at least it makes more sense to me because `left` takes a left
 .   `null` takes nothing
 . 
 . 
 . 
 .   Inside the loop there is `token = next()` because we need to update the 
 .   update the token, because we need to check the next token after "us".
 .   `prec_earlier` is "our" precedence, that will continue the same after 
 .   every loop (invariant), but the next token precedence might NOT. That is the case 
 .   because `token.infix` might a cosume a operador token 
 .   `left = token.infix(left)` we are also reassigning back to `left`, to keep 
 .   accumulating the returned infix expression on the top of the tree, ensuring it gets
 .   resolved later, since "our" precedence is lower tha next
 . 
 . 
 . 
 . You may also take a look at Wren Compiler which associatates token with function pointers
 . to infix and prefix operations, much in the vein of the original pratt parser paper.
 .     https://github.com/wren-lang/wren/blob/main/src/vm/wren_compiler.c
 . 
 . Other code/article examples of pratt parsing (or precedence climbing parsing, to me seems like the same algorithm, no cap):
 .     https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
 .     https://github.com/munificent/magpie/blob/master/src/com/stuffwithstuff/magpie/parser/MagpieParser.java
 .     https://github.com/andychu/pratt-parsing-demo/blob/master/tests.py
 . 
 . This one is a parser for bantam language that has various precedences, we "borrow" tests from there
 .     https://github.com/KubaO/pybantam
 . 
 . Why does it work? See the deriving in: 
 .     https://www.engr.mun.ca/%7Etheo/Misc/exp_parsing.htm#grammar
 . and in Clark's paper:
 .     https://www.antlr.org/papers/Clarke-expr-parsing-1986.pdf
 . 
*/

parse_expr :: proc(prec_prev: i64) -> ^Expr {
    /* expressions that takes nothing (null) as left operand */
    left := parse_null_denotations() 
    /*
    . if current token is left associative or current token has higher precedence
    . than previous precedence then stay in the loop, effectively creating a left leaning
    . sub-tree, else, we recurse to create a right leaning sub-tree.
    */
    for precedence(peek()) > prec_prev + associativity(peek())  {
        /* expressions that needs a left operand such as postfix, mixfix, and infix operator */
        left = parse_left_denotations(left)
    }
    return left
}


/*
 . Lets run a small example by hand: 1+2*3^4^5-6
 . We have a sequence of operators as such:
 . '+' 1ºop  < '*' 2ºop
 . '*' 2ºop  < '^' 3ºop
 . '^' 3ºop == '^' 4ºop ( but is if 3ºop < 4ºop because of right associativity of '^'),
 . '^' 4ºop  > '-' 5ºop
 . `e()`      is parse_expr(); * and e is the expr to me parsed
 . `prec_op`  is not changing precedence, the one passed as input to the function
 . `prec(op)` is calculating the precedence again, op is next token taken from current strean/list
 . `ret`      is return

stream = 1+2*3^4^5-6
left = 1
p = -1; p'= prec(+)
p' > p ->
left = e()
  +
 / \
1   e
stream = 2*3^4^5-6
-> 

    left = 2
    p = prec_+; p'= prec(*)
    p' > p ->
      *
     / \
    2   e
    stream = 3^4^5-6
    -> 

        left = 3
        p = prec_*; p'= prec(^)
        p' > p ->
          ^
         / \
        3   e
        stream = 4^5-6
        -> 

            left = 4
            p = prec_^; p'= prec(^)
            p' > p // because of associativity ->
              ^
             / \
            4   e
            stream = 4^5-6
            ->

                left = 5
                p = prec_^; p'= prec(-)
                p' < p  <-
                ret 5
                stream = -6
                <-

            left = ^
                  / \
                 4   5
            p = prec_^; p'= prec(-)
            p' < p <-
            ret ^
               / \
              4   5
            stream = -6
            <-

        left = ^
              / \
             3   ^
                / \
               4   5
        p = prec_*; p'= prec(-)
        p' < p <-
        ret ^
           / \
          3   ^
             / \
            4   5
        stream = -6
        <- 

    left = *
          / \
         2   ^
            / \
           3   ^
              / \
             4   5
    p = prec_+; p'= prec(-)
    p' == p; <- // but left associate so we don't recurse 
    ret    *
          / \
         2   ^
            / \
           3   ^
              / \
             4   5
    stream = -6
    <-  

left = +
      / \
     1   *
        / \
       2   ^
          / \
         3   ^
            / \
           4   5

p = -1; p'= prec(-)
p' > p ->
    -
   / \
  +   e
 / \
1   *
   / \
  2   ^
     / \
    3   ^
       / \
      4   5
stream = 6
-> 

    left = 6
    p = prec_+; p'= prec(n)
    p' < p <- // because prececende of numbers tokens are lowest, so always loose
    ret 6
    stream =
    <- 

left =  -
       / \
      +   6
     / \
    1   *
       / \
      2   ^
         / \
        3   ^
           / \
          4   5

p = -1; p'= prec(token_end)
p' == p <- //but we needed to be bigger than so we return 
stream = 
ret     -
       / \
      +   6
     / \
    1   *
       / \
      2   ^
         / \
        3   ^
           / \
          4   5
 .
 . That's our final tree, repecting both associativity and precedence
 .
*/

parse_params :: proc() -> []Token {
    next({'('})
    params := [dynamic]Token{}
    for peek().kind == 'i' {
        append(&params, next({'i'}))
        if peek().kind != ')' {
            next({','})
        }
    }
    if peek().kind != ')' {
        /*
         . Extra checking for cases where the user put something
         . other than identifiers in the parameter list. Ex: call = fn(id1, id2, 98) 1+2;
        */
        expect({'i'}, msg="Function paremeters an only be identifiers")
    }
    next({')'})
    return params[:]
}

parse_assign :: proc() -> ^Assign {
    ast : ^Assign;
    identifier := next()
    assign     := next()

    // Be sure it's a function declaration
    if peek().kind == 'k' && peek().keyword == "fn" {
        func := new(Assign_Function)
        next({'k'})
        func.params = parse_params()
        ast = func
    } else { // if not then it's just simple assignment
        ast = new(Assign)
    } 

    ast.identifier = identifier
    ast.assign     = assign
    ast.expr = parse_expr(PRECEDENCE_LOWEST)
    ast.semicolon = next({';'})
    return ast
}

parse_start :: proc() -> ^Start{
    ast := new(Start)
    assignments := [dynamic]^Assign{}
    for peek(0).kind == 'i' && peek(1).kind == '=' {
        assign := parse_assign()
        append(&assignments, assign)
    }
    ast.assignments = assignments[:]
    ast.expr = parse_expr(PRECEDENCE_LOWEST)
    return ast
}


tokens: []Token = nil;
token_cursor := 0
parse :: proc () -> ^Ast {
   return parse_start()
}

reset::proc () {
    tokens = nil
    token_cursor = 0
    scope_reset()
    scope_enter()
}

is_letter :: proc(c:u8) -> bool { return unicode.is_letter(rune(c));}
is_digit  :: proc(c:u8) -> bool { return unicode.is_digit(rune(c)); }

import "core:builtin"
import "core:math"
import "core:unicode"
import "core:os"
import str "core:strconv"
import fmt "core:fmt"
