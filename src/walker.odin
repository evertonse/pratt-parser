package pratt

walker_interp :: proc(ast: ^Ast) -> f64 {
    using fmt
    walker :: walker_interp
    val : f64 = 0

    switch ast.kind {

        case Start:
            ast := cast(^Start)ast;
            for a in ast.assignments {
                walker(a)
            }
            val = walker(ast.expr)

        case Assign:
            ast := cast(^Assign)ast;
            var_name := ast.identifier.identifier
            scope_add(var_name, Symbol{
                name = ast.identifier.identifier,
                val = walker(ast.expr),
                is_function = false,
            })

        case Assign_Function:
            ast := cast(^Assign_Function)ast;

            /* add this functions id to the current scope */
            fn_name := ast.identifier.identifier
            scope_add(fn_name, Symbol{
                    name = ast.identifier.identifier,
                    val = 0,
                    is_function = true,
                    body = ast.expr,
                    params=ast.params
            })

        case Expr:
            assert(false, "WTFF this is abstract struct gentle bruh")

        case Expr_Function_Call:
            expr := cast(^Expr_Function_Call)ast;
            if expr.left.kind != Expr_Identifier { 
                msg := "Tried to make a function call with compound expression, we only allow simple identifier for look up symbol table simplicity reasons"
                error(expr.open, msg)
            }

            fn_identifier := cast(^Expr_Identifier)expr.left
            fn_symbol, ok := scope_get(fn_identifier.identifier.identifier)
            if !ok {
                error(fn_identifier.identifier, tprintf("Function `%v` does not exist in this scope", fn_identifier.identifier.identifier))
            }

            params_len := len(fn_symbol.params)
            args_len   := len(expr.exprs)
            /* we allow passing more just for kicks */
            if  args_len < params_len  {
                msg := tprintf(
                    "Trying to call `%v` with too few arguments, expected %v got %v",
                    fn_identifier.identifier.identifier, params_len, args_len
                )
                error(fn_identifier.identifier, msg)
            } 

            fn_scope := Scope{}
            for param, idx in fn_symbol.params {
                sym := fn_scope[param.identifier] 
                sym.val = walker(expr.exprs[idx])
                fn_scope[param.identifier]  = sym
            }

            scope_enter(fn_scope)
            val = walker(fn_symbol.body)
            scope_exit()

        case Expr_Number:
            expr := cast(^Expr_Number)ast;
            number := expr.number.number
            switch _ in  number {
                case i64:  val = f64(number.(i64))
                case f64:  val = number.(f64)
            } 

        case Expr_Identifier:
            expr := cast(^Expr_Identifier)ast;
            key := expr.identifier.identifier

            sym, ok := scope_get(key)
            if !ok {
                error(expr.identifier, msg=fmt.tprintf("The identifier `%v` hasn't been defined", expr.identifier.identifier))
            }
            val = sym.val

        case Expr_Prefix:
            expr := cast(^Expr_Prefix)ast;
            val = walker(expr.right)
            if expr.op.kind == '-' {
                val = -val
            } else if expr.op.kind == '!' {
                val = math.sin(val)
            } else if expr.op.kind == '~' {
                val -= 1
            }

        case Expr_Postfix:
            expr := cast(^Expr_Postfix)ast;
            val = walker(expr.left)
            if expr.op.kind == '!' {
                val = math.cos(val)
            } else if expr.op.kind == '~' {
                val += 1
            }
        case Expr_Mixfix:
            expr := cast(^Expr_Mixfix)ast;
            val = walker(expr.left)
            if val != f64(0)  && val != f64(-0) {
                val = walker(expr.mid)
            } else {
                val = walker(expr.right)
            }

        case Expr_Grouped:
            expr := cast(^Expr_Grouped)ast;
            val = walker(expr.expr)

        case Expr_Infix:
            expr := cast(^Expr_Infix)ast;
            left := walker(expr.left)
            right := walker(expr.right)
            if expr.op.kind == '-' {
                val = left - right
            } else if expr.op.kind == '+' {
                val = left + right
            } else if expr.op.kind == '*' {
                val = left * right
            } else if expr.op.kind == '/' {
                val = left / right
            } else if expr.op.kind == '^' {
                val = math.pow(left, right)
            } else if expr.op.kind == 'k'  {
                switch expr.op.keyword {
                    case "eq":
                        if left == right  {
                            val = 1
                        }
                    case "lt":
                        if left < right  {
                            val = 1
                        }
                    case "gt":
                        if left > right  {
                            val = 1
                        }
                    case "or":
                        if left == 1  || right == 1  {
                            val = 1
                        }
                    case "and":
                        if left == 1  && right == 1  {
                            val = 1
                        }
                }
            }

        case:
            assert(false,"walker_interp generic case")
    }

    return val
}

walker_paren :: proc(ast: ^Ast) -> string {
    walker :: walker_paren
    using fmt
    val : string

    switch ast.kind {
        case Start:
            ast := cast(^Start)ast;
            if len(ast.assignments) == 0  {
                return walker(ast.expr)
            }

            for a in ast.assignments {
                val = tprint(val, walker(a), "", sep="")
            }
            val = tprint(val, "ret ", walker(ast.expr), sep="")

        case Assign:
            ast := cast(^Assign)ast;
            val = tprint(ast.identifier.identifier, ' ', ast.assign.kind,' ', walker(ast.expr), ';', sep="")

        case Assign_Function:
            ast := cast(^Assign_Function)ast;
            val = tprint(ast.identifier.identifier, ' ', ast.assign.kind,' ', "fn(" , sep="")
            for params, idx in ast.params {
                if idx < len(ast.params)-1 {
                    val = tprint(val ,params.identifier, ", ", sep="")
                } else {
                    val = tprint(val ,params.identifier, ")", sep="")
                }
            }
            val = tprint(val, " ret ", walker(ast.expr), ";" , sep="")

        case Expr:
            assert(false, "Abstract bro, you get the point ")

        case Expr_Number:
            ast := cast(^Expr_Number)ast;
            val = fmt.tprint(ast.number.number, sep="")

        case Expr_Identifier:
            ast := cast(^Expr_Identifier)ast;
            val = fmt.tprint(ast.identifier.identifier, sep="")

        case Expr_Mixfix:
            ast := cast(^Expr_Mixfix)ast;
            val = fmt.tprint('(', walker(ast.left), ' ', ast.op1.kind, sep="")     // (b ?
            val = fmt.tprint(val, ' ', walker(ast.mid), ' ', ast.op2.kind, sep="") // (b ? c :
            val = fmt.tprint(val, ' ', walker(ast.right) , ')', sep="")             // (b ? c : d)

        case Expr_Prefix:
            ast := cast(^Expr_Prefix)ast;
            val = walker(ast.right)
            op_str := tprint(ast.op.kind) if ast.op.kind != 'k' else ast.op.keyword
            val = fmt.tprint("(", op_str, val, ")", sep="")

        case Expr_Postfix:
            ast := cast(^Expr_Postfix)ast;
            val = walker(ast.left)
            op_str := tprint(ast.op.kind) if ast.op.kind != 'k' else ast.op.keyword
            val = fmt.tprint("(", val, op_str, ")" , sep="")

        case Expr_Infix:
            ast := cast(^Expr_Infix)ast;
            left  := walker(ast.left)
            right := walker(ast.right)
            op_str := tprint(ast.op.kind) if ast.op.kind != 'k' else ast.op.keyword
            val = fmt.tprint("(", left, ' ', op_str, ' ', right, ")", sep="")

        case Expr_Grouped:
            ast := cast(^Expr_Grouped)ast;
            val = walker(ast.expr)
            /*
             * Maybe we need to surround with parens -> val = fmt.tprint("(", val, ")", sep="")
             * But prolly not because it is assumed it already parenthesized
            */
            val = fmt.tprint(val, sep="")

        case Expr_Function_Call:
            ast := cast(^Expr_Function_Call)ast;
            val = walker(ast.left)

            val = fmt.tprint(val, "(", sep="")
            for e, i in ast.exprs {
                if i == len(ast.exprs)-1 {
                    val = fmt.tprint(val, walker(e), sep="")
                } else {
                    val = fmt.tprint(val, walker(e), ", ", sep="")
                }
            }
            val = fmt.tprint(val,")", sep="")

        case:
            assert(false,"Unhandled walker parenthesize")
    }

    return val
}

walker_print :: proc(ast: ^Ast, indent:=0) {
    if ast == nil { return }
    indent := indent

    using fmt
    walker :: walker_print

    print("\n")
    for _ in 0..=indent {
        print(" ")
    }

    indent += 2

    switch ast.kind {

        case Start:
            ast := cast(^Start)ast;
            print(ast.kind)
            for a in ast.assignments {
                walker(a, indent)
            }
            walker(ast.expr, indent)

        case Assign:
            ast := cast(^Assign)ast;
            print(ast.kind, ast.identifier.identifier)
            walker(ast.expr, indent)

        case Assign_Function:
            ast := cast(^Assign_Function)ast;
            print(ast.kind, ast.identifier.identifier, ast.assign.kind, "fn(")
            for params, idx in ast.params {
                if idx < len(ast.params)-1 {
                    print(params.identifier, ", ", sep="")
                } else {
                    print(params.identifier)
                }
            }
            print(')')
            walker(ast.expr, indent)

        case Expr:
            assert(false, "WTFF this is abstract expr lil bro ")
        case Expr_Number:
            ast := cast(^Expr_Number)ast;
            print(ast.kind, '=', ast.number.number)

        case Expr_Identifier:
            ast := cast(^Expr_Identifier)ast;
            print(ast.kind, '=', ast.identifier.identifier)

        case Expr_Prefix:
            ast := cast(^Expr_Prefix)ast;
            print(ast.kind, '=', ast.op.kind)
            walker(ast.right, indent)

        case Expr_Infix:
            ast := cast(^Expr_Infix)ast;
            op_str := tprint(ast.op.kind) if ast.op.kind != 'k' else ast.op.keyword
            print(ast.kind, '=',op_str )
            walker(ast.left, indent)
            walker(ast.right, indent)

        case Expr_Postfix:
            ast := cast(^Expr_Postfix)ast;
            print(ast.kind, '=', ast.op.kind)
            walker(ast.left, indent)

        case Expr_Grouped:
            ast := cast(^Expr_Grouped)ast;
            print(ast.kind)
            walker(ast.expr, indent)

        case Expr_Mixfix:
            ast := cast(^Expr_Mixfix)ast;
            print(ast.kind)
            print(' ' ,ast.op1.kind, sep="")
            print(' ' ,ast.op2.kind, sep="")
            walker(ast.left, indent)
            walker(ast.mid, indent)
            walker(ast.right, indent)

        case Expr_Function_Call:
            ast := cast(^Expr_Function_Call)ast;
            print(ast.kind)
            walker(ast.left, indent)
            for e in ast.exprs {
                walker(e, indent)
            }
            
        case:
            assert(false, fmt.tprint("Unhandled token on walker_print ", ast.kind))
    }

    indent -= 2
    if indent == 0 {
        println()
    }
}

import "core:fmt"
import "core:math"
