package pratt

Scope :: #type map[string]Symbol
Scope_Table :: [dynamic]Scope

Symbol :: struct  {
    name : string,
    val: f64,
    is_function: bool,
    params: []Token,
    body: ^Expr,
}

scopes := Scope_Table{}

scope_enter :: proc(scope := Scope{}) {
    append(&scopes, scope)
}

scope_exit :: proc() -> Scope{
    return pop(&scopes)
}

scope_reset :: proc() {
    for s, idx in scopes {
        delete(s)
        ordered_remove(&scopes, idx)
    }
}

scope_get :: proc(identifier: string) -> (sym: Symbol, ok: bool) {
    ok = false

    idx := len(scopes) // start from last scope
    for idx > 0 {
        idx -= 1
        scope := scopes[idx]
        if identifier in scope {
            ok = true
            sym = scope[identifier]
            return
        }
    }
    return
}

scope_add :: proc(identifier: string, sym: Symbol) {
    scope := scopes[len(scopes)-1]
    scope[identifier] = sym
    scopes[len(scopes)-1] = scope
}
