module Parse

open FSharp.Text.Lexing
open System
open AST

exception ParseError of Position * string * Exception

let parse parser src =
    let lexbuf = LexBuffer<char>.FromString src

    let parser = parser Lexer.tokenize

    try
        Ok(parser lexbuf)
    with
    | e ->
        let pos = lexbuf.EndPos
        let line = pos.Line
        let column = pos.Column
        let message = e.Message
        let lastToken = new String(lexbuf.Lexeme)
        eprintf "Parse failed at line %d, column %d:\n" line column
        eprintf "Last token: %s" lastToken
        eprintf "\n"
        Error(ParseError(pos, lastToken, e))

let newline = "\n"

let rec prettyPrintAST (ast:AST) = // i
   
    let prettyPrintCmd command = 
        match command with
        | VariableAssign(s,e) -> (prettyPrintAST (S(s))) + ":=" + (prettyPrintAST (E(e)))
        | ArrayAssign(s,e1,e2) -> (prettyPrintAST (S(s))) + "[" + (prettyPrintAST (E(e1))) + "]" + ":=" + (prettyPrintAST (E(e2)))
        | Skip -> "skip"
        | Sequence(c1,c2) -> (prettyPrintAST(C(c1))) + ";" + newline + (prettyPrintAST(C(c2)))
        | If(guardedcmd) -> "if" + (prettyPrintAST (GC(guardedcmd))) + "fi"
        | Do(guardedcmd) -> "do" + (prettyPrintAST (GC(guardedcmd))) + "od"
    
    let prettyPrintExpr expr = 
        match expr with
        | Num f -> string f
        | String f -> f
        | TimesExpr(e1,e2) -> (prettyPrintAST (E(e1))) + " * " + (prettyPrintAST (E(e2)))
        | DivExpr(e1,e2) -> (prettyPrintAST (E(e1))) + " / " + (prettyPrintAST (E(e2)))
        | PlusExpr(e1,e2) -> (prettyPrintAST (E(e1))) + " + " + (prettyPrintAST (E(e2)))
        | MinusExpr(e1,e2) -> (prettyPrintAST (E(e1))) + " - " + (prettyPrintAST (E(e2)))
        | PowExpr(e1,e2) -> (prettyPrintAST (E(e1))) + " ^ " + (prettyPrintAST (E(e2)))
        | UMinusExpr(e1) -> "-" + (prettyPrintAST (E(e1)))
        | UPlusExpr(e1) -> "+" + (prettyPrintAST (E(e1)))
        | ParExpr(e1) -> "(" + (prettyPrintAST (E(e1))) + ")"
    
    let prettyPrintGC guardedcmd =
        match guardedcmd with
        | Cond(b,c) -> (prettyPrintAST (B(b))) + "->" + (prettyPrintAST (C(c)))
        | Else(gc1, gc2) -> (prettyPrintAST (GC(gc1))) + " [] " + (prettyPrintAST (GC(gc2)))
    
    let prettyPrintBool boolean =
        match boolean with
        | True -> "true"
        | False -> "false"
        | And(b1,b2) -> (prettyPrintAST (B(b1))) + " & " + (prettyPrintAST (B(b2)))
        | Or(b1,b2) -> (prettyPrintAST (B(b1))) + " | " + (prettyPrintAST (B(b2)))
        | LogAnd(b1,b2) -> (prettyPrintAST (B(b1))) + " && " + (prettyPrintAST (B(b2)))
        | LogOr(b1,b2) -> (prettyPrintAST (B(b1))) + " || " + (prettyPrintAST (B(b2)))
        | Negate(b1) -> "! " + (prettyPrintAST (B(b1)))
        | Par(b1) -> "(" + (prettyPrintAST (B(b1))) + ")"
        | Equal(e1,e2) -> (prettyPrintAST (E(e1))) + " = " + (prettyPrintAST (E(e2)))
        | NotEqual(e1,e2) -> (prettyPrintAST (E(e1))) + " != " + (prettyPrintAST (E(e2)))
        | GreaterThan(e1,e2) -> (prettyPrintAST (E(e1))) + " > " + (prettyPrintAST (E(e2)))
        | GreaterEqual(e1,e2) -> (prettyPrintAST (E(e1))) + " >= " + (prettyPrintAST (E(e2)))
        | LessThan(e1,e2) -> (prettyPrintAST (E(e1))) + " < " + (prettyPrintAST (E(e2)))
        | LessEqual(e1,e2) -> (prettyPrintAST (E(e1))) + " <= " + (prettyPrintAST (E(e2)))
    
    match ast with
        | S(s) -> s
        | E(e) -> prettyPrintExpr e
        | B(b) -> prettyPrintBool b
        | C(c) -> prettyPrintCmd c
        | GC(gc) -> prettyPrintGC gc

let analysis (src: string) : string =
    match parse Parser.startGCL (src) with
        | Ok ast ->
            Console.Error.WriteLine("> {0}", ast)
            prettyPrintAST (C(ast))
        | Error e -> "Parse error: {0}"