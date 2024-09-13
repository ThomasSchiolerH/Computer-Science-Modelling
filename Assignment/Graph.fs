module Graph

open Types
open Parse
open FSharp.Text.Lexing
open System
open AST

(*
    This defines the input and output for graphs. Please do not
    change the definitions below as they are needed for the validation and
    evaluation tools!
*)

type Input = { determinism: Determinism }

type Output = { dot: string }

let mutable i = 0

type Node = I of int | F of string

type Label = AST

type Edge = {
    source : Node;
    label : AST;
    target : Node
}

type ProgramGraph = Edge list

let rec guardedCommandEdgeDone (guardedcmd: GuardedCommand) : BooleanExpression =
    match guardedcmd with
    | Else(gc1, gc2) -> And(guardedCommandEdgeDone gc1, guardedCommandEdgeDone gc2)
    | Cond(b, _) -> Negate(b)

let rec edges (ast: AST, qS: Node, qF: Node) : ProgramGraph =

    let rec guardedCommandEdge (guardedcmd : GuardedCommand) : ProgramGraph =
        match guardedcmd with
        | Cond(b, c) -> i <- i+1; {source = qS; label = B(b); target = I(i)}::edges(C(c), qF, I(i))
        | Else(gc1, gc2) -> edges (GC(gc1), qS, qF) @ edges (GC(gc2), qS, qF)
    
    let rec commandEdge (cmd : Command) : ProgramGraph =
        match cmd with
        | Sequence(c1, c2) -> (i<-i+1); edges(C(c1), qS, I(i)) @ edges(C(c2), I(i), qF)
        | If(gcmd) -> edges(GC(gcmd), qS, qF)
        | Do(gcmd) -> edges(GC(gcmd), qS, qF) @ [{source = qS; label = B(guardedCommandEdgeDone gcmd); target = qF}]
        | _ -> [{source = qS; label = ast ; target = qF}]
    
    match ast with
    | C(c) -> commandEdge(c)
    | GC(gc) -> guardedCommandEdge(gc)


let label2dot (ast : AST) : string =
    // q> -> q1 [label = "skip"];
    sprintf "[label=\"" + prettyPrintAST(ast) + "\"]"

let node2string (n: Node) : string =
    match n with
    | I(i) -> string i
    | F(s) -> s

let edge2dot (e: Edge) : string =
    node2string(e.source) + " -> " + node2string(e.target) + " " + label2dot(e.label) + " ;"

let ast2pg (ast: AST) : List<Edge> =
    edges(ast, F("qS"), F("qF"))

let rec edges2dot (pg : list<Edge>) : string =
    match pg with
    | [] -> ""
    | e::pg -> sprintf "%s " (edge2dot e) + edges2dot pg

let pg2dot (pg : ProgramGraph) : string =
    "digraph program_graph {rankdir=LR; " + edges2dot(pg) + " }"

let analysis (src: string) (input: Input) : Output =
    match parse Parser.startGCL (src) with
        | Ok ast ->
            let pg = (ast2pg(C(ast)))
            let dotstring = pg2dot(pg)
            { dot = dotstring }
        | Error e -> { dot = "" }
