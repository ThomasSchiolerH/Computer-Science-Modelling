module Interpreter

open Types
open Parse
open FSharp.Text.Lexing
open System
open AST
open Graph

(*
    This defines the input and output for the interpreter. Please do not
    change the definitions below as they are needed for the validation and
    evaluation tools!
*)
type InterpreterMemory =
    { variables: Map<string, int>
      arrays: Map<string, List<int>> }

type Input =
    { determinism: Determinism
      assignment: InterpreterMemory
      trace_length: int }

type Node = Graph.Node

type TerminationState =
    | Running
    | Stuck
    | Terminated

type ProgramTrace = 
    { node : string
      state : TerminationState
      memory : InterpreterMemory }

type Configuration<'node> =
    { node: 'node
      memory: InterpreterMemory }

type Output =
    { execution_sequence: List<Configuration<string>>
      final: TerminationState }

// let stringifyNode (internalNode: Node) : string =
//    match internalNode with
//    | I(-1) -> "qF"
//    | _ -> nodeToString(internalNode)

let prepareConfiguration (c: Configuration<Node>) : Configuration<string> =
    { node = node2string c.node
      memory = c.memory }

let rec ArithmeticSemantic (lab: expr) (m: InterpreterMemory) =
    match lab with
    | Num(n) -> Some(n |> int)
    | String(x) ->
        if 
            m.variables.ContainsKey x
        then
            Some(m.variables.[x])
        else
            None
    | TimesExpr(a1,a2) ->
        let a1Semantic = ArithmeticSemantic a1 m 
        let a2Semantic = ArithmeticSemantic a2 m
        if
            a1Semantic.IsSome && a2Semantic.IsSome
        then
            Some(a1Semantic.Value*a2Semantic.Value)
        else
            None
    | DivExpr(a1,a2) ->
        let a1Semantic = ArithmeticSemantic a1 m 
        let a2Semantic = ArithmeticSemantic a2 m
        if
            a1Semantic.IsSome && a2Semantic.IsSome
        then
            Some(a1Semantic.Value/a2Semantic.Value)
        else
            None
    | PlusExpr(a1,a2) ->
        let a1Semantic = ArithmeticSemantic a1 m 
        let a2Semantic = ArithmeticSemantic a2 m
        if 
            a1Semantic.IsSome && a2Semantic.IsSome 
        then
            Some(a1Semantic.Value+a2Semantic.Value) 
        else
            None
    | MinusExpr(a1,a2) ->
        let a1Semantic = ArithmeticSemantic a1 m 
        let a2Semantic = ArithmeticSemantic a2 m
        if 
            a1Semantic.IsSome && a2Semantic.IsSome 
        then
            Some(a1Semantic.Value-a2Semantic.Value) 
        else
            None
    | PowExpr(a1,a2) ->
        let a1Semantic = ArithmeticSemantic a1 m 
        let a2Semantic = ArithmeticSemantic a2 m
        if 
            a1Semantic.IsSome && a2Semantic.IsSome && (a2Semantic.Value>=0) 
        then 
            Some(((a1Semantic.Value|>float)**(a2Semantic.Value|>float))|>int) 
        else
            None
    | UMinusExpr(a1) ->
        let a1Semantic = ArithmeticSemantic a1 m
        if 
            a1Semantic.IsSome then Some(-1*a1Semantic.Value) 
        else
            None

and BooleanSemantic  (lab: BooleanExpression) (m : InterpreterMemory) =
    match lab with
    | True -> Some(true)
    | False -> Some(false)
    | And(b1,b2) ->
        let b1Semantic = BooleanSemantic b1 m
        let b2Semantic = BooleanSemantic b2 m
        if b1Semantic.IsNone then None else (if b1Semantic.Value then Some(b2Semantic.Value) else Some(false)) 
    | Or(b1,b2) ->
        let b1Semantic = BooleanSemantic b1 m
        let b2Semantic = BooleanSemantic b2 m
        if b1Semantic.IsNone then None else (if b1Semantic.Value then Some(b1Semantic.Value) else Some(b2Semantic.Value)) 
    | LogAnd(b1,b2) ->
        let b1Semantic = BooleanSemantic b1 m
        let b2Semantic = BooleanSemantic b2 m
        if b1Semantic.IsNone || b2Semantic.IsNone then None else (if b1Semantic.Value && b2Semantic.Value then Some(true) else Some(false))
    | LogOr(b1,b2) ->
        let b1Semantic = (BooleanSemantic b1 m)
        let b2Semantic = BooleanSemantic b2 m
        if b1Semantic.Value = true || b2Semantic.Value = true then Some(true) else (if b1Semantic.Value = false && b2Semantic.Value = false then Some(false) else None)
    | Negate(b1) ->
        let b1Semantic = BooleanSemantic b1 m
        if b1Semantic.IsNone then None else if b1Semantic.Value = false then Some(true) else Some(false)
    | Equal(a1,a2) ->
        let a1Semantic = ArithmeticSemantic a1 m
        let a2Semantic = ArithmeticSemantic a2 m
        if a1Semantic.IsSome && a2Semantic.IsSome then (if (ArithmeticSemantic a1 m).Value = (ArithmeticSemantic a2 m).Value then Some(true) else Some(false)) else None
    | NotEqual(a1,a2) ->
        let a1Semantic = ArithmeticSemantic a1 m
        let a2Semantic = ArithmeticSemantic a2 m
        if a1Semantic.IsSome && a2Semantic.IsSome then (if (ArithmeticSemantic a1 m).Value <> (ArithmeticSemantic a2 m).Value then Some(true) else Some(false)) else None
    | GreaterThan(a1,a2) ->
        let a1Semantic = ArithmeticSemantic a1 m
        let a2Semantic = ArithmeticSemantic a2 m
        if a1Semantic.IsSome && a2Semantic.IsSome then (if (ArithmeticSemantic a1 m).Value > (ArithmeticSemantic a2 m).Value then Some(true) else Some(false)) else None  
    | GreaterEqual(a1,a2) ->
        let a1Semantic = ArithmeticSemantic a1 m
        let a2Semantic = ArithmeticSemantic a2 m
        if a1Semantic.IsSome && a2Semantic.IsSome then (if (ArithmeticSemantic a1 m).Value >= (ArithmeticSemantic a2 m).Value then Some(true) else Some(false)) else None 
    | LessThan(a1,a2) ->
        let a1Semantic = ArithmeticSemantic a1 m
        let a2Semantic = ArithmeticSemantic a2 m
        if a1Semantic.IsSome && a2Semantic.IsSome then (if (ArithmeticSemantic a1 m).Value < (ArithmeticSemantic a2 m).Value then Some(true) else Some(false)) else None 
    | LessEqual(a1,a2) ->
        let a1Semantic = ArithmeticSemantic a1 m
        let a2Semantic = ArithmeticSemantic a2 m
        if a1Semantic.IsSome && a2Semantic.IsSome then (if (ArithmeticSemantic a1 m).Value <= (ArithmeticSemantic a2 m).Value then Some(true) else Some(false)) else None
    | _ -> None

//implement for whole language
let Semantic(lab: Label, m: InterpreterMemory) : Option<'InterpreterMemory> =
    match lab with
    | C(ast) ->
        match ast with
        | Skip -> Some(m)
        | VariableAssign(x, a) ->
            let (a1: int option) = (ArithmeticSemantic a m)
            if a1.IsSome then Some( {variables = m.variables.Add(x,a1.Value); arrays =m.arrays} ) else None
    | B(ast) ->
        let boolSemanticResult = BooleanSemantic ast m
        if boolSemanticResult.IsSome && boolSemanticResult.Value then Some(m) else None
    | _ -> None

//def 1.11
let rec ExecutionSteps(pg: ProgramGraph, q: Node, m: InterpreterMemory) : List<Configuration<'node>> =
    match pg with
    | e::pg2 ->
        let lab = e.label
        let t = e.target
        if not (e.source.Equals(q)) then
            ExecutionSteps(pg2, q, m)
        else
            let memPrime = Semantic(lab, m)
            if memPrime.IsSome then [ {node=t; memory=memPrime.Value} ]@ExecutionSteps(pg2, q, m)
            else
                ExecutionSteps(pg2, q, m)
    | [] -> []
    
// def 1.13    
let rec ExecutionSequence (pg: ProgramGraph, q: Node, m: InterpreterMemory, tl: int, stateCount: int) =
    let nextStates = ExecutionSteps(pg, q, m)
    match tl, nextStates with
    | 0, _ -> [ {node=q; memory=m} ], 0
    | _, [] -> ([ {node=q; memory=m} ], if q.Equals(F("qF")) then 2 else 1)
    | _, config::confList -> let iniEdge = [ {node=q; memory = m} ]
                             let nextEdge, newCount = ExecutionSequence(pg, config.node, config.memory, tl-1, stateCount)
                             (iniEdge@nextEdge, newCount)

let analysis (src: string) (input: Input) : Output =
    match parse Parser.startGCL src with
    | Ok ast ->
        let pg = ast2pg(C(ast))
        let e, stateCounter = ExecutionSequence(pg, F("qS"), input.assignment, input.trace_length, 0)
        let execution_sequence: List<Configuration<Node>> = e
        let final: TerminationState = if stateCounter = 1 then Stuck else if stateCounter = 2 then Terminated else Running
        { execution_sequence = List.map prepareConfiguration execution_sequence
          final = final }
    | Error error -> failwith "Analysis error"