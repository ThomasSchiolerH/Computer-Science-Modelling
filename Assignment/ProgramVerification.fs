module ProgramVerification

open System
open Predicate.AST

(*
    This defines the input and output for the program verification analysis.
    Please do not change the definitions below as they are needed for the
    validation and evaluation tools!
*)

type Input = unit

type Output =
    { verification_conditions: List<SerializedPredicate> }

let analysis (src: string) (input: Input) : Output =
    let (P, C, Q) =
        match Predicate.Parse.parse src with
        | Ok (AnnotatedCommand (P, C, Q)) -> P, C, Q
        | Error e ->
            failwith
                $"Failed to parse.\n\nDid you remember to surround your program with predicate blocks, like so?\n\n  {{ true }} skip {{ true }}\n\n{e}"

    let mutable count = 0
    
    let rec spCommand(cmd: Command, p: Predicate): Predicate =
        match cmd with
        | Skip -> p
        | Assign(x, a) -> assignFunc(x, a, p)
        | Sep(cmd1, cmd2) -> spCommand(cmd2, spCommand(cmd1, p))
        | If(gc) -> spGuardedCommand(gc, p)
        | Do(inv, gc) -> BooleanOp(inv, LOr, generateDoneVC(gc))
        | _ -> failwith "Sorry. An error occurred."
    
    and assignFunc(x: String, a: AExpr, pred: Predicate): Predicate =
        let freshVar = "_f" + count.ToString()
        count <- count + 1
        let combinedPred = BooleanOp(
            predicateSub(pred, Variable(freshVar), Variable(x)),
            LAnd,
            RelationalOp(Variable(x), Eq, expressionSub(a, Variable(freshVar), Variable(x)))
        )
    
        Exists(freshVar, combinedPred)
    
    and spGuardedCommand(gc: GuardedCommand, p: Predicate): Predicate =
        match gc with
        | Guard(bool, cmd) -> spCommand(cmd, BooleanOp(bool, LAnd, p))
        | Choice(gc1, gc2) -> BooleanOp(spGuardedCommand(gc1, p), LOr, spGuardedCommand(gc2, p))
    
    and predicateSub(p: Predicate, e: AExpr, x: AExpr) =
        match p with
        | Bool(b) -> Bool(b)
        | RelationalOp(e1, r, e2) ->
            RelationalOp(expressionSub(e1,e,x), r, expressionSub(e2,e,x))
        | Exists(i, p) -> Exists(i,predicateSub(p, e, x))
        | Forall(i, p) -> Forall(i,predicateSub(p, e, x))
        | Not(p) -> Not(predicateSub(p, e, x))
        | BooleanOp(p1, r, p2) -> BooleanOp(predicateSub(p1,e,x), r, predicateSub(p2,e,x))
    
    and expressionSub(e, e', y): AExpr =
        match e with
        | Number(n) -> Number(n)
        | Variable(x) -> if x.Equals(y.ToString()) then e' else Variable(x)
        | LogicalVariable(x) -> LogicalVariable(x)
        | Array(a, e1) -> Array(a, expressionSub(e1,e',y))
        | LogicalArray(a, e1) -> LogicalArray(a, expressionSub(e1, e', y))
        | Binary(e1, r, e2) -> Binary(expressionSub(e1, e', y), r, expressionSub(e2, e', y))
        | Function(Division(e1, e2)) -> Function(Division(expressionSub(e1, e', y), expressionSub(e2, e', y)))
        | Function(Min(e1,e2)) -> Function(Min(expressionSub(e1, e', y), expressionSub(e2, e', y)))
        | Function(Max(e1, e2)) -> Function(Max(expressionSub(e1, e', y), expressionSub(e2, e', y)))
        | Function(Count(s ,e1)) -> Function(Count(s, expressionSub(e1, e', y)))
        | Function(LogicalCount(s, e1)) -> Function(LogicalCount(s, expressionSub(e1, e', y)))
        | Function(Length(s)) -> Function(Length(s))
        | Function(LogicalLength(s)) -> Function(LogicalLength(s))
        | Function(Fac(e1)) -> Function(Fac(expressionSub(e1, e', y)))
        | Function(Fib(e1)) -> Function(Fib(expressionSub(e1, e', y)))
    
    and generateCmdVC(cmd: Command, R: Predicate) =
        match cmd with
        | Skip -> []
        | Assign(x, a) -> []
        | Sep(c1, c2) -> generateCmdVC(c1, R) @ generateCmdVC(c2, spCommand(c1, R))
        | If(gc) -> generateGuardedCmdVC(gc, R)
        | Do(I, gc) -> [BooleanOp(R, Implies, I); BooleanOp(spGuardedCommand(gc, I), Implies, I)] @ generateGuardedCmdVC(gc, I)
        | _ -> failwith "Sorry. An error occurred."
    
    and generateGuardedCmdVC(gc: GuardedCommand, R: Predicate) =
        match gc with
        | Guard(b, c1) -> generateCmdVC(c1, BooleanOp(b, LAnd, R))
        | Choice(gc1, gc2) -> generateGuardedCmdVC(gc1, R) @ generateGuardedCmdVC(gc2, R)
    
    and generateDoneVC(gc: GuardedCommand) =
        match gc with
        | Guard(b, _) -> Not(b)
        | Choice(gc1, gc2) -> BooleanOp(generateDoneVC(gc1), LAnd, generateDoneVC(gc2))
    
    let vConditions: List<Predicate> =
        [BooleanOp(spCommand(C, P), Implies, Q)] @ generateCmdVC(C, P)
    
    // Let this line stay as it is.
    { verification_conditions = List.map serialize_predicate vConditions }