module SignAnalysis

open Types
open AST

(*
     This defines the input and output of the sign analysis. Please do not
    change the definitions below as they are needed for the validation and
    evaluation tools!
*)

type Sign =
    | Negative
    | Zero
    | Positive

type SignAssignment =
    { variables: Map<string, Sign>
      arrays: Map<string, Set<Sign>> }

type Input =
    { determinism: Determinism
      assignment: SignAssignment }

type Output =
    { initial_node: string
      final_node: string
      nodes: Map<string, Set<SignAssignment>> }

let combine_signs (signs: Set<Sign>) : Option<Sign * Set<Sign>> =
    if Set.contains Positive signs && Set.contains Negative signs then
        Some(Zero, Set.union (Set.singleton Positive) (Set.singleton Negative))
    elif Set.contains Positive signs then
        Some(Positive, Set.empty)
    elif Set.contains Negative signs then
        Some(Negative, Set.empty)
    else
        Some(Zero, Set.empty)

let rec abstract_eval_expr (expr: expr) (assignment: SignAssignment) : Sign =
    match expr with
    | Num n ->
        if n < 0.0 then Negative
        elif n = 0.0 then Zero
        else Positive
    | String s ->
        Map.find s assignment.variables
    //| AST.ArrayAccess (s, e) ->
    //    let index_sign = abstract_eval_expr e assignment
    //    let array_signs = Map.find s assignment.arrays
    //    match index_sign with
    //    | Negative | Zero | Positive -> array_signs
    | TimesExpr (e1, e2) ->
        let s1 = abstract_eval_expr e1 assignment
        let s2 = abstract_eval_expr e2 assignment
        match s1, s2 with
        | Zero, _ | _, Zero -> Zero
        | Positive, Positive | Negative, Negative -> Positive
        | Positive, Negative | Negative, Positive -> Negative
    | DivExpr (e1, e2) ->
        let s1 = abstract_eval_expr e1 assignment
        let s2 = abstract_eval_expr e2 assignment
        match s1, s2 with
        | Zero, _ -> Zero
        | Positive, Positive | Negative, Negative -> Positive
        | Positive, Negative | Negative, Positive -> Negative
        | _, Zero -> failwith "Division by zero not allowed"
    | PlusExpr (e1, e2) | MinusExpr (e1, e2) ->
        let s1 = abstract_eval_expr e1 assignment
        let s2 = abstract_eval_expr e2 assignment
        match combine_signs (Set.ofList [s1; s2]) with
        | Some(Zero, set) when not (Set.isEmpty set) -> failwith "Ambiguous sign: both positive and negative values possible"
        | Some(sign, _) -> sign
        | None -> failwith "Unexpected case in combine_signs"
    // | PowExpr (e1, e2) ->
    //     failwith "Not yet implemented"
    // | UMinusExpr e1 ->
    //     failwith "Not yet implemented"
    // | UPlusExpr e1 ->
    //     failwith "Not yet implemented"
    // | ParExpr e1 ->
    //     failwith "Not yet implemented"

let abstract_eval_bool (b: BooleanExpression) (assignment: SignAssignment) : bool =
    let rec eval_bool_expr (b: BooleanExpression) : bool =
        match b with
        | Equal (e1, e2) ->
            let s1 = abstract_eval_expr e1 assignment
            let s2 = abstract_eval_expr e2 assignment
            s1 = s2
        | NotEqual (e1, e2) ->
            let s1 = abstract_eval_expr e1 assignment
            let s2 = abstract_eval_expr e2 assignment
            s1 <> s2
        | LessThan (e1, e2) ->
            let s1 = abstract_eval_expr e1 assignment
            let s2 = abstract_eval_expr e2 assignment
            s1 = Negative && s2 = Positive
        | GreaterThan (e1, e2) ->
            let s1 = abstract_eval_expr e1 assignment
            let s2 = abstract_eval_expr e2 assignment
            s1 = Positive && s2 = Negative
        | LessEqual (e1, e2) ->
            let s1 = abstract_eval_expr e1 assignment
            let s2 = abstract_eval_expr e2 assignment
            s1 = Negative || s2 = Positive || s1 = s2
        | GreaterEqual (e1, e2) ->
            let s1 = abstract_eval_expr e1 assignment
            let s2 = abstract_eval_expr e2 assignment
            s1 = Positive || s2 = Negative || s1 = s2
        | And (b1, b2) ->
            let r1 = eval_bool_expr b1
            let r2 = eval_bool_expr b2
            r1 && r2
        | Or (b1, b2) ->
            let r1 = eval_bool_expr b1
            let r2 = eval_bool_expr b2
            r1 || r2
        | Negate b1 ->
            not (eval_bool_expr b1)

    eval_bool_expr b


let rec abstract_analyze_cmd (c: Command) (assignment: SignAssignment) : SignAssignment =
    match c with
    | VariableAssign (s, e) ->
        let sign = abstract_eval_expr e assignment
        { assignment with variables = Map.add s sign assignment.variables }
    | ArrayAssign (s, index_expr, value_expr) ->
        let index_sign = abstract_eval_expr index_expr assignment
        let value_sign = abstract_eval_expr value_expr assignment
        let existing_signs = Map.find s assignment.arrays
        let updated_signs = 
            match index_sign with
            | Negative | Zero | Positive -> Set.add value_sign existing_signs
        { assignment with arrays = Map.add s updated_signs assignment.arrays }
    //| If (b, c1, c2) ->
    //    let true_branch_assignment = abstract_analyze_cmd c1 assignment
    //    let false_branch_assignment = abstract_analyze_cmd c2 assignment
    //    SignAssignment.union true_branch_assignment false_branch_assignment
    //| Do (b, c1) ->
    //    let _ = abstract_eval_bool b assignment
    //    let body_assignment = abstract_analyze_cmd c1 assignment
    //    SignAssignment.union assignment body_assignment
    | Sequence (c1, c2) ->
        let updated_assignment = abstract_analyze_cmd c1 assignment
        abstract_analyze_cmd c2 updated_assignment
    | Skip ->
        assignment




let analysis (src: string) (input: Input) : Output =
    failwith "Sign analysis not yet implemented" // TODO: start here
