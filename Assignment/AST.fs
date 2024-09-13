// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module AST

// a  ::=  n  |  x  |  A[a]  |  a + a  |  a - a  |  a * a  |  a / a  |  - a  |  a ^ a  |  (a)
// ArithmeticExpression
type expr = 
    | Num of float  // n  
    | String of string   // x
    | ArrayAccess of (string * expr)    // A[a]  
    | TimesExpr of (expr * expr) // a * a  
    | DivExpr of (expr * expr)   // a / a 
    | PlusExpr of (expr * expr) // a + a  
    | MinusExpr of (expr * expr)    // a - a  
    | PowExpr of (expr * expr)  // a ^ a 
    | UMinusExpr of (expr)    // | - a
    | UPlusExpr of (expr)    // | + a
    | ParExpr of (expr) // (a)

// b  ::=  true  |  false  |  b & b  |  b | b  |  b && b  |  b || b  |  ! b
//     |  a = a  |  a != a  |  a > a  |  a >= a  |  a < a  |  a <= a  |  (b)
type BooleanExpression = 
    | True // true
    | False // false
    | And of (BooleanExpression * BooleanExpression) // b & b
    | Or of (BooleanExpression * BooleanExpression) // b | b
    | LogAnd of (BooleanExpression * BooleanExpression) // b && b
    | LogOr of (BooleanExpression * BooleanExpression) // b || b
    | Negate of BooleanExpression // ! b
    | Par of BooleanExpression // (b)
    | Equal of (expr * expr) // a = a
    | NotEqual of (expr * expr) // a != a
    | GreaterThan of (expr * expr) // a > a
    | GreaterEqual of (expr * expr) // a >= a  
    | LessThan of (expr * expr) // a < a
    | LessEqual of (expr * expr) // a <= a

// // C  ::=  x := a  |  A[a] := a  |  skip  |  C ; C  |  if GC fi  |  do GC od    
type Command = 
    | VariableAssign of (string * expr) // x := a
    | ArrayAssign of (string*expr*expr) // A[a] := a
    | Skip // skip
    | Sequence of Command * Command// C ; C
    | If of GuardedCommand //  if GC fi
    | Do of GuardedCommand //  do GC od

// GC ::=  b -> C  |  GC [] GC
and GuardedCommand =
    | Cond of (BooleanExpression * Command) // b -> C
    | Else of (GuardedCommand * GuardedCommand) // GC [] GC

type AST = 
    | S of string
    | E of expr
    | B of BooleanExpression
    | C of Command
    | GC of GuardedCommand
