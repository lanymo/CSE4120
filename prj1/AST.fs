namespace AST

type Identifier = string

type LineNo = int

type Exp =
  | Null
  | Num of int
  | Boolean of bool
  | Var of string // Ex) "x"
  | Deref of string // Ex) "*x"
  | AddrOf of string // Ex) "&x"
  | Neg of Exp // -E
  | Add of Exp * Exp // E + E
  | Sub of Exp * Exp // E - E
  | Mul of Exp * Exp // E * E
  | Div of Exp * Exp // E / E
  | Equal of Exp * Exp // E == E
  | NotEq of Exp * Exp // E != E
  | LessEq of Exp * Exp // E <= E
  | LessThan of Exp * Exp // E < E
  | GreaterEq of Exp * Exp // E >= E
  | GreaterThan of Exp * Exp // E > E
  | And of Exp * Exp // E && E
  | Or of Exp * Exp // E || E
  | Not of Exp // !E

type CType =
  | CInt // "int" in C
  | CBool // "bool" in C
  | CIntPtr // "int*" in C
  | CBoolPtr // "bool*" in C

// Each statement is associated with the line number. This line number will be
// used to report error. For "If" and "While", the line number denotes the
// position of the closing parenthesis ')' in the statement.
type Stmt =
  | Declare of LineNo * CType * Identifier // Ex) "int x;"
  | Define of LineNo * CType * Identifier * Exp // Ex) "int x = 0;"
  | Assign of LineNo * Identifier * Exp // Ex) "x = 10;"
  | PtrUpdate of LineNo * Identifier * Exp // Ex) "*x = 10;"
  | Return of LineNo * Exp // Ex) "return 0;"
  | If of LineNo * Exp * Stmt list * Stmt list // if (E) { S } else { S }
  | While of LineNo * Exp * Stmt list // while (E) { S }

// Argument declaration consists of argument's type and name.
type ArgDecl = CType * Identifier

// Function definition consists of (1) return type, (2) name of the function,
// (3) argument declarations, and (4) body statements.
type Function = CType * Identifier * ArgDecl list * Stmt list

type Program = Function

(*****************************************************************************
 * Start of functions for converting AST into string. You don't have to read *
 * read and understand the code below.                                       *
 * ***************************************************************************)

module Exp =
  let rec toStr exp =
    match exp with
    | Null -> "NULL"
    | Num i -> string i
    | Boolean true -> "true"
    | Boolean false -> "false"
    | Var vname -> vname
    | Deref vname -> sprintf "*%s" vname
    | AddrOf vname -> sprintf "&%s" vname
    | Add (e1, e2) -> sprintf "(%s + %s)" (toStr e1) (toStr e2)
    | Sub (e1, e2) -> sprintf "(%s - %s)" (toStr e1) (toStr e2)
    | Mul (e1, e2) -> sprintf "(%s * %s)" (toStr e1) (toStr e2)
    | Div (e1, e2) -> sprintf "(%s / %s)" (toStr e1) (toStr e2)
    | Neg e -> sprintf "-%s" (toStr e)
    | Equal (e1, e2) -> sprintf "(%s == %s)" (toStr e1) (toStr e2)
    | NotEq (e1, e2) -> sprintf "(%s != %s)" (toStr e1) (toStr e2)
    | LessEq (e1, e2) -> sprintf "(%s <= %s)" (toStr e1) (toStr e2)
    | LessThan (e1, e2) -> sprintf "(%s < %s)" (toStr e1) (toStr e2)
    | GreaterEq (e1, e2) -> sprintf "(%s >= %s)" (toStr e1) (toStr e2)
    | GreaterThan (e1, e2) -> sprintf "(%s > %s)" (toStr e1) (toStr e2)
    | And (e1, e2) -> sprintf "(%s && %s)" (toStr e1) (toStr e2)
    | Or (e1, e2) -> sprintf "(%s || %s)" (toStr e1) (toStr e2)
    | Not e -> sprintf "!%s" (toStr e)

module CType =
  let toStr typ =
    match typ with
    | CInt -> "int"
    | CBool -> "bool"
    | CIntPtr -> "int*"
    | CBoolPtr -> "bool*"

module Stmt =
  let rec toStr indent stmt =
    match stmt with
    | Declare (_, t, x) -> sprintf "%s%s %s" indent (CType.toStr t) x
    | Define (_, t, x, e) ->
      sprintf "%s%s %s = %s" indent (CType.toStr t) x (Exp.toStr e)
    | Assign (_, x, e) -> sprintf "%s%s = %s" indent x (Exp.toStr e)
    | PtrUpdate (_, x, e) -> sprintf "%s*%s = %s" indent x (Exp.toStr e)
    | Return (_, e) -> sprintf "%sreturn %s" indent (Exp.toStr e)
    | If (_, e, s1, s2) ->
        let expStr = Exp.toStr e
        let s1Lines = List.map (toStr (indent + "  ")) s1
        let s1Str = String.concat ",\n" s1Lines
        let s2Lines = List.map (toStr (indent + "  ")) s2
        let s2Str = String.concat ",\n" s2Lines
        sprintf "%sif (%s) [\n%s\n%s] else [\n%s\n%s]"
          indent expStr s1Str indent s2Str indent
    | While (_, e, s) ->
        let expStr = Exp.toStr e
        let stmtLines = List.map (toStr (indent + "  ")) s
        let stmtStr = String.concat ",\n" stmtLines
        sprintf "%swhile (%s) [\n%s\n%s]" indent expStr stmtStr indent

module Program =
  let argToStr decl =
    let (typ, vname) = decl
    sprintf "%s %s" (CType.toStr typ) vname

  let toStr (p: Program) : string =
    let retType, fname, args, stmts = p
    let retStr = CType.toStr retType
    let argStr = List.map argToStr args |> String.concat ", "
    let stmtStr = List.map (Stmt.toStr "  ") stmts |> String.concat ",\n"
    sprintf "%s %s(%s) [\n%s\n]" retStr fname argStr stmtStr
