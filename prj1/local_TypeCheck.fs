module TypeCheck

open AST

// Symbol table is a mapping from 'Identifier' (string) to 'CType'. Note that
// 'Identifier' and 'Ctype' are defined in AST.fs file.
type SymbolTable = Map<Identifier,CType>

// For semantic analysis, you will need the following type definition. Note the
// difference between 'Ctype' and 'Typ': 'Ctype' represents the type annoted in
// the C code, whereas 'Typ' represents the type obtained during type checking.
type Typ = Int | Bool | NullPtr | IntPtr | BoolPtr | Error

// Convert 'CType' into 'Typ'.
let ctypeToTyp (ctype: CType) : Typ =
  match ctype with
  | CInt -> Int
  | CBool -> Bool
  | CIntPtr -> IntPtr
  | CBoolPtr -> BoolPtr

// Check expression 'e' and return its type. If the type of expression cannot be
// decided due to some semantic error, return 'Error' as its type.
let rec checkExp (symTab: SymbolTable) (e: Exp) : Typ =
  match e with
  | Null -> NullPtr
  | Num _ -> Int
  | Boolean _ -> Bool
  | Var x -> 
      let t = Map.containsKey x symTab
      match t with
      | true -> ctypeToTyp (Map.find x symTab)
      | false -> Error
  | Deref x ->
      if Map.containsKey x symTab then
        let xt = ctypeToTyp (Map.find x symTab)
        match xt with
        | IntPtr -> Int
        | BoolPtr -> Bool
        | _ -> Error
      else
        Error
  | AddrOf x -> 
      if Map.containsKey x symTab then
        let xt = ctypeToTyp (Map.find x symTab)
        match xt with
        | Int -> IntPtr
        | Bool -> BoolPtr
        | _ -> Error
      else
        Error
  | Neg x ->
      if ((checkExp symTab x) = Int) then Int else Error
  | Add (e1, e2) 
  | Sub (e1, e2) 
  | Mul (e1, e2)
  | Div (e1, e2) ->
      if ((checkExp symTab e1) = Int && (checkExp symTab e2) = Int) then Int else Error
  | Equal (e1, e2) 
  | NotEq (e1, e2) ->
      let t1 = checkExp symTab e1
      let t2 = checkExp symTab e2
      if (t1 = Error || t2 = Error) then Error
      else if (t1 = t2 || ((t1 = IntPtr || t1 = BoolPtr) && t2 = NullPtr)) then Bool else Error
  | LessEq (e1, e2) 
  | LessThan (e1, e2) 
  | GreaterEq (e1, e2) 
  | GreaterThan (e1, e2) ->
      if ((checkExp symTab e1) = Int && (checkExp symTab e2) = Int) then Bool else Error
  | And (e1, e2) 
  | Or (e1, e2) ->
      let t1 = checkExp symTab e1
      let t2 = checkExp symTab e2
      if (t1 = Bool && t2 = Bool) then Bool else Error
  | Not e -> 
      if ((checkExp symTab e) = Bool) then Bool else Error

// Check statement 'stmt' and return a pair of (1) list of line numbers that
// contain semantic errors, and (2) symbol table updated by 'stmt'.
let rec checkStmt (symTab: SymbolTable) (retTyp: CType) (stmt: Stmt) =
  match stmt with
  | Declare (line, ctyp, x) ->
      // If you think this statement is error-free, then return [] as error line
      // list. If you think it contains an error, you may return [line] instead.
    ([], Map.add x ctyp symTab)
  | Define (line, ctyp, x, e) ->
    let t = checkExp symTab e
    let expt = ctypeToTyp ctyp
    if t = expt || (t = NullPtr && (expt = IntPtr || expt = BoolPtr)) then ([], Map.add x ctyp symTab)
    else ([line], symTab)
  | Assign(line, x, e) ->
     if Map.containsKey x symTab then
        let xt = ctypeToTyp (Map.find x symTab) // Typ of x
        let et = checkExp symTab e // Typ of e
        if (xt = et) then ([], symTab)
        else if((xt = IntPtr || xt = BoolPtr) && et = NullPtr) then ([], symTab) else ([line], symTab)
     else ([line], symTab)
  | PtrUpdate(line, x, e) ->
     if Map.containsKey x symTab then
        let xt = ctypeToTyp (Map.find x symTab) // Typ of x
        let et = checkExp symTab e // Typ of e
        match xt with
        | IntPtr -> if (et = Int || et = NullPtr) then ([], symTab) else ([line], symTab)
        | BoolPtr -> if (et = Bool || et = NullPtr) then ([], symTab) else ([line], symTab)
        | _ -> ([line], symTab)
     else ([line], symTab)
  | Return (line, e) ->
    let et = checkExp symTab e
    let ret = ctypeToTyp retTyp
    if (et = ret) then ([], symTab) else ([line], symTab)
  | If (line, e, s1, s2) ->
    let et = checkExp symTab e //condition of If
    if (et = Error) then ([line],symTab)
    else
      let s1t = checkStmts symTab retTyp s1
      let s2t = checkStmts symTab retTyp s2
      if (s1t = [] && s2t = []) then ([], symTab) else (s1t @ s2t, symTab)
  | While (line, e, s) ->
    let et = checkExp symTab e
    if (et = Error) then ([line], symTab)
    else
      let st = checkStmts symTab retTyp s
      if (st = []) then ([], symTab) else ([line], symTab)

// Check the statement list and return the line numbers of semantic errors. Note
// that 'checkStmt' and 'checkStmts' are mutually-recursive (they can call each
// other). This function design will make your code more concise.
and checkStmts symTab (retTyp: CType) (stmts: Stmt list): LineNo list =
  match stmts with
  | [] -> []
  | head :: tail ->
    let (errorLine, symTab) = checkStmt symTab retTyp head
    let restError = checkStmts symTab retTyp tail
    match errorLine with
    | [] -> restError
    | _ -> errorLine @ restError

// Record the type of arguments to the symbol table.
let rec collectArgTypes argDecls symTab =
  match argDecls with
  | [] -> symTab
  | (ctyp, x) :: tail -> collectArgTypes tail (Map.add x ctyp symTab)

// Check the program and return the line numbers of semantic errors.
let run (prog: Program) : LineNo list =
  let (retTyp, _, args, stmts) = prog
  let symTab = collectArgTypes args Map.empty
  let errorLines = checkStmts symTab retTyp stmts
  // Remove duplicate entries and sort in ascending order.
  List.sort (List.distinct errorLines)