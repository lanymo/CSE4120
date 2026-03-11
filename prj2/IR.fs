namespace IR

type Register = string

type Label = string

(* An operand can be either register or immediate (constant integer). *)
type Operand = Reg of Register | Imm of int

(* Kinds of unary operation *)
type UnaryOp = NegOp | NotOp

(* Kinds of binary operation. Note that AndOp and OrOp are not needed. *)
type BinaryOp =
  // Arithmetic operations (+, -, *, /)
  | AddOp | SubOp | MulOp | DivOp
  // Comparison operations (=, !=, >=, >, <=, <) that are evaluated into 1 if
  // true and 0 if false.
  | EqOp | NeqOp | LeqOp | LtOp | GeqOp | GtOp

type Instr =
  | Set of Register * Operand // Set (r, o) means "r = o"
  | LocalAlloc of Register * int // LocalAlloc (r, n) means "r = alloc(n)"
  | UnOp of Register * UnaryOp * Operand //  Ex) "r = - o"
  | BinOp of Register * BinaryOp * Operand * Operand // Ex) "r = o1 + o2"
  | Load of Register * Register // Load (r1, r2) means "r1 = load r2"
  | Store of Operand * Register // Store (o, r) means "store o, r" ("*r = o")
  | Goto of Label // Jump to the specified label
  | GotoIf of Operand * Label // Jump if the operand value is non-zero
  | GotoIfNot of Operand * Label // Jump if the operand value is zero
  | Label of Label // No operation (used only to mark the position of label)
  | Ret of Operand // Return a value

(* IR-level function := function name * arguments as registers * instructions *)
type IRFunction = string * Register list * Instr list

(* We assume that a program consists of only one function *)
type IRCode = IRFunction

(**************************************************************************
 * Start of functions for converting IR code into string. You don't have to
 * understand the code below.
 * ************************************************************************)

module Instr =
  let oprndToStr oprnd =
    match oprnd with
    | Reg r -> r
    | Imm i -> sprintf "%d" i

  let unopToStr optyp =
    match optyp with
    | NegOp -> "-"
    | NotOp -> "!"

  let binopToStr optyp =
    match optyp with
    | AddOp -> "+"
    | SubOp -> "-"
    | MulOp -> "*"
    | DivOp -> "/"
    | EqOp -> "=="
    | NeqOp -> "!="
    | LeqOp -> "<="
    | LtOp -> "<"
    | GeqOp -> ">="
    | GtOp -> ">"

  let toStr instr =
    match instr with
    | Set (r, v) -> sprintf "%s = %s" r (oprndToStr v)
    | LocalAlloc (r, sz) -> sprintf "%s = alloc(%d)" r sz
    | UnOp (r, op, v) -> sprintf "%s = %s %s" r (unopToStr op) (oprndToStr v)
    | BinOp (r, op, v1, v2) ->
        let left_str = oprndToStr v1 in
        let binop_str = binopToStr op in
        let right_str = oprndToStr v2 in
        sprintf "%s = %s %s %s" r left_str binop_str right_str
    | Load (r1, r2) -> sprintf "%s = load %s" r1 r2
    | Store (o, r) -> sprintf "store %s, %s" (oprndToStr o) r
    | Goto l -> sprintf "goto %s" l
    | GotoIf (v, l) -> sprintf "if %s then goto %s" (oprndToStr v) l
    | GotoIfNot (v, l) -> sprintf "ifnot %s then goto %s" (oprndToStr v) l
    | Label l -> sprintf "label %s" l
    | Ret v -> sprintf "ret %s" (oprndToStr v)

module IRCode =
  let toStr (ir: IRCode): string =
    let (fname, args, instrs) = ir in
    let arg_str = String.concat ", " args in
    let instr_str = List.map Instr.toStr instrs |> String.concat ",\n  " in
    sprintf "%s(%s) : [\n  %s\n]" fname arg_str instr_str
