module Optimize

open IR
open CFG
open DFA


module ConstantFolding =
  let foldConstant instr =
    match instr with
    | UnOp (r, NegOp, Imm x) -> (true, Set (r, Imm (-x)))
    | UnOp (r, NotOp, Imm x) -> (true, Set (r, Imm (if x = 0 then 1 else 0)))
    | BinOp (r, AddOp, Imm x, Imm y) -> (true, Set (r, Imm (x + y)))
    | BinOp (r, SubOp, Imm x, Imm y) -> (true, Set (r, Imm (x - y)))
    | BinOp (r, MulOp, Imm x, Imm y) -> (true, Set (r, Imm (x * y)))
    | BinOp (r, DivOp, Imm x, Imm y) -> (true, Set (r, Imm (x / y)))
    | BinOp (r, EqOp, Imm x, Imm y) -> (true, Set (r, Imm (if x = y then 1 else 0)))
    | BinOp (r, NeqOp, Imm x, Imm y) -> (true, Set (r, Imm (if x <> y then 1 else 0)))
    | BinOp (r, LeqOp, Imm x, Imm y) -> (true, Set (r, Imm (if x <= y then 1 else 0)))
    | BinOp (r, LtOp, Imm x , Imm y) -> (true, Set(r, Imm(if x < y then 1 else 0)))
    | BinOp (r, GeqOp, Imm x, Imm y) -> (true, Set(r, Imm(if x >= y then 1 else 0)))
    | BinOp (r, GtOp, Imm x, Imm y) -> (true, Set(r, Imm(if x > y then 1 else 0)))
    | _ -> (false, instr)
  let run instrs =
    let results = List.map foldConstant instrs
    let flags, instrs = List.unzip results
    let isOptimized = List.contains true flags
    (isOptimized, instrs)


module ConstantPropagation =
  let constantVal (rd: RDSet) (reg: Register) =
    // Propagation is sound only when *every* reaching definition of 'reg'
    // assigns the same constant. Counting only 'Set (reg, Imm _)' is wrong:
    // at a join point a non-constant definition (Load/BinOp/Set from reg)
    // may also reach, so we must collect all definitions of 'reg' first.
    let defs =
      Set.filter (fun (i: Instr) ->
        match i with
        | Set (r, _) | Load (r, _) | UnOp (r, _, _) | BinOp (r, _, _, _) -> r = reg
        | _ -> false
      ) rd

    let constOf i =
      match i with
      | Set (_, Imm v) -> Some v
      | _ -> None

    match Set.toList defs with
    | [] -> None
    | head :: tail ->
        let c = constOf head
        if Option.isSome c && List.forall (fun i -> constOf i = c) tail then c
        else None

  let rec propagate (instr: Instr) (rd: RDSet) : Instr =
    match instr with
    | BinOp (reg, operator, operand1, operand2) ->
        let v1 =
          match operand1 with
          | Reg r -> 
              match constantVal rd r with
              | Some v -> Imm v
              | None -> operand1
          | _ -> operand1
        let v2 =
          match operand2 with
          | Reg r -> 
              match constantVal rd r with
              | Some v -> Imm v
              | None -> operand2
          | _ -> operand2
        BinOp (reg, operator, v1, v2)
    | UnOp (reg, op, operand) ->
        let v =
          match operand with
          | Reg r -> 
              match constantVal rd r with
              | Some v -> Imm v
              | None -> operand
          | _ -> operand
        UnOp (reg, op, v)
    | Store (operand, reg) ->
        let v =
          match operand with
          | Reg r -> 
              match constantVal rd r with
              | Some v -> Imm v
              | None -> operand
          | _ -> operand
        Store (v, reg)
    | Set (reg, operand) ->
        let v =
          match operand with
          | Reg r -> 
              match constantVal rd r with
              | Some v -> Imm v
              | None -> operand
          | _ -> operand
        Set (reg, v)
    | Ret operand ->
        let v =
          match operand with
          | Reg r -> 
              match constantVal rd r with
              | Some v -> Imm v
              | None -> operand
          | _ -> operand
        Ret v
    | _ -> instr

  let rec propagateNode (nodes: int list) (rdMap: Map<int, RDSet>) (cfg: CFG) : Instr list =
    match nodes with
    | [] -> []
    | node :: rest ->
        let instr = CFG.getInstr node cfg 
        let rd = Map.find node rdMap
        let optimizedInstr = propagate instr rd 
        optimizedInstr :: propagateNode rest rdMap cfg

  let run instrs =
    let cfg = CFG.make instrs
    let rdMap = RDAnalysis.run cfg
    let nodes = CFG.getAllNodes cfg
    let optimizedInstructions = propagateNode nodes rdMap cfg
    let isOptimized = optimizedInstructions <> instrs
    (isOptimized, optimizedInstructions)

module CopyPropagation = 
  // Write your logic to run copy propagation with RD analysis result.
  let run instrs =
    let cfg = CFG.make instrs
    let aeMap = AEAnalysis.run cfg
    let isOptimized = false
    (isOptimized, instrs)

module CommonSubexpressionElimination =
  // Write your logic to run common subexpression elimination with AE analysis result.
  let run instrs =
    let cfg = CFG.make instrs
    let aeMap = AEAnalysis.run cfg
    let isOptimized = false
    (isOptimized, instrs)

module DeadCodeElimination =
  open LVAnalysis

  // A pure definition of r is dead iff r is not live *after* the node, i.e.
  // r is not in the live-OUT set. (Checking live-IN is wrong: the transfer
  // function removes the defined reg from IN, so it would look dead always.)
  let isLive (instr: Instr) (liveOut: LVSet) : bool =
    match instr with
    | Set (r, _) | UnOp (r, _, _) | BinOp (r, _, _, _) | Load (r, _) -> Set.contains r liveOut
    | _ -> true

  // live-OUT[node] = union of live-IN of its successors.
  let liveOutOf (cfg: CFG) (lvInMap: Map<int, LVSet>) (node: int) : LVSet =
    CFG.getSuccs node cfg
    |> List.fold (fun acc s -> Set.union acc (Map.find s lvInMap)) Set.empty

  // Apply Dead Code Elimination to all nodes in the CFG
  let rec eliminateNodes (cfg: CFG) (lvMap: Map<int, LVSet>) (nodes: int list) : Instr list =
    match nodes with
    | [] -> []
    | node :: rest ->
        let instr = CFG.getInstr node cfg
        let liveOut = liveOutOf cfg lvMap node
        if isLive instr liveOut then instr :: eliminateNodes cfg lvMap rest
        else eliminateNodes cfg lvMap rest

  let run instrs =
    let cfg = CFG.make instrs
    let lvMap = LVAnalysis.run cfg
    let nodes = CFG.getAllNodes cfg
    let optimizedInstrs = eliminateNodes cfg lvMap nodes
    let isOptimized = optimizedInstrs <> instrs
    (isOptimized, optimizedInstrs)
  

module Mem2Reg =

  let promoteM2r (instrs: Instr list) : Instr list =
    let rec processInstrs (instrs: Instr list) (m2r: Map<Register, Register>) : Instr list =
      match instrs with
      | [] -> []
      | instr :: rest ->
          match instr with
          | LocalAlloc (reg, _) ->
              processInstrs rest m2r

          | Store (value, memReg) ->
           
              let newReg = Register ("t_" + memReg.ToString())
              let updatedMap = Map.add memReg newReg m2r
              Set (newReg, value) :: processInstrs rest updatedMap

          | Load (destReg, memReg) ->
              match Map.tryFind memReg m2r with
              | Some reg -> Set (destReg, Reg reg) :: processInstrs rest m2r
              | None -> instr :: processInstrs rest m2r
          | _ ->
              instr :: processInstrs rest m2r

    processInstrs instrs Map.empty

  let run instrs =
    let optimizedInstrs = promoteM2r instrs
    let isOptimized = optimizedInstrs <> instrs
    (isOptimized, optimizedInstrs)


// You will have to run optimization iteratively, as shown below.
let rec optimizeLoop instrs =
  let cp, instrs = ConstantPropagation.run instrs
  let cf, instrs = ConstantFolding.run instrs
 // let cop, instrs = CopyPropagation.run instrs
 // let cse, instrs = CommonSubexpressionElimination.run instrs
  let m2r, instrs = Mem2Reg.run instrs
  let dce, instrs = DeadCodeElimination.run instrs
  if ( cp || cf || m2r || dce) then optimizeLoop instrs else instrs

// Optimize input IR code into faster version.
let run (ir: IRCode) : IRCode =
  let (fname, args, instrs) = ir
  (fname, args, optimizeLoop instrs)
