module Executor

open IR
open State

let evalOperand oprnd regMap =
  match oprnd with
  | Imm i -> i
  | Reg r -> RegMap.lookup r regMap

let unOp opTyp i =
  match opTyp with
  | NegOp -> - i
  | NotOp -> if i = 0 then 1 else 0

let binOp opTyp i1 i2 =
  match opTyp with
  | AddOp -> i1 + i2
  | SubOp -> i1 - i2
  | MulOp -> i1 * i2
  | DivOp -> if i2 <> 0 then i1 / i2
             else raise (RuntimeError "Div-by-zero")
  | EqOp -> if i1 = i2 then 1 else 0
  | NeqOp -> if i1 <> i2 then 1 else 0
  | LeqOp -> if i1 <= i2 then 1 else 0
  | LtOp -> if i1 < i2 then 1 else 0
  | GeqOp -> if i1 >= i2 then 1 else 0
  | GtOp -> if i1 > i2 then 1 else 0

let execInstr labelMap instr regMap mem pc =
  match instr with
  | Set (r, o) ->
      Running (RegMap.bind r (evalOperand o regMap) regMap, mem, pc + 1)
  | LocalAlloc (r, n) ->
      let mem, addr = Memory.allocate n mem
      Running (RegMap.bind r addr regMap, mem, pc + 1)
  | UnOp (r, opTyp, oprnd) ->
      let v = evalOperand oprnd regMap
      Running (RegMap.bind r (unOp opTyp v) regMap, mem, pc + 1)
  | BinOp (r, opTyp, oprnd1, oprnd2) ->
      let v1 = evalOperand oprnd1 regMap
      let v2 = evalOperand oprnd2 regMap
      Running (RegMap.bind r (binOp opTyp v1 v2) regMap, mem, pc + 1)
  | Load (r1, r2) ->
      let addr = RegMap.lookup r2 regMap
      let v = Memory.read addr mem
      Running (RegMap.bind r1 v regMap, mem, pc + 1)
  | Store (oprnd, r) ->
      let v = evalOperand oprnd regMap
      let addr = RegMap.lookup r regMap
      Running (regMap, Memory.update addr v mem, pc + 1)
  | Goto l ->
      Running (regMap, mem, LabelMap.find l labelMap)
  | GotoIf (o, l) ->
      let v = evalOperand o regMap
      let jmpAddr = LabelMap.find l labelMap
      if v <> 0 then Running (regMap, mem, jmpAddr)
      else Running (regMap, mem, pc + 1)
  | GotoIfNot (o, l) ->
      let v = evalOperand o regMap
      let jmpAddr = LabelMap.find l labelMap
      if v = 0 then Running (regMap, mem, jmpAddr)
      else Running (regMap, mem, pc + 1)
  | Label _ -> Running (regMap, mem, pc + 1)
  | Ret oprnd -> Finished (evalOperand oprnd regMap)

let rec step irMap labelMap regMap mem pc =
  let instr = IRMap.find pc irMap in
  match execInstr labelMap instr regMap mem pc with
  | Finished v -> printfn "%d" v
  | Running (regMap', mem', pc') -> step irMap labelMap regMap' mem' pc'

let rec genCodeMap instrs (irMap, labelMap) idx =
  match instrs with
  | [] -> (irMap, labelMap)
  | headInstr :: tailInstrs ->
      let irMap = IRMap.add idx headInstr irMap in
      let labelMap =
        match headInstr with
        | Label l -> LabelMap.add l idx labelMap
        | _ -> labelMap
      genCodeMap tailInstrs (irMap, labelMap) (idx + 1)

let rec initializeRegs argRegs argVals regMap =
  match argRegs, argVals with
  | [], [] -> regMap
  | [], _ | _, [] -> failwith "Invalid number of argument provided"
  | headReg :: tailRegs, headVal :: tailVals ->
      let regMap = RegMap.bind headReg headVal regMap in
      initializeRegs tailRegs tailVals regMap

let run (ir: IRCode) (argVals: int list): unit =
  let (_, argRegs, instrs) = ir in
  if List.isEmpty instrs then failwith "Cannot execute empty instruction list"
  let irMap, labelMap = genCodeMap instrs (IRMap.empty, LabelMap.empty) 0
  let regMap = initializeRegs argRegs argVals RegMap.empty
  step irMap labelMap regMap Memory.empty 0
