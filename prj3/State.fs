namespace State

open IR

exception RuntimeError of string

// Address of memory space.
type MemAddr = int

// Unlike real memory, let's assume that code uses separate space.
type CodeAddr = int

// Mapping from register name to value (int)
type RegMap = Map<Register,int>

module RegMap =
  let empty: RegMap = Map.empty

  let lookup (reg: Register) (regMap: RegMap): int =
    if Map.containsKey reg regMap then Map.find reg regMap
    else raise (RuntimeError "Undefined register accessed")

  let bind (reg: Register) (v: int) (regMap: RegMap) =
    Map.add reg v regMap

// Mapping from memory address to value (int)
type Memory = int * Map<MemAddr,int>

module Memory =
  // Start from non-zero address value, to distinguish from NULL.
  let empty: Memory = (0x1000, Map.empty)

  let allocate size (mem: Memory): Memory * MemAddr =
    let memBrk, map = mem
    let newMem = (memBrk + size, map)
    (newMem, memBrk)

  let read (addr: MemAddr) (mem: Memory): int =
    let memBrk, map = mem
    if addr < 0 ||  addr >= memBrk || not (Map.containsKey addr map)
    then raise (RuntimeError "Invalid memory access (read)")
    else Map.find addr map

  let update (addr: MemAddr) (v: int) (mem: Memory) : Memory =
    let memBrk, map = mem
    if addr < 0 || addr >= memBrk
    then raise (RuntimeError "Invalid memory access (update)")
    else (memBrk, Map.add addr v map)

// Mapping from code address to IR instruction
type IRMap = Map<CodeAddr,Instr>

module IRMap =
  let empty: IRMap = Map.empty

  let find (addr: CodeAddr) (irMap: IRMap): Instr =
    if Map.containsKey addr irMap then Map.find addr irMap
    else failwith "Invalid code address access"

  let add (addr: CodeAddr) (instr: Instr) (irMap: IRMap) : IRMap =
    Map.add addr instr irMap

// Mapping from label to code address
type LabelMap = Map<Label,CodeAddr>

module LabelMap =
  let empty: LabelMap = Map.empty

  let find (label: Label) (labelMap: LabelMap): CodeAddr =
    if Map.containsKey label labelMap then Map.find label labelMap
    else raise (RuntimeError "Undefined label referred")

  let add (label: Label) (v: CodeAddr) (labelMap: LabelMap) =
    Map.add label v labelMap

// Execution state consists of register map, memory, and program counter.
type State = RegMap * Memory * CodeAddr

type StepResult =
  | Finished of int (* Program (function) successfully returned an integer *)
  | Running of State (* Successful execution of one instruction *)
