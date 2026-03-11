namespace CFG

open IR

// Mapping from node ID to instruction
type InstrMap = Map<int,Instr>

// Mapping from label string to its corresponding node ID
type LabelMap = Map<string,int>

// Mapping from node ID to its edges (destination nodes)
type EdgeMap = Map<int,int list>

// Triple of PC map, successor map, and predecessor map
type CFG = InstrMap * EdgeMap * EdgeMap

module CFG =

  // Scan instructions to construct instruction map and label map, while
  // assigning a unique ID to each node.
  let rec private scanInstrs instrs idx (instrMap, labelMap) =
    match instrs with
    | [] -> (instrMap, labelMap)
    | headInstr :: tailInstrs ->
      let instrMap = Map.add idx headInstr instrMap
      let labelMap =
        match headInstr with
        | Label l -> Map.add l idx labelMap
        | _ -> labelMap
      scanInstrs tailInstrs (idx + 1) (instrMap, labelMap)

  let rec private updatePreds curNodeID succNodes predMap =
    let folder accPredMap succ =
      match Map.tryFind succ accPredMap with
      | None -> Map.add succ [curNodeID] accPredMap
      | Some oldPreds -> Map.add succ (curNodeID :: oldPreds) accPredMap
    List.fold folder predMap succNodes

  let rec private findEdges labelMap instrs idx (succMap, predMap) =
    match instrs with
    | [] -> (succMap, predMap)
    | headInstr :: tailInstrs ->
      let succs =
        match headInstr with
        | Ret _ -> []
        | Goto l -> [Map.find l labelMap]
        | GotoIf (_, l) | GotoIfNot(_, l) ->
          if List.isEmpty tailInstrs then [Map.find l labelMap]
          else [Map.find l labelMap; idx + 1]
        | _ -> if List.isEmpty tailInstrs then [] else [idx + 1]
      let succMap = Map.add idx succs succMap
      let predMap = updatePreds idx succs predMap
      findEdges labelMap tailInstrs (idx + 1) (succMap, predMap)

  // Construct a CFG from the provided instruction list.
  let make (instrs: Instr list): CFG =
    let instrMap, labelMap = scanInstrs instrs 0 (Map.empty, Map.empty)
    let succMap, predMap = findEdges labelMap instrs 0 (Map.empty, Map.empty)
    (instrMap, succMap, predMap)

  // Return the list of all the node IDs in the CFG.
  let getAllNodes (cfg: CFG): int list =
    let (instrMap, _, _) = cfg
    List.map fst (Map.toList instrMap)

  // Get the corresponding instruction for the provided node ID.
  let getInstr (nodeID: int) (cfg: CFG): Instr =
    let (instrMap, _, _) = cfg
    match Map.tryFind nodeID instrMap with
    | None -> failwith "Invalid node ID provided (maybe wrong use of APIs)"
    | Some instr -> instr

  // Get the successor nodes of the provided node ID.
  let getSuccs (nodeID: int) (cfg: CFG): int list =
    let (_, succMap, _) = cfg
    match Map.tryFind nodeID succMap with
    | None -> []
    | Some succs -> succs

  // Get the predecessor nodes of the provided node ID.
  let getPreds (nodeID: int) (cfg: CFG): int list =
    let (_, _, predMap) = cfg
    match Map.tryFind nodeID predMap with
    | None -> []
    | Some preds -> preds
