module DFA

open IR
open CFG


// You can represent a 'reaching definition' element with an instruction.
type RDSet = Set<Instr>
type LVSet = Set<Register>
type AESet = Set<Instr>

module RDAnalysis = //for constant propagation -> 
  // Write your logic to compute reaching definition set for each CFG node.
  // using by RDAnalysis.run cfg
  let transfer (rd_in : RDSet) (instr:Instr) : RDSet =
    match instr with
    | Set (r, _) | Load (r, _) | UnOp (r, _, _) | BinOp (r, _, _, _) ->
      let rd_out = Set.filter (fun i ->
                               match i with
                               | Set (reg, _) | Load (reg, _) | UnOp (reg, _, _) | BinOp (reg, _, _, _) -> reg <> r
                               | _ -> true) rd_in
      Set.add instr rd_out
    | _ -> rd_in

  let run (cfg: CFG) : Map<int, RDSet> =
    let nodes = CFG.getAllNodes cfg // Get all node IDs
    let initialOutSet = Map.ofSeq (Seq.map (fun node -> node, Set.empty) nodes)
    let initialInSet = Map.ofSeq (Seq.map (fun node -> node, Set.empty) nodes)

    let rec iter (inSet: Map<int, RDSet>, outSet: Map<int, RDSet>) : Map<int, RDSet> =
      let (updatedInSet, updatedOutSet, isChanged) =
        List.fold (fun (inAcc, outAcc, changed) node ->
          let preds = CFG.getPreds node cfg
          let rd_in =
            if List.isEmpty preds then Set.empty
            else List.fold (fun acc pred -> Set.union acc (Map.find pred outAcc)) Set.empty preds

          let instr = CFG.getInstr node cfg
          let new_rd_out = transfer rd_in instr

          let outChanged = 
            match Map.tryFind node outAcc with
            | Some oldOut -> oldOut <> new_rd_out
            | None -> true

          let inAcc = Map.add node rd_in inAcc
          let outAcc = Map.add node new_rd_out outAcc
          (inAcc, outAcc, changed || outChanged)
        ) (inSet, outSet, false) nodes

      if isChanged then iter (updatedInSet, updatedOutSet)
      else updatedInSet

    iter (initialInSet, initialOutSet)

 //AE에서 store를 만나면 update => load에 대한 IR은 available X
module AEAnalysis =
  // Write your logic to compute available expression set for each CFG node.
  let run (cfg: CFG) : Map<int,AESet> = //int -> node id, AESet -> available expression set
    Map.empty

module LVAnalysis =
  // Write your logic to compute liveness set for each CFG node.
  let def (instr: Instr) : Set<Register> =
    match instr with
    | Set (r, _) | Load (r, _) | UnOp (r, _, _) | BinOp (r, _, _, _) -> Set.singleton r
    | _ -> Set.empty

  let usedreg (instr : Instr) : Set<Register> =
    match instr with
    | Set (_, Reg r) | UnOp (_, _, Reg r) | Ret(Reg r) | Store(Reg r, _)-> Set.singleton r
    | BinOp (_, _, Reg r1, Reg r2)  -> Set.ofList [r1;r2]
    | Load (_, r) -> Set.singleton r
    | _ -> Set.empty

  let transfer (lv_out : LVSet) (instr: Instr) : LVSet =
    let defreg = def instr
    let usedregs = usedreg instr
    Set.union (Set.difference lv_out defreg) usedregs

  let run (cfg: CFG) : Map<int,LVSet> = //int -> node id, LVSet -> liveness register set
    let nodes = CFG.getAllNodes cfg
    let initialOutSet = Map.ofSeq (Seq.map (fun node -> node, Set.empty) nodes)
    let initialInSet = Map.ofSeq (Seq.map (fun node -> node, Set.empty) nodes)

    let rec iter (inSet: Map<int, LVSet>, outSet: Map<int, LVSet>) : Map<int, LVSet> = 
      let (updatedInSet, updatedOutSet, isChanged) =
        List.fold (fun (inAcc, outAcc, changed) node ->
          let succs = CFG.getSuccs node cfg
          let lv_out =
            if List.isEmpty succs then Set.empty
            else List.fold (fun acc succ -> Set.union acc (Map.find succ inAcc)) Set.empty succs

          let instr = CFG.getInstr node cfg
          let new_lv_in = transfer lv_out instr

          let outChanged = 
            match Map.tryFind node outAcc with
            | Some oldin -> oldin <> new_lv_in
            | None -> true
            
          let inAcc = Map.add node new_lv_in inAcc
          let outAcc = Map.add node lv_out outAcc
          (inAcc, outAcc, changed || outChanged)
        ) (inSet, outSet, false) nodes

      if isChanged then iter (updatedInSet, updatedOutSet)
      else updatedInSet

    iter (initialInSet, initialOutSet)