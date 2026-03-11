module Translate

open AST
open IR
open Helper

// Symbol table is a mapping from identifier to a pair of register and type.
// Register is recorded here will be containg the address of that variable.
type SymbolTable = Map<Identifier,Register * CType>

// Let's assume the following size for each data type.
let sizeof (ctyp: CType) =
  match ctyp with
  | CInt -> 4
  | CBool -> 1
  | CIntPtr -> 8
  | CBoolPtr -> 8
  | CIntArr n -> 4 * n
  | CBoolArr n -> n

// Find the register that contains pointer to variable 'vname'
let lookupVar (symtab: SymbolTable) (vname: Identifier) : Register =
  let _ = if not (Map.containsKey vname symtab) then failwith "Unbound variable"
  fst (Map.find vname symtab)

let rec transExp (symtab: SymbolTable) (e: Exp) : Register * Instr list =
  match e with
  | Null ->
      let r = createRegName ()
      (r, [Set (r, Imm 0)])
  | Num i ->
      let r = createRegName ()
      (r, [Set (r, Imm i)]) 
  | Boolean b ->
      let r = createRegName()
      if b then (r, [Set (r, Imm 1)]) else (r, [Set (r, Imm 0)])
  | Var vname ->
      let varReg = lookupVar symtab vname // Contains the address of 'vname'
      let r = createRegName ()
      (r, [Load (r, varReg)])
  | Deref vname -> 
      let varReg = lookupVar symtab vname
      let t1 = createRegName()
      let t2 = createRegName()
      (t2, [Load (t1, varReg); Load (t2, t1)])
  | AddrOf vname -> //&vname, 주소 반환
      let varReg = lookupVar symtab vname
      (varReg, [])
  | Arr (arrname, idx) ->
      let varReg = lookupVar symtab arrname //Contains the address of array(base address)
      let (idxReg, instrs) = transExp symtab idx
      let offsetReg = createRegName()
      let arrType = snd (Map.find arrname symtab)
      let size =
          match arrType with
          | CIntArr _ -> 4
          | CBoolArr _ -> 1
      let offsetInst = [BinOp (offsetReg, MulOp, Reg idxReg, Imm size)]
      let addrReg = createRegName()
      let valueReg = createRegName()
      let addrInst = [BinOp (addrReg, AddOp, Reg varReg, Reg offsetReg)]
      let valueInst = [Load (valueReg, addrReg)]
      (valueReg, instrs @ offsetInst @ addrInst @ valueInst)
  | Neg e ->
      let (r1, instrs') = transExp symtab e
      let r = createRegName()
      (r, instrs' @ [UnOp (r, NegOp, Reg r1)])
  | Add (e1, e2) ->
      let (t1, instrs1) = transExp symtab e1
      let (t2, instrs2) = transExp symtab e2
      let r = createRegName ()
      (r, instrs1 @ instrs2 @ [BinOp (r, AddOp, Reg t1, Reg t2)])
  | Sub (e1, e2) ->
      let (t1, instrs1) = transExp symtab e1
      let (t2, instrs2) = transExp symtab e2
      let r = createRegName ()
      (r, instrs1 @ instrs2 @ [BinOp (r, SubOp, Reg t1, Reg t2)])
  | Mul (e1, e2) ->
      let (t1, instrs1) = transExp symtab e1
      let (t2, instrs2) = transExp symtab e2
      let r = createRegName ()
      (r, instrs1 @ instrs2 @ [BinOp (r, MulOp, Reg t1, Reg t2)])
  | Div (e1, e2) ->
      let (t1, instrs1) = transExp symtab e1
      let (t2, instrs2) = transExp symtab e2
      let r = createRegName ()
      (r, instrs1 @ instrs2 @ [BinOp (r, DivOp, Reg t1, Reg t2)])
  | Equal (e1, e2) ->
      let (t1, instrs1) = transExp symtab e1
      let (t2, instrs2) = transExp symtab e2
      let r = createRegName ()
      (r, instrs1 @ instrs2 @ [BinOp (r, EqOp, Reg t1, Reg t2)])
  | NotEq (e1, e2) ->
      let (t1, instrs1) = transExp symtab e1
      let (t2, instrs2) = transExp symtab e2
      let r = createRegName ()
      (r, instrs1 @ instrs2 @ [BinOp (r, NeqOp, Reg t1, Reg t2)])
  | LessEq (e1, e2) ->
      let (t1, instrs1) = transExp symtab e1
      let (t2, instrs2) = transExp symtab e2
      let r = createRegName ()
      (r, instrs1 @ instrs2 @ [BinOp (r, LeqOp, Reg t1, Reg t2)])
  | LessThan (e1, e2) ->
      let (t1, instrs1) = transExp symtab e1
      let (t2, instrs2) = transExp symtab e2
      let r = createRegName ()
      (r, instrs1 @ instrs2 @ [BinOp (r, LtOp, Reg t1, Reg t2)])
  | GreaterEq (e1, e2) ->
      let (t1, instrs1) = transExp symtab e1
      let (t2, instrs2) = transExp symtab e2
      let r = createRegName ()
      (r, instrs1 @ instrs2 @ [BinOp (r, GeqOp, Reg t1, Reg t2)])
  | GreaterThan (e1, e2) ->
      let (t1, instrs1) = transExp symtab e1
      let (t2, instrs2) = transExp symtab e2
      let r = createRegName ()
      (r, instrs1 @ instrs2 @ [BinOp (r, GtOp, Reg t1, Reg t2)])
  | And (e1, e2) ->
      let (t1, instrs1) = transExp symtab e1
      let lFalse = createLabel ()
      let lEnd = createLabel ()
      let t2, instrs2 = transExp symtab e2
      let res = createRegName ()
      let checkInst = [GotoIfNot (Reg t1, lFalse)]
      let resInst = [Set (res, Reg t2); Goto lEnd; Label lFalse; Set (res, Imm 0); Label lEnd]
      (res, instrs1 @ checkInst @ instrs2 @ resInst)
  | Or (e1, e2) ->
      let (t1, instrs1) = transExp symtab e1
      let lTrue = createLabel ()
      let lEnd = createLabel ()
      let t2, instrs2 = transExp symtab e2
      let res = createRegName ()
      let checkInst = [GotoIf (Reg t1, lTrue)]
      let resInst = [Set (res, Reg t2); Goto lEnd; Label lTrue; Set (res, Imm 1); Label lEnd]
      (res, instrs1 @ checkInst @ instrs2 @ resInst)
  | Not e ->
      let (r1, instrs) = transExp symtab e
      let r = createRegName()
      (r, instrs @ [UnOp (r, NotOp, Reg r1)])

let rec transStmt (symtab: SymbolTable) stmt : SymbolTable * Instr list =
  match stmt with
  | Declare (_, typ, vname) ->
      let r = createRegName ()
      let size = sizeof typ
      let symtab = Map.add vname (r, typ) symtab
      (symtab, [LocalAlloc (r, size)])
  | Define (_, typ, vname, e) ->
      let r = createRegName()
      let size = sizeof typ
      let symtab = Map.add vname (r, typ) symtab
      let (t1, instrs) = transExp symtab e
      (symtab, instrs @ [LocalAlloc (r, size); Store (Reg t1, r)])
  | Assign (_, vname, e) ->
      let (t1, value) = transExp symtab e
      let varReg = lookupVar symtab vname
      (symtab, value @ [Store (Reg t1, varReg)])
  | PtrUpdate (_, vname, e) -> 
      let varReg = lookupVar symtab vname 
      let (t1, value) = transExp symtab e //10, load value
      let addrReg = createRegName()
      let addrInst = [Load (addrReg, varReg)]
      (symtab, value @ addrInst @ [Store (Reg t1, addrReg)])

  | ArrUpdate (_, vname, e1, e2) -> //arr[1] = 2
      let arrReg = lookupVar symtab vname //arr, base address
      let (idxReg, instrs1) = transExp symtab e1 //index
      let (valReg, instrs2) = transExp symtab e2 //value
      let offsetReg = createRegName() //base address + index address
      let faddrReg = createRegName() //result address
      let arrType = snd (Map.find vname symtab)
      let size =
          match arrType with
          | CIntArr _ -> 4
          | CBoolArr _ -> 1
      let offsetInst = [BinOp (offsetReg, MulOp, Reg idxReg, Imm size)]
      let faddrInst = [BinOp (faddrReg, AddOp, Reg arrReg, Reg offsetReg)]
      (symtab, instrs1 @ instrs2 @ offsetInst @ 
               faddrInst @ [Store (Reg valReg, faddrReg)])
  | Return (_, e) -> //return 0;
      let (r, instrs') = transExp symtab e
      (symtab, instrs' @ [Ret (Reg r)])
  | If (_, e, s1, s2) -> //if (E) {S} else {S}
      let (condReg, condInst) = transExp symtab e
      let lTrue = createLabel() // L1
      let lfin = createLabel() // L_fin

      let checkInst = [GotoIf(Reg condReg,lTrue)]
      let instrs2 = transStmts symtab s2
      let elseInst = [Goto lfin; Label lTrue]
      let instrs1 = transStmts symtab s1
      let finInst = [Label lfin]
      (symtab, condInst @ checkInst @ instrs2 @ elseInst @ instrs1 @ finInst)
  | While (_, cond, stmts) ->
      let loop = createLabel () 
      let lstmt = createLabel ()  
      let lfin = createLabel ()   
  
      let (condReg, condInstrs) = transExp symtab cond
      let stmtInstrs = transStmts symtab stmts
      (symtab,
       [Label loop] @ condInstrs @ [GotoIfNot (Reg condReg, lfin)] @
       [Label lstmt] @ stmtInstrs @ [Goto loop; Label lfin])


and transStmts symtab stmts: Instr list =
  match stmts with
  | [] -> []
  | headStmt :: tailStmts ->
      let symtab, instrs = transStmt symtab headStmt
      instrs @ transStmts symtab tailStmts

// This code allocates memory for each argument and records information to the
// symbol table. Note that argument can be handled similarly to local variable.
let rec transArgs accSymTab accInstrs args =
  match args with
  | [] -> accSymTab, accInstrs
  | headArg :: tailArgs ->
      // In our IR model, register 'argName' is already defined at the entry.
      let (argTyp, argName) = headArg
      let r = createRegName ()
      let size = sizeof argTyp
      // From now on, we can use 'r' as a pointer to access 'argName'.
      let accSymTab = Map.add argName (r, argTyp) accSymTab
      let accInstrs = [LocalAlloc (r, size); Store (Reg argName, r)] @ accInstrs
      transArgs accSymTab accInstrs tailArgs

// Translate input program into IR code.
let run (prog: Program) : IRCode =
  let (_, fname, args, stmts) = prog
  let argRegs = List.map snd args
  let symtab, argInstrs = transArgs Map.empty [] args
  let bodyInstrs = transStmts symtab stmts
  (fname, argRegs, argInstrs @ bodyInstrs)
