open FSharp.Text.Lexing
open AST
open IR

let checkArgValid argv =
  let argNum = Array.length argv
  argNum <> 0 &&
  match argv[0] with
  | "print-ir" -> argNum = 2
  | "run-ir" -> argNum = 3
  | "print-opt" -> argNum = 2
  | "run-opt" -> argNum = 3
  | _ -> false

let usage () =
  let _ = printfn "<Usage>"
  let _ = printfn "[*] ./Optimize print-ir <source file>"
  let _ = printfn "[*] ./Optimize run-ir <source file> <input file>"
  let _ = printfn "[*] ./Optimize print-opt <source file>"
  let _ = printfn "[*] ./Optimize run-opt <source file> <input file>"
  exit 1

let lex (file: string): LexBuffer<_> =
  try
    let streamReader = new System.IO.StreamReader(file)
    let lexbuf = LexBuffer<_>.FromTextReader streamReader
    lexbuf.EndPos <- { lexbuf.EndPos with pos_lnum = 1 }
    lexbuf
  with :? System.IO.IOException ->
    printfn "[*] Failed to open file '%s'" file
    exit 1

let parse (lexbuf: LexBuffer<_>) : Program =
  try Parser.prog Lexer.token lexbuf
  with _ ->
    printfn "[*] Parsing error at line %d" (lexbuf.EndPos.Line)
    exit 1

let strToVal s =
  match s with
  | "true" -> 1
  | "false" -> 0
  | _ -> try int s with _ -> failwith "Invalid argument value"

// Read each line in 'filepath' and use it as arguments for function.
let runWithInputs irCode filepath =
  let lines = System.IO.File.ReadAllLines filepath
  Array.iter (fun (s: string) ->
    let args = Array.map strToVal (s.Trim().Split [|' '|])
    Executor.run irCode (Array.toList args)
  ) lines

[<EntryPoint>]
let main argv =
  if not (checkArgValid argv) then usage ()
  let lexbuf = lex argv[1]
  let prog = parse lexbuf
  match argv[0] with
  | "print-ir" -> printfn "%s" (IRCode.toStr (Translate.run prog))
  | "run-ir" -> runWithInputs (Translate.run prog) argv[2]
  | "print-opt" ->
      let optimizedIR = Optimize.run (Translate.run prog)
      printfn "%s" (IRCode.toStr optimizedIR)
  | "run-opt" ->
      let optimizedIR = Optimize.run (Translate.run prog)
      runWithInputs optimizedIR argv[2]
  | _ -> failwith "Unreachable"
  0
