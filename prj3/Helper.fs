module Helper

let regNum = ref 0

let labelNum = ref 0

// Create a fresh and unique register name. The name starts with prefix '%', so
// it will never conflict with the name of function arguments.
let createRegName () =
  let reg = sprintf "%%r%d" !regNum
  let _ = regNum := !regNum + 1
  reg

// Create a fresh and unique label name. The name starts with 'L'.
let createLabel () =
  let label = sprintf "L%d" !labelNum
  let _ = labelNum := !labelNum + 1
  label
