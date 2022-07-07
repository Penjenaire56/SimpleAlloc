let profiling = ref false
let specs = [ ("--profiling", Arg.Set profiling, " time profiling") ]

let () =
  let alspecs = Arg.align specs in
  let usage = "usage: dune exec -- src/main.exe [options]" in
  Arg.parse alspecs (fun _ -> ()) usage

let profiling = !profiling
