(*
 * compiler.ml
 * 
 * Front-end to the bf compiler
 *
 * Andrei Formiga, 2008-04-09
 *
 *)

open StrPath

let usage progname = 
  Printf.printf "Usage: %s [-o output_file] <program>\n" progname

let default_outname iname = 
  remove_extension iname
        
let default_c_name iname = 
  change_extension iname "c"

let process_args args = 
  let optbeg = "-o" in
  if str_starts_with args.(1) optbeg then
    if Array.length args <= 3 then failwith "No input file given"
    else (args.(2), args.(3))
  else
    (default_outname args.(1), args.(1))

let c_compile cname oname = 
  let cmdline = Printf.sprintf "gcc -o %s %s" oname cname in
  let retval = Sys.command cmdline in
  if retval <> 0 then 
    print_endline "Problem compiling, maybe gcc is not in your path"
  else
    ()

let compile_file (oname, iname) = 
  try 
    let contents = File.read_contents iname in
    let cmds = AbsSyntax.parse contents in
    let ccode = CGen.generate_code (Optim.optimize cmds) in
    let cname = default_c_name iname in      
    (
      File.write_all_lines cname ccode;
      c_compile cname oname
    )
  with Sys_error _ -> 
    Printf.printf "Could not find file: %s\n" iname

let main() = 
  if Array.length Sys.argv > 1 then 
    compile_file (process_args Sys.argv)
  else
    usage Sys.argv.(0)


let () = main()



      
