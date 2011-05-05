(*
 * semAnl.ml
 * 
 * Semantic analysis for bf programs
 * 
 * Andrei Formiga, 2008-04-09
 * 
 *)

open AbsSyntax

let decorate_loops cmds =
  let popstk stk =
    match stk with
      []        -> failwith "decorate_loops: unmatched ]"
    | n :: stk' -> (n, stk') in
  let rec loop n stk cmds =
    match cmds with
      [] -> if stk <> [] then failwith "decorate_loops: unmatched ["
            else []
    | (BegLoop 0) :: cmds' -> (BegLoop n) :: loop (n+1) (n::stk) cmds'
    | (BegLoop _) :: cmds' -> failwith "decorate_loops: found unexpected BegLoop <> 0"
    | (EndLoop 0) :: cmds' -> let l, stk' = popstk stk in (EndLoop l) :: loop n stk' cmds'
    | (EndLoop _) :: cmds' -> failwith "decorate_loops: found unexpected EndLoop <> 0"
    | c :: cmds' -> c :: loop n stk cmds' in
  loop 0 [] cmds
