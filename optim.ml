(*
 * optim.ml
 * 
 * Optimizer for bf programs
 *
 * Andrei Formiga, 2008-04-09
 *
 *)

open AbsSyntax

let coalesce_ops cmds = 
  let coalescing_cons el l = 
    match el,l with
      (Inc i1, Inc i2 :: l') -> Inc (i1+i2) :: l'
    | (Dec d1, Dec d2 :: l') -> Dec (d1+d2) :: l'
    | _ -> el :: l in
  List.fold_right coalescing_cons cmds []

let optimize cmds = 
  coalesce_ops cmds
