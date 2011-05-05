(*
 * cGen.ml 
 * 
 * C back-end for the bf compiler
 * 
 * Andrei Formiga, 2008-04-09
 * 
 *)

open AbsSyntax

let putparam beg n = 
  beg ^ (string_of_int n) ^ ";"

(* TODO: identation *)
let cmd_repr cmd = 
  match cmd with
    MoveRight -> "p++;"
  | MoveLeft -> "p--;"
  | Inc n -> putparam "*p += " n
  | Dec n -> putparam "*p -= " n
  | Read -> "*p = getchar();"
  | Write -> "putchar(*p);"
  | BegLoop n -> "while (*p) {"
  | EndLoop n -> "}"

let cmd_ident cmds = 
  let calc_ident cmd (i, l) = 
    match cmd with
      BegLoop n -> (i-1, (i-1) :: l)
    | EndLoop n -> (i+1, i :: l)
    | _ -> (i, i :: l) in
  let _, idents = List.fold_right calc_ident cmds (1, []) in
  List.combine cmds idents


let identation n = 
  String.make (CTempl.ident_size * n) ' '

let code cmds = 
  let str (cmd, ident) = (identation ident) ^ (cmd_repr cmd) in
  List.map str (cmd_ident cmds)
    
let generate_code cmds = 
  List.concat [CTempl.pre_code; code cmds; CTempl.post_code]
