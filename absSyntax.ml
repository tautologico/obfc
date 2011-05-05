(*
 * absSyntax.ml
 * 
 * Abstract syntax for bf programs
 *
 * Andrei Formiga, 2008-04-09
 *
 *)


open StrPath

type cmd = MoveRight | MoveLeft | Inc of int | Dec of int 
         | Read | Write | BegLoop of int | EndLoop of int

module CharMap = Map.Make(Char)

let command_map_ls = 
  [('>', MoveRight); 
   ('<', MoveLeft);
   ('+', Inc 1);
   ('-', Dec 1);
   (',', Read);
   ('.', Write);
   ('[', BegLoop 0);
   (']', EndLoop 0)]

let command_map = 
  List.fold_right (fun (c, v) m -> CharMap.add c v m) command_map_ls CharMap.empty

let is_comment ch = 
  CharMap.mem ch command_map
                     
let process_char ch = 
  CharMap.find ch command_map

let parse s = 
  List.map process_char (List.filter is_comment (explode s))

