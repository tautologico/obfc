(*
 * file.ml
 * 
 * Utility functions for files
 *
 * Andrei Formiga, 2008-04-09
 *
 *)

(* Pretty ugly function, but in OCaml it seems we have no choice *)
let read_all_lines fname =   
  let f = open_in fname in
  let lines = ref [] in
  try
   (
     while true do
       lines := (input_line f) :: !lines
     done;
     !lines
   )
  with End_of_file -> 
   (
    close_in f;
    List.rev !lines
   )

let read_contents fname = 
  let f = open_in fname in
  let len = in_channel_length f in
  let buf = String.create len in
  (
    ignore (input f buf 0 len);
    close_in f;
    buf
  )

let write_all_lines fname lines = 
  let f = open_out fname in
  (
    List.iter (Printf.fprintf f "%s\n") lines;
    close_out f
  )

