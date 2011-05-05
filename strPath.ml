(*
 * strPath.ml
 * 
 * Utility functions over strings and paths
 *
 * Andrei Formiga, 2008-04-09
 *
 *)

let range i j = 
  let rec loop x = 
    if x = j then [] else x :: loop (x + 1) in
  loop i

let str_starts_with s x = 
  let sl, xl = String.length s, String.length x in
  if sl < xl then false 
  else let ins = range 0 xl in
       List.fold_right (&&) (List.map (fun i -> s.[i] = x.[i]) ins) true

let remove_beginning s beg = 
    let sl, bl = (String.length s, String.length beg) in
    String.sub s bl (sl - bl)
    
(** Returns a char list with the characters of string s *) 
let explode s = 
  let sl = String.length s in
  let rec loop i =
    if i = sl then []
    else s.[i] :: (loop (i + 1)) in
  loop 0

let has_extension p = 
  try
    let _ = String.rindex p '.' in
    true
  with Not_found -> false

let remove_extension p = 
  try
    Filename.chop_extension p
  with Invalid_argument _ -> 
    p

(** Changes the extension of path p to ext *)
let change_extension p ext = 
  let dot = String.make 1 '.' in
  try
    let pnoext = Filename.chop_extension p in
    pnoext ^ dot ^ ext
  with Invalid_argument _ ->
    p ^ dot ^ ext


