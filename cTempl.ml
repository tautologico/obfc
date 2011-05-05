(*
 * cTempl.ml 
 * 
 * Templates for the C back-end
 * 
 * Andrei Formiga, 2008-04-09
 * 
 *)

let memory_size = 8192

let includes = [
"#include <stdio.h>";
""
]

let memory_dec = [
"unsigned int memory[" ^ (string_of_int memory_size) ^ "];";
""
]

let main = [
"int main(void)";
"{";
"  unsigned int *p = memory;";
""
]

let pre_code = List.concat [includes; memory_dec; main]

let post_code = [
"";
"  printf(\"\\n\");";
"  return 0;";
"}";
""
]

let ident_size = 2
let ident_str = String.make ident_size ' '
