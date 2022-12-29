(*entry_of_channel : in_channel -> (int * int)
dictionary_of_file : in_channel -> (int * int) list
*)
(*아 힘들다*)
let entry_of_channel : in_channel -> (int * int)
(*let entry_of_channel ch =*)
= fun ch ->
  let e1 = input_line ch i다
  let e2 = input_line ch in
    (int_of_string e1, int_of_string e2)

let rec dictionary_of_channel : in_channel -> (int * int) list
= fun ch ->
(*let rec dictionary_of_channel ch =  *)
  try
    let e = entry_of_channel ch in
      e :: dictionary_of_channel ch
  with 
    End_of_file -> []

(*dictionary_of_file : string -> (int * int) list*)

let dictionary_of_file filename = 
  let ch = open_in filename in
  let dict = dictionary_of_channel ch in 
    close_in ch;
    dict


    (*
dictionary_to_file : (int * int) -> (int * int) list -> unit

let dictionary_to_file filename graphEdge =
  let ch = open_out filename in 
    dictionary_to_file ch graphEdge;
    close_out ch


let read_file filename =
  let lines = ref [] in
  let ch = open_in filename in
  try
    while true; do
      lines := input_line ch :: !lines
    done; !lines
  with End_of_file ->
    close_in ch;
    List.rev !lines;;

(*let filelines = File.lines_of filename in
Enum.iter ( fun line -> (*Do something with line here*) ) filelines*)

(*https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)

      *)


