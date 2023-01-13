(*1차 ! : MUTAG_A.txt 읽어보기
  명령어 : dictionary_of_file "MUTAG_A.txt";;
  결과값 : string -> (int * int) list = <fun> *)

let mdict = ref []
let extend_dict (k) = mdict := k::!mdict

let entry_of_channel : in_channel -> (int * int)
= fun ch ->
  let e1 = input_line ch in
  let e2 = input_line ch in
    (int_of_string e1, int_of_string e2)

let rec dictionary_of_channel : in_channel -> (int * int) list
= fun ch ->
  try
    let e = entry_of_channel ch in
      e::(dictionary_of_channel ch)
  with 
    End_of_file -> []
  
(*dictionary_of_file : string -> (int * int) list*)

let dictionary_of_file filename
= fun filename -> 
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

    (*let filelines = File.lines_of filename in
Enum.iter ( fun line -> (*Do something with line here*) ) filelines*)

(*https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)

*)

(* 두 번째 시도 *)
let read_file filename
= fun filename ->
  let graphEdgeList = ref [] in
  let entry = open_in filename in
  try
    while true; do
      graphEdgeList := entry_of_channel entry :: !graphEdgeList
    done; !graphEdgeList
  with End_of_file ->
    close_in entry;
    List.rev !graphEdgeList

(* 세 번째 시도 *)
let read_lines name
= fun name ->
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


(* couting example *)
let channel_statistics in_channel
= let lines = ref 0 in
try
  while true do
    let line = input_line in_channel in
    lines := !lines +1
  done
with
End_of_file ->
  print_string "There were ";
  print_int !lines;
  print_string " lines.";
  print_newline ()

let file_staticsitcs name = 
  let channel = open_in name in
  try
    channel_statistics channel;
    close_in channel
  with
  _ -> close_in channel


(*time to mix*)
let rec print_tuples list 
= match list with
  | [] -> ()
  | (e1, e2)::t -> Printf.printf "%d, %d" e1 e2; print_newline (); print_tuples t

exception InputError

let make_tuple oFileLine 
= let rawline = input_line oFileLine in
  let sep = ',' in
  let _str = String.split_on_char sep rawline in
  match _str with
  | h::t -> 
    match t with
    | h2::t -> (int_of_string h, int_of_string (String.trim h2))
    | h2::[] -> (int_of_string h, int_of_string (String.trim h2))
    | _ -> raise(Failure ("Variable is not included in environment"))
  | h::[] -> raise(Failure ("Variable is not included in environment"))
  | _ -> raise(Failure ("Variable is not included in environment"))

let read_graphEdge oFile
= let lines = ref 0 in
  let edgeList = ref [] in
  try
    while true do
      let line = make_tuple oFile in
      lines := !lines +1;
      edgeList := ( line :: !edgeList )
    done 
  with 
  End_of_file ->
    print_string "There were ";
    print_int !lines;
    print_string " lines.";
    print_newline ();
    print_tuples !edgeList;
    print_newline ()

let file_graphEdge filename
= let oFile = open_in filename in
  try
    read_graphEdge oFile;
    close_in oFile;
  with
  _ -> close_in oFile;