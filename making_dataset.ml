type graph_list = graph list
and graph = node list * edge list
and node = int list
and edge = (int * int) list


let rec read_indicator oFile indicator
= try 
  let line = input_line oFile in
    read_indicator oFile (indicator @ [(int_of_string line) - 1]) 
with 
  End_of_file -> indicator 

let channel = open_in "MUTAG_graph_indicator.txt" 
let indicator = read_indicator channel []
let indicator_len = List.length indicator

let rec couting_graph_num indicator num 
= match indicator with
  | [] -> num
  | h::[] -> num
  | h::t -> if (h = (List.hd t)) then couting_graph_num t num 
            else couting_graph_num t (num+1)

let graph_num = couting_graph_num indicator 1

(*
let making_graph labels_list indicator myA 
*)