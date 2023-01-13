type graph_list = graph list
and graph = node list * edge list
and node = int list
and edge = (int * int) list


exception InputError

let rec read_feature oFile features 
= try 
  let line = input_line oFile in
    read_feature oFile (features @ [[Float.of_int(int_of_string line)]]) 
with 
  End_of_file -> features 


let rec read_graph_labels oFile labels 
= try 
  let line = input_line oFile in
    let l = int_of_string line in
    if l == 1 then read_graph_labels oFile (labels @ [1]) 
    else read_graph_labels oFile (labels @ [0]) 
with 
  End_of_file -> labels 


let rec read_indicator oFile indicator
= try 
  let line = input_line oFile in
    read_indicator oFile (indicator @ [(int_of_string line) - 1]) 
with 
  End_of_file -> indicator 


let make_tuple oFileLine 
= let rawline = input_line oFileLine in (*raw line 형태는 'int, int'*)
  let sep = ',' in
  let _str = String.split_on_char sep rawline in
  match _str with
  | [] -> raise InputError
  | h::t -> 
    match t with
      | [] -> raise InputError
      | h2::t -> (int_of_string h, int_of_string (String.trim h2))

let rec read_graph_edge oFile edges
= try
  let line = make_tuple oFile in
    read_graph_edge oFile (edges @ [line])
with 
  End_of_file -> edges


let channel = open_in "MUTAG_node_labels.txt" 
let x_node = read_feature channel []
let nodes_len = List.length x_node

let channel = open_in "MUTAG_edge_labels.txt" 
let x_edge = read_feature channel []
let edges_len = List.length x_edge


let channel = open_in "MUTAG_graph_indicator.txt" 
let indicator = read_indicator channel []
let indicator_len = List.length indicator


let channel = open_in "MUTAG_graph_labels.txt" 
let labels = read_graph_labels channel []
let labels_len = List.length labels 

let channel = open_in "MUTAG_A.txt"
let myA = read_graph_edge channel []
let myA_len = List.length myA


let rec couting_graph_num indicator num 
= match indicator with
  | [] -> num
  | h::[] -> num
  | h::t -> if (h = (List.hd t)) then couting_graph_num t num 
            else couting_graph_num t (num+1)

let graph_num = couting_graph_num indicator 1

let rec make_node first final list
= if(first = final) then list@[first]
else make_node (first+1) final (list@[first])

let rec make_edge (first:int) (final:int) (myA:(int*int) list) list
= match myA with 
  | [] -> list
  | (e1,e2)::t -> if( (first<e1) && (final >= e1) && (first<e2) && (final >= e2) ) then make_edge first final t list@[(e1, e2)]
  else make_edge first final t list

let make_ num myA graph_list cnt lcnt
= let n = make_node (lcnt+1) cnt [] in
  let e = make_edge lcnt cnt myA [] in
  let newGraph = (n,e) in graph_list@[newGraph]

let rec making_graph_edge indicator myA graph_list cnt lcnt
= match indicator with
  | [] -> graph_list
  | h::[] -> let graph_list = make_ h myA graph_list cnt lcnt 
              in graph_list
  | h::t -> if (h = (List.hd t)) then making_graph_edge t myA graph_list (cnt+1) lcnt 
            else 
              let graph_list = make_ h myA graph_list cnt lcnt
              in making_graph_edge t myA graph_list (cnt+1) cnt

let a = making_graph_edge indicator myA [] 1 0

            (*
let making_graph labels_list indicator myA 
*)