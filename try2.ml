(*graph_to_edges = {}
graph_to_nodes = {}
node_to_graph = {}*)

let graph_to_edges = [||]
let graph_to_nodes = [||]
let node_to_graph = [||]

(*with open("MUTAG/MUTAG_graph_indicator.txt") as file:
  i = 0 
  for line in file.readlines():
    graph_idx = line.strip()
    idx = int(graph_idx) - 1
    node_to_graph[i] = idx
    if not idx in graph_to_nodes:
      graph_to_nodes[idx] = []
    graph_to_nodes[idx].append(i)
    graph_to_edges[idx] = []
    i = i+1 *)

let read_graph_indicator oFile
= let i = ref 0 in
  try while true do
    let line = input_line oFile in
    let graph_idx = int_of_string line in
    let idx = graph_idx -1 in      
    if (Array.mem idx graph_to_nodes = false) then Array.set graph_to_nodes (idx) []
    else ();
    Array.set graph_to_nodes (idx) [i];
    i := !i + 1
  done
  with
    End_of_file -> ()
    
let file_graph_indicator filename
= let oFile = open_in filename in
  try
    read_graph_indicator oFile;
    close_in oFile;
  with
  _ -> close_in oFile

  (*
A = []
with open("MUTAG/MUTAG_A.txt") as file:
  i = 0
  j = 0
  for line in file.readlines():
    edge = line.strip().split(', ')
    fr_node = int(edge[0]) - 1
    to_node = int(edge[1]) - 1
    A.append((fr_node, to_node))
    
    if fr_node in graph_to_nodes[j]:
      graph_to_edges[j].append(i)
    elif fr_node in graph_to_nodes[j+1]:
      j = j + 1
      graph_to_edges[j] = [i]
    i = i + 1 *)

exception InputError

let make_tuple oFileLine 
= let rawline = input_line oFileLine in (*raw line 형태는 'int, int'*)
  let sep = ',' in
  let _str = String.split_on_char sep rawline in
  match _str with
  | h::t -> 
    match t with
      | h2::t -> (int_of_string h, int_of_string (String.trim h2))
      | h2::[] -> (int_of_string h, int_of_string (String.trim h2))
      | _ -> raise InputError
  | h::[] -> raise InputError
  | _ -> raise InputError
  
  (*The function to extend memory for tuple list - MUTAG_A.txt*)
let read_graphEdge oFile
= let lines = ref 0 in (*to check the number of lines*)
  try
    while true do
      let line = make_tuple oFile in
      lines := !lines +1; (*to check the number of lines*)
      let a = [|line|] in
      let graph_to_edges = Array.append graph_to_edges a in ()
    done
  with 
  End_of_file -> ()
  
  (*명령어 : file_graphEdge "MUTAG_A.txt";;*)   
let file_graphEdge filename 
= let oFile = open_in filename in
  try
    read_graphEdge oFile;
    close_in oFile;
  with
  _ -> close_in oFile

    (*
graph_to_label = {}
with open("MUTAG/MUTAG_graph_labels.txt") as file:
  i = 0 
  for line in file.readlines():
    label = line.strip()
    graph_to_label[i] = int(label)
    i = i+1
*)

let graph_to_label = [||]

let read_graph_labels oFile
= let i = ref 0 in
  try while true do
    let line = input_line oFile in
    Array.set graph_to_label !i (int_of_string(line));
    i := !i + 1
  done
  with
    End_of_file -> ()

let file_graph_labels filename
= let oFile = open_in filename in
  try 
    read_graph_labels oFile;
    close_in oFile;
  with
  _ -> close_in oFile

  (*
max_node_label = 0
node_to_label = {}
with open("MUTAG/MUTAG_node_labels.txt") as file:
  i = 0 
  for line in file.readlines():
    label = line.strip()
    int_label = int(label) 
    node_to_label[i] = int_label
    i = i+1
    if int_label > max_node_label:
      max_node_label = int_label
*)

let max_node_label = ref 0
let node_to_label = ref [||]

let read_node_labels oFile
= let i = ref 0 in
  try while true do
    let label = input_line oFile in
    let int_label = int_of_string label in
    Array.set !node_to_label !i int_label;
    i := !i + 1;
    if (int_label > !max_node_label) then max_node_label := int_label
    else ()
  done
  with
    End_of_file -> ()

let file_node_labels filename
= let oFile = open_in filename in
  try 
    read_node_labels oFile;
    close_in oFile;
  with
  _ -> close_in oFile

  
let x_node = ref [||]
let x_node = Array.make_matrix (Array.length !node_to_label) (Array.length !node_to_label) !x_node

let update_x_node k
= for i = 1 to Array.length !node_to_label do
    Array.set (x_node.(i).(!node_to_label.(i))) i k
done
(*
let update_x_node 
= for val = 0 to node_to_label.len do
  x_node.(val).(node_to_label.(val)) = 1
done
*)