let rec read_feature oFile features 
= try 
  let line = input_line oFile in
    read_feature oFile (features @ [[(int_of_string line)]]) 
with 
  End_of_file -> features 

let rec read_graph_labels oFile labels 
= 
try 
  let line = input_line oFile in
    let l = int_of_string line in
    if l == 1 then read_graph_labels oFile (labels @ [1]) 
    else read_graph_labels oFile (labels @ [0]) 
with 
  End_of_file -> labels 

let rec read_indicator oFile indicator
= 
try 
  let line = input_line oFile in
    read_indicator oFile (indicator @ [(int_of_string line) - 1]) 
with 
  End_of_file -> indicator 

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