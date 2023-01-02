(*graph_to_edges = {}
graph_to_nodes = {}
node_to_graph = {}*)

let graph_to_edges = Array.create_matrix
let graph_to_nodes = Array.create_matrix
let node_to_graph = Array.create_matrix

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
    let graph_idx = int_to_string line in
    let idx = graph_idx -1 in      
    if (Array.mem idx graph_to_nodes = false) Array.append graph_to_nodes.(idx) [];
    else ()
    Array.append graph_to_nodes.(idx) i;
    Array.append graph_to_nodes.(idx) [];
    i := i + 1
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
    i = i + 1


graph_to_label = {}
with open("MUTAG/MUTAG_graph_labels.txt") as file:
  i = 0 
  for line in file.readlines():
    label = line.strip()
    graph_to_label[i] = int(label)
    i = i+1


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


let x_node = ref [||]

let make_x_node
= for i = 0 to node_to_label.len do
    x_node.append([]);
    for j = 0 to max_node_label.len+1 
    x_node.(i).append(0)
done

let update_x_node 
= for val = 0 to node_to_label.len do
  x_node.(val).(node_to_label.(val)) = 1
done
