(*graph_to_edges = {}
graph_to_nodes = {}
node_to_graph = {}*)

let graph_to_edges = {||}
let graph_to_nodes = {||}
let node_to_graph = {||}



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
= let lines = ref 0 in
  try while true do
    lines := lines + 1;
    let line = input_line oFile in
    let graph_idx = int_to_string line in
    let idx = graph_idx -1 in      
    if (Array.mem idx graph_to_nodes = false) graph_to_nodes[idx]
  
  done

  with
    End_of_file -> ();
    
    let file_graph_indicator filename
    = let oFile = open_in filename in
      try
        read_graph_indicator oFile;
        close_in oFile;
      with
      _ -> close_in oFile

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


X_node = []
for i in range(len(node_to_label)):
  X_node.append([])
  for _ in range(max_node_label+1):
    X_node[i].append(0)


for _, val in enumerate(node_to_label):
  X_node[val][node_to_label[val]] = 1
