
let graph_to_edges = Array.make 1 0
let graph_to_nodes = Array.make 1 0
let node_to_graph = Array.make 1 0


let read_graph_indicator oFile
= let i = ref 0 in
  try 
    while true do
      let line = input_line oFile in
      let graph_idx = int_of_string line in
      let idx = graph_idx -1 in
      let node_to_graph[i] = idx in

      if (Array.mem idx == false) let graph_to_nodes[idx] = []
      else graph_to_nodes[idx].append(i)
      
      let graph_to_edges[idx] = [] in
      let i := !i+1 in
      idx
    done
  with
  End_of_file -> print_newline ()

let file_graph_indicator filename
= let oFile = open_in filename in
  try
    read_graph_indicator oFile;
    close_in oFile;
  with
  _ -> close_in oFile