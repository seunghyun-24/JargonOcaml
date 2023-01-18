type abstract_graph = abs_node list * abs_edge list
and abs_node = itv list 
and abs_edge = triple list
and triple = itv list * from_idx * to_idx
and itv = Itv of float * float 
and from_idx = int
and to_idx = int 

(* sort_abs_graph_edges 함수 *)
let rec find_in_tuple mem tuple_list
= match tuple_list with
| [] -> false
| (a,b)::t -> if (a=mem || b=mem) then true else find_in_tuple mem t

let tmp_candidate (current_abs_edges : triple list) (reachable : ('a * 'b) list)
= match current_abs_edges with
  | [] -> true
  | (_itv, current_abs_edges_e1, current_abs_edges_e2)::tl -> 
    if (find_in_tuple current_abs_edges_e1 reachable || find_in_tuple current_abs_edges_e2 reachable) then false
    else true

let rec tmp_reachable_candidate (current_abs_edges : triple list) (reachable : ('a * 'b) list)
= match current_abs_edges with
  | [] -> reachable
  | ( _itv, current_abs_edges_e1, current_abs_edges_e2)::t -> 
    if (find_in_tuple current_abs_edges_e1 reachable || find_in_tuple current_abs_edges_e2 reachable) then 
      let reachable = reachable@[current_abs_edges_e1, current_abs_edges_e2] 
      in tmp_reachable_candidate t reachable
    else tmp_reachable_candidate t reachable

let rec tmp_newAbsEdges_candidate (current_abs_edges : triple list) reachable new_abs_edges
= match current_abs_edges with
  | [] -> new_abs_edges
  | ( _itv, current_abs_edges_e1, current_abs_edges_e2)::t -> 
    if (find_in_tuple current_abs_edges_e1 reachable || find_in_tuple current_abs_edges_e2 reachable) then 
      let new_abs_edges = new_abs_edges@[_itv, current_abs_edges_e1, current_abs_edges_e2] 
      in tmp_newAbsEdges_candidate t reachable new_abs_edges
    else tmp_newAbsEdges_candidate t reachable new_abs_edges  

let rec trim_candidates candidates new_abs_edges current_abs_edges reachable 
= if(List.length candidates  > 0) then
    let reachable = tmp_reachable_candidate current_abs_edges reachable in
    let new_abs_edges = tmp_newAbsEdges_candidate current_abs_edges reachable new_abs_edges in
    let candidates = List.filter (fun n -> tmp_candidate current_abs_edges reachable) candidates in
    trim_candidates candidates new_abs_edges current_abs_edges reachable 
  else (candidates, new_abs_edges, reachable)

let sort_abs_graph_edges (abs_graph : (itv list * triple list) ) 
= let (abs_node, abs_edge) = abs_graph in
  let current_abs_edges = abs_edge in
  let new_abs_edges = [List.hd current_abs_edges] in

  let reachable = [] in
  let ( _itv, current_abs_edges_e1, current_abs_edges_e2) = List.hd new_abs_edges in
  let reachable = reachable@[current_abs_edges_e1, current_abs_edges_e2] in
  let reachable = List.tl reachable in

  let candidates = current_abs_edges in
  let candidates = List.tl candidates in

  let (candidates, new_abs_edges, reachable) = trim_candidates candidates new_abs_edges current_abs_edges reachable in (*filter 쓰는게 나을 듯*)
  
  let new_abs_graph = [abs_node, new_abs_edges] 
in new_abs_graph

(* sort_abs_graph_edges 함수 끝 *)

let in_make_nodes abs_edges nodes
= match abs_edges with 
  | [] -> nodes
  | (_itv, fi, ti)::t -> in_make_nodes t (nodes@[fi]@[ti])

let let_us_DFS src edges 
= 

let isConnected graph
= let (nodes, edges) = graph in
  let visited_nodes_len = let_us_DFS 0 edges
  if (visited_nodes_len = List.length nodes) then true else false

let rec _takeNodes_inm nodes idx
= match nodes with
  | [] -> idx_node_map
  | h::t -> let (List.nth idx_node_map h) = idx in _takeNodes_inm nodes (idx+1)

let rec making_graph idx_node_map nodes 
= match nodes with
  | [] -> [g_node, g_edge]
  | h::t ->
    let g_node = g_node @ [List.hd idx_node_map h] in
    let List.nth g_edge (List.hd idx_node_map) = [] in
    making_graph idx_node_map t

let takeNodes graph nodes idx_node_map
= let nodes = List.sort compare nodes in
  let idx_node_map = _takeNodes_inm nodes 0 in
  let graph = making_graph idx_node_map nodes in graph

let check_addEdge graph a 
= if not (List.mem g_edges (List.nth graph (List.nth idx_node_map a))) 
  then let List.nth graph (List.nth idx_node_map a) = [] in graph
else graph

let addEdge
= let (g_nodes, g_edges) = graph in
  let graph = check_addEdge graph fr in
  let graph = check_addEdge graph to in
  let List.nth g_edges (List.nth idx_node_map fr) = List.nth g_edges (List.nth idx_node_map fr)@(List.nth idx_node_map to) in
  let List.nth g_edges (List.nth idx_node_map to) = List.nth g_edges (List.nth idx_node_map to)@(List.nth idx_node_map fr) in
  (g_nodes, g_edges)


let check_not_connected abs_graph
= let (abs_nodes, abs_edges) = abs_graph in
  let nodes = [] in
  let nodes = in_make_nodes abs_edges nodes
  let graph = takeNodes graph nodes
  let edges = []
  let edges = addEdge 
  let bool = isConnected graph 
in (not bool)

let remove_in_left_graph left_graph graph
= List.filter (fun n -> not (List.mem (List.nth left_graph n) graph)) graph

let rec not_connected_abs_graph 
= if (check_not_connected) then
  let left_graph = remove_in_left_graph left_graph graph in
  let graph = btm_up_graph_chooser_from_big
  let abs_graph = construct_absgraph_BBBP
in (left_graphs, graph, abs_graph)
else (left_graphs, graph, abs_graph)

let make_graphs_len_list left_graphs graphs graphs_len_list 
= match left_graphs with
  | [] -> graphs_len_list 
  | (n,e)::t -> let graphs_len_list = graphs_len_list@[(List.length graphs_len_list), List.length e]
in make_graphs_len_list t graphs graphs_len_list

let btm_up_graph_chooser_from_middle left_graphs graphs
= let graphs_len_list = [] in
  let graphs_len_list = make_graphs_len_list left_graphs graphs graphs_len_list in
  let graphs_len_list_sorted = List.sort (fun (k1, v1) (k2, v2) -> match compare v1 v2 with | 0 -> compare k1 k2 | c -> c) graphs_len_list in
  let (graph_idx, graph_len) = List.nth graphs_len_list_sorted (int (List.length left_graphs)/2) 
in graph_idx

let rec feat_val_abs_node node_feature abs_node num
= match node_feature with
| [] -> abs_node 
| h::t -> let abs_node = abs_node@(num, num) in feat_val_abs_node t abs_node (num+1)

let rec make_absNode abs_nodes
= let node_feature = List.mem x_node val_ in
  let abs_node = [] in
  let abs_node = feat_val_abs_node node_feature abs_node 0 in
  let abs_nodes = abs_nodes@abs_node in




let construct_absgraph_undirected 
= let abs_nodes = [] in
  let abs_edges = [] in
  let node_abs_node_map = [] in

  let (find_node, find_edge) = List.nth graphs graph_idx in
  let abs_nodes = make_absNode


in [abs_nodes, abs_edges]

let rec update_left_graphs
= if (List.length left_graphs > 0) then
    let graph = btm_up_graph_chooser_from_middle
    let abs_graph = construct_absgraph_undirected 

    let (left_graphs, graph, abs_graph) = not_connected_abs_graph 

    let learned_abs_graph = generalize 

    let score = eval_abs_graph_on_graphs_GC
    let chosen_train_graphs = eval_abs_graph_on_graphs_exist

    if(score < default_score * expected || List.length chosen_train_graphs = 1) then
      print_int(0);
      let left_graphs = remove_left_graphs 
      update_left_graphs 
    else 
      let chosen_graphs = eval_abs_graph_on_graphs_exist 
      let left_graphs = left_graphs - chosen_graphs in
      let learned_abs_graph = learned_abs_graph @ 
  else learned_parameters 
in learned_parameters


let learn_abs_graphs_bottom_up 
= let default_score = (List.length labeled_graphs + List.length train_graphs) / List.length train_graphs in
  let learned_parameters = [] in
  let learned_parameters = update_left_graphs
in learned_parameters 