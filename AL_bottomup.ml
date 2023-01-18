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

(* 
let in_make_nodes abs_edges nodes
= match abs_edges with 
  | [] -> nodes
  | (_itv, fi, ti)::t -> in_make_nodes t (nodes@[fi]@[ti])

let let_us_DFS src edges

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
*)

(*btm_up_graph_chooser_from_middle*)
let rec make_graphs_len_list left_graphs graphs graphs_len_list 
= match left_graphs with
  | [] -> graphs_len_list 
  | (n,e)::t -> let graphs_len_list = graphs_len_list@[(List.length graphs_len_list), List.length e]
in make_graphs_len_list t graphs graphs_len_list

let btm_up_graph_chooser_from_middle left_graphs graphs
= let graphs_len_list = [] in
  let graphs_len_list = make_graphs_len_list left_graphs graphs graphs_len_list in
  let graphs_len_list_sorted = List.sort (fun (k1, v1) (k2, v2) -> match compare v1 v2 with | 0 -> compare k1 k2 | c -> c) graphs_len_list in
  let (graph_idx, graph_len) = List.nth graphs_len_list_sorted ((List.length left_graphs)/2)
in graph_idx
(*btm_up_graph_chooser_from_middle 끝*)

(*construct_absgraph_undirected*)
let rec feat_val_abs_ne ne_feature abs_
= match ne_feature with
  | [] -> abs_
  | h::t -> let abs_ = abs_@[(h,h)] in feat_val_abs_ne t abs_

let rec saving_like_array _index _saving _list cnt
= match _list with
  | [] -> if(cnt=_index) then _list@[_saving]
  else saving_like_array _index _saving (_list@[]) (cnt+1)
  | h::t -> if(cnt=_index) then [_saving]@t 
  else h::(saving_like_array _index _saving t (cnt+1))

let rec enum_graph_idx idx find_node abs_nodes node_abs_node_map x_node
= match find_node with
  | [] -> (abs_nodes, node_abs_node_map)
  | h::t -> 
    let node_feature = List.nth x_node h in
    let abs_node = [] in
    let abs_node = feat_val_abs_ne node_feature abs_node in
    let node_abs_node_map = saving_like_array idx h node_abs_node_map 0 in
    enum_graph_idx (idx+1) t (abs_nodes@abs_node) node_abs_node_map x_node

let rec saving_abs_edges find_edge abs_edges myA x_edge node_abs_node_map
= match find_edge with
  | [] -> abs_edges 
  | h::t -> 
    let (from_node, to_node) = List.nth myA h in
    let new_itv = [] in
    let edge_feature = List.nth x_edge h in
    let new_itv =  feat_val_abs_ne edge_feature new_itv in
    let abs_edge = (new_itv, List.nth node_abs_node_map from_node, List.nth node_abs_node_map to_node) in
    if(to_node > from_node) then let abs_edges = abs_edges@[abs_edge] in saving_abs_edges t abs_edges myA x_edge node_abs_node_map
    else saving_abs_edges t abs_edges myA x_edge node_abs_node_map

let construct_absgraph_undirected graph_idx my_maps graphs x_node
= let abs_nodes = [] in
  let abs_edges = [] in
  let node_abs_node_map = [] in

  let (find_node, find_edge) = List.nth graphs graph_idx in
  let (abs_nodes, node_abs_node_map) = enum_graph_idx 0 find_node abs_nodes node_abs_node_map x_node in
  let abs_edges = saving_abs_edges find_edge abs_edges
in (abs_nodes, abs_edges)
(*construct_absgraph_undirected 끝*)

(*generalize*)
  (* eval_abs_graph_DFS *)
let enu_itv itvs x_edge edge
= match itvs with
  | [] -> true
  | h::t -> let (bot, top) = List.nth itvs h in
  if(List.nth (List.nth x_edge edge) h < bot || top < List.nth (List.nth x_edge edge)) then false
  else enu_itv t x_edge edge

let concrete_edge_belong_abs_edge abs_edge edge x_edge
= let (itvs, p, q) = abs_edge in
  enu_itv itvs x_edge edge

let concrete_edge_belong_abs_node abs_node node x_node
= enu_itv abs_node x_node node


let eval_abs_graph_DFS
= let (nodes, edges) = graph in
  let (absNodes, absEdges) = abs_graph in
  let abs_edge_first = List.hd absEdges in
  let (abs_node_fr, abs_node_to) = abs_edge_first in
  let candidate_edges = [] in
  

  (* eval_abs_graph_DFS *)

let rec make_cic_set idx graphs abs_edges_len labeled_graphs train_graphs correct_set incorrect_set
= match graphs with
  | [] -> (correct_set, incorrect_set)
  | h::t -> if not(List.mem idx train_graphs) then make_cic_set (idx+1) t abs_edges_len labeled_graphs train_graphs correct_set incorrect_set
  else let (nodes, edges) = h in let edges_len = List.length edges in
  if (abs_edges_len > edges_len) then make_cic_set (idx+1) t abs_edges_len labeled_graphs train_graphs correct_set incorrect_set
  else let exists = eval_abs_graph_DFS in
  if(exists) then 
    if (List.mem idx labeled_graphs) then let correct_set = correct_set@[idx] in make_cic_set (idx+1) t abs_edges_len labeled_graphs train_graphs correct_set incorrect_set
    else let incorrect_set = incorrect_set@[idx] in make_cic_set (idx+1) t abs_edges_len labeled_graphs train_graphs correct_set incorrect_set
  else make_cic_set (idx+1) gt abs_edges_len labeled_graphs train_graphs correct_set incorrect_set

let rec check_same_thing (nodes, edges) correct_set
= match correct_set with
  | [] -> false
  | h::t -> if(List.mem h nodes || List.mem h edges) then true
  else check_same_thing (nodes, edges) t 

let eval_abs_graph_on_graphs_GC graphs abs_edges_len labeled_graphs left_graphs
= let (absNodes, absEdges) = abs_graph in
  let correct_set = [] in
  let incorrect_set = [] in
  let abs_edges_len = List.length absEdges in
  let (correct_set, incorrect_set) = make_cic_set 0 graphs abs_edges_len labeled_graphs train_graphs correct_set incorrect_set in
  if not (check_same_thing left_graph correct_set) then 0
  else (List.length correct_set / (List.length correct_set + List.length incorrect_set + 1))


let enumerate_and_remove_edges_aggressive

let generalize_edge_intervals_to_top 

let generalize_node_intervals_to_top

let refine 

let generalize abs_graph graphs labeled_graphs left_graphs train_graphs my_maps
= let best_abs_graph = abs_graph in
  let (absNodes, absEdges) = best_abs_graph in
  let edge_idx = List.length absEdges -1 in
  let best_score = eval_abs_graph_on_graphs_GC graphs abs_edges_len labeled_graphs left_graphs
  let (best_abs_graph, best_score) = enumerate_and_remove_edges_aggressive
  let best_abs_graph = sort_abs_graph_edges best_abs_graph in
  let (best_abs_graph, best_score) = generalize_edge_intervals_to_top 
  let (best_abs_graph, best_score) = generalize_node_intervals_to_top
  let (best_abs_graph, best_score) = refine 
in best_abs_graph

(*generalize 끝*)

let rec update_left_graphs left_graphs graphs my_maps
= if (List.length left_graphs > 0) then
    let graph_idx = btm_up_graph_chooser_from_middle left_graphs graphs
    let abs_graph = construct_absgraph_undirected graph_idx my_maps graphs x_node
  
    (*let (left_graphs, graph, abs_graph) = not_connected_abs_graph*) 
  
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


let learn_abs_graphs_bottom_up labeled_graphs train_graphs left_graphs graphs my_maps
= let default_score = (List.length labeled_graphs + List.length train_graphs) / List.length train_graphs in
  let learned_parameters = [] in
  let learned_parameters = update_left_graphs left_graphs graphs my_maps
in learned_parameters 