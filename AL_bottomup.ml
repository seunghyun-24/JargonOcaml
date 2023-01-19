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

let rec condition_candidate_edges candidate_edges edges abs_edge_first edge x_edge myA absNode abs_node_fr abs_node_to x_node 
= match edges with
  | [] -> candidate_edges
  | h::t -> let cond1 = concrete_edge_belong_abs_edge abs_edge_first h x_edge in
  let (e1, e2) = List.nth myA h in
  let cond2 = concrete_edge_belong_abs_node (List.nth absNodes abs_node_fr) e1 x_node in
  let cond3 = concrete_edge_belong_abs_node (List.nth absNodes abs_node_to) e2 x_node in
  if (cond1 && cond2 && cond3) then let candidate_edges = candidate_edges@[h] in condition_candidate_edges candidate_edges t abs_edge_first edge x_edge myA absNode abs_node_fr abs_node_to x_node 
  else condition_candidate_edges candidate_edges t abs_edge_first edge x_edge myA absNode abs_node_fr abs_node_to x_node 


let get_abs_edge_case_and_update_sub_abs_graph sub_abs_graph abs_edge
= let (itv, p, q) = abs_edge in
  let (sub_abs_nodes, sub_abs_edges) = sub_abs_graph in
  if (List.mem p sub_abs_nodes && List.mem q sub_abs_nodes) then
    let sub_abs_edges = sub_abs_edges@[(p,q)] in 
    let sub_abs_graph = (sub_abs_nodes, sub_abs_edges) in
    let case = 2 in (sub_abs_graph, case)
  else if (List.mem q sub_abs_nodes) then
    let sub_abs_nodes = sub_abs_nodes@[p] in
    let sub_abs_edges = sub_abs_edges@[(p,q)] in
    let case = 1 in
    let sub_abs_graph = (sub_abs_nodes, sub_abs_edges) in
    (sub_abs_graph, case)
  else if (List.mem p sub_abs_nodes) then
    let sub_abs_nodes = sub_abs_nodes@[q] in
    let sub_abs_edges = sub_abs_edges@[(p,q)] in
    let sub_abs_graph = (sub_abs_nodes, sub_abs_edges) in
    let case = 0 in (sub_abs_graph, case) 
  else (sub_abs_graph, (-1))

  
let rec candidating_fr_nodes 
= match candidate_fr_nodes with
  | [] -> 1 
  | h::t ->
  let condition1 = not (List.mem fr_con (List.hd subgraph)) in
  let condition2 = concrete_node_belong_abs_node (List.nth absNodes abs_node_fr, List.hd (List.nth myA), x_node) in
  let condition3 = concrete_edge_belong_abs_edge target_abs_edge con_edge x_edge in
  if(condition1 && condition2 && condition3) then
    let (itv, e1, e2) = target_abs_edge in
    let new_abs_edge_idx_to_concrete_edge = abs_edge_idx_to_concrete_edge in
    let new_abs_node_idx_to_concrete_node = abs_edge_idx_to_concrete_edge in
    let new_abs_edge_idx_to_concrete_edge = saving_like_array abs_edge_idx con_edge new_abs_edge_idx_to_concrete_edge 0 in
    let new_abs_node_idx_to_concrete_node = saving_like_array e1 fr_con new_abs_node_idx_to_concrete_node 0 in
    let new_subgraph = subgraph in
    let (new_node, new_edge) = new_subgraph in
    let new_node = new_node@[fr_con] in
    let new_edge = new_edge@[con_edge] in
    if(exist_subgraph_DFS ) then 0
    else candidating_fr_nodes 
  else candidate_fr_nodes 

let rec candidating_to_nodes 
= match candidate_to_nodes with
  | [] -> 1 
  | h::t ->
    let (absNodes, absEdges) = abs_graph in
    let (e1, e2) = List.nth myA con_edge
    let (con_edge, to_con) = h in
    let condition1 = not (List.mem to_con (List.hd subgraph)) in
    let condition2 = concrete_node_belong_abs_node (List.nth absNodes abs_node_to) e2 x_node in
    let condition3 = concrete_edge_belong_abs_edge target_abs_edge con_edge x_edge in
    if(condition1 && condition2 && condition3) then
      let (itv, e1, e2) = target_abs_edge
      let new_abs_node_idx_to_concrete_node = abs_node_idx_to_concrete_node in
      let new_abs_node_idx_to_concrete_node = saving_like_array e2 to_con new_abs_node_idx_to_concrete_node 0 in
      let new_abs_edge_idx_to_concrete_edge = abs_edge_idx_to_concrete_edge in
      let new_abs_edge_idx_to_concrete_edge = saving_like_array abs_edge_idx con_edge new_abs_edge_idx_to_concrete_edge 0 in
      let new_subgraph = subgraph in
      let (new_node, new_edge) = new_subgraph in
      let new_node = new_subgraph@[to_con] in
      let new_edge = new_subgraph@[con_edge] in 
      if(exist_subgraph_DFS) then 0
      else candidating_to_nodes 
    else candidating_to_nodes

and 

exist_subgraph_DFS subgraph sub_abs_graph abs_graph graph abs_edge_idx abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge nodes_to_edge
= let (absNodes, absEdges) = abs_graph in
  if (List.length absEdges = abs_edge_idx) then false 
  else let target_abs_edge = List.nth absEdges abs_edge_idx in
  let new_sub_abs_graph = sub_abs_graph in
  let (new_sub_abs_graph, case) = get_abs_edge_case_and_update_sub_abs_graph sub_abs_graph abs_edge in
  if (case = 2) then
    let (itv_, abs_node_fr, abs_node_to) = target_abs_edge in
    let fr_con = List.nth abs_node_idx_to_concrete_node abs_node_fr in
    let to_con = List.nth abs_node_idx_to_concrete_node abs_node_to in
    if List.mem (fr_con, to_con) nodes_to_edge then 
      let con_edge = List.nth nodes_to_edge (fr_con, to_con) in
      if concrete_edge_belong_abs_edge target_abs_edge con_edge x_edge then
        let new_abs_edge_idx_to_concrete_edge = abs_edge_idx_to_concrete_edge in
        let new_abs_edge_idx_to_concrete_edge = saving_like_array abs_edge_idx con_edge new_abs_edge_idx_to_concrete_edge 0 in
        let new_subgraph = subgraph in
        let (new_node, new_edge) = new_subgraph in
        let new_edge = new_edge@[con_edge] in
        let new_subgraph = (new_node, new_edge) in
        if (exist_subgraph_DFS ) then 0
        else 1
      else 1
    else if (case = 1) then
      let (itv_, abs_node_fr, abs_node_to) = target_abs_edge in
      let to_con = List.nth abs_node_idx_to_concrete_node abs_node_to in
      let candidate_fr_nodes = List.nth pred_node_to_nodes to_con in
      let result = candidating_fr_nodes in
      result
    else if (case = 0) then
      let (itv_, abs_node_fr, abs_node_to) = target_abs_edge in
      let fr_con = List.nth abs_node_idx_to_concrete_node abs_node_fr in
      let candidate_to_nodes = succ_node_to_nodes fr_con in
      let result = candidating_to_nodes in
      result
    else 1

let rec checking_exist_subgraph_DFS candidate_edges abs_node_fr abs_node_to sub_abs_graph_edge myA init_graph_edge subgraph 
= match candidate_edges with
  | [] -> false
  | h::t -> let abs_node_idx_to_concrete_node = [] in let abs_node_idx_to_concrete_edge = [] in
  let sub_abs_graph_edge = (abs_node_fr, abs_node_to) in let sub_ags_graph = [[abs_node_fr, abs_node_to], [sub_abs_graph_edge]] in
  let subgraph = [] in let (e1, e2) = List.nth myA init_graph_edge in let subgraph = subgraph @ [[e1@[e2]], init_graph_edge] in
  let abs_node_idx_to_concrete_edge = saving_like_array 0 init_graph_edge abs_node_idx_to_concrete_edge 0 in
  let abs_node_idx_to_concrete_ndoe = saving_like_array abs_node_fr e1 abs_node_idx_to_concrete_edge 0 in
  let abs_node_idx_to_concrete_ndoe = saving_like_array abs_node_to e2 abs_node_idx_to_concrete_edge 1 in
  if(exit_subgraph_DFS ) then true
  else checking_exist_subgraph_DFS t abs_node_fr abs_node_to sub_abs_graph_edge myA init_graph_edge subgraph 



let eval_abs_graph_DFS x_node x_edge myA 
= let (nodes, edges) = graph in
  let (absNodes, absEdges) = abs_graph in
  let abs_edge_first = List.hd absEdges in
  let (abs_node_fr, abs_node_to) = abs_edge_first in
  let candidate_edges = [] in
  let candidate_edges = condition_candidate_edges candidate_edges edges abs_edge_first edge x_edge myA absNode abs_node_fr abs_node_to x_node in
  let bool_exist_subgraph_DFS = checking_exist_subgraph_DFS candidate_edges abs_node_fr abs_node_to sub_abs_graph_edge myA init_graph_edge subgraph 
in bool_exist_subgraph_DFS

  (* eval_abs_graph_DFS *)

  (* eval_abs_graph_on_graphs_GC *)
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

  (* eval_abs_graph_on_graphs_GC 끝 *)


  (*enumerate_and_remove_edges_aggressive *)
let rec remove_idx_edge _list idx z
= match _list with
  | [] -> []
  | h::t -> if (idx=z) then t
  else h@(remove_idx_edge t idx (z+1))

let rec remove_edge 
= if (edge_idx >= 0) then
    let new_abs_graph = best_abs_graph in
    let (absNodes, absEdges) - new_abs_graph in
    let absEdges = remove_idx_edge absEdges edge_idx 0 in
    if (not my_connect new_abs_graph) then
      let edge_idx = edge_idx-1 in
      remove_edge
    else 
      let new_abs_graph = sort_abs_graph_edges new_abs_graph in
      let new_score = eval_abs_graph_on_graphs_GC  in

      if( new_score >= best_socre ) then 
        let best_abs_graph = new_abs_graph in
        let best_score = new_score in
        let edge_idx = edge_idx-1 in
        remove_edge
      else let edge_idx = edge_idx-1 in remove_edge
  else (best_abs_graph, best_score)

let enumerate_and_remove_edges_aggressive abs_graph current_score
= let current_abs_graph = abs_graph in
  let best_abs_graph = abs_graph in
  let best_score = current_score in
  let (absNodes, absEdges) = abs_graph in
  let original_edge_len = List.nth absEdges in
  let edge_idx = List.length absEdges -1 in
  let (best_abs_graph, best_score) = remove_edge
in (best_abs_graph, best_score)

  (*enumerate_and_remove_edges_aggressive 끝*)

let rec remove_edge_intervals 
= if(edge_idx >= 0) then
  let new_abs_graph = best_abs_graph in
  let new_itv = [] in
  let (absNodes, absEdges) = new_abs_graph in
  let (itv, new_from, new_to) = List.nth absEdges edge_idx in
  let absEdges = saving_like_array edge_idx (new_itv, new_from, new_to) absEdges 0 in
  let new_abs_graph = (absNodes, absEdges) in
  let new_score = eval_abs_graph_on_graphs_GC in
  if(new_score >= best_score) then
    let best_abs_graph = new_abs_graph in
    let best_score = new_score in
    let edge_idx = edge_idx -1 in
    remove_edge_intervals   
  else let edge_idx = edge_idx -1 in remove_edge_intervals  
else (best_abs_graph, best_score)

let generalize_edge_intervals_to_top 
= let best_abs_graph = abs_graph in
  let best_score = current_score in 
  let (absNodes, absEdges) = best_abs_graph in
  let edge_idx = absEdges -1 in
  let (best_abs_graph, best_score) = remove_edge_intervals 

let rec remove_node 
= if (node_idx >= 0) then
  let new_abs_graph = best_abs_graph in
  let new_score = eval_abs_graph_on_graphs_GC in
  if(new_score >= best_score) then
    let best_abs_graph = new_abs_graph in
    let best_score = new_score in
    let node_idx = node_idx -1 in
    remove_node 
  else node_idx = node_idx-1 in remove_node 
else (best_abs_graph, best_score)

let generalize_node_intervals_to_top
= let (absNodes, absEdges) = abs_graph
  let best_abs_graph = abs_graph in
  let best_score = current_score in
  let node_idx = List.length absNodes -1 in 
  let (best_abs_graph, best_score) = remove_node
  
  (*refine*)
let rec enu_itvs itvs
= match itvs with 
  | [] ->
  | h::t -> 
    let (newAbsNodes, newAbsEdges) = new_abs_grpah in
    let (a,b) = List.nth h itvs in
  if ( a != -99 && b != 99 ) then
    let new_abs_graph = current_abs_graph in
    let new_itvs = itvs in
    let new_itvs = saving_like_array feat_idx (a,99) new_itvs 0 in
    let newAbsNode = saving_like_array node_idx new_itvs newAbsNodes 0 in
    let new_score = eval_abs_graph_on_graphs_GC  in

    if (new_score >= best_score) then 
      let flag = true in
      let best_abs_graph = new_abs_graph in
      let best_score = new_score in
      enu_itvs2

    else if ((a != -99 && b == 99) || (a == -99 && b != 99))

and

enu_itvs2 
= let new_abs_graph = current_abs_graph in
  let new_itvs = itvs in
  let new_itvs = saving_like_array feat_idx (-99,b) new_itvs 0 in
  let newAbsNode = saving_like_array node_idx new_itvs newAbsNodes 0 in
  let new_score = eval_abs_graph_on_graphs_GC new_abs_graph graphs labeled_graphs left_graphs train_graphs  in
  if (new_score >= best_score ) then
    let flag = True in
    let best_abs_graph = new_abs_graph in
    let best_score = new_score 


let rec range_absNodes absNodes
= match absNodes with
  | [] -> 
  | h::t -> 
    let (absNodes, absEdges) = current_abs_graph in
    let itvs = List.nth absNodes node_idx in
    if (List.empty itvs) range_absNodes t
    else enu_itvs itvs

let refine 
= let current_abs_graph = abs_graph in
  let best_abs_graph = abs_graph in
  let best_score = current_score in
  let flag = False in
  let (absNodes, absEdges) = abs_graph in
  let original_node_len = List.length absNodes in
  let (absNodes, absEdges) = abs_graph in
  let  = range_absNodes absNodes
  
  (*refine 끝*)

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