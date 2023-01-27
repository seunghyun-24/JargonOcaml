type abstract_graph = abs_node list * abs_edge list
and abs_node = itv list 
and abs_edge = triple list
and triple = itv list * from_idx * to_idx
and itv = Itv of float * float 
and from_idx = int
and to_idx = int 

let convert_array _list
= Array.of_list _list

let convert_list _array 
= Array.to_list _array

let empty_list _list
= match _list with | [] -> true | h::t -> false

let rec make_graphs_len_list left_graphs graphs graphs_len_list 
= match left_graphs with
  | [] -> graphs_len_list 
  | (n,e)::t -> let graphs_len_list = graphs_len_list@[(List.length graphs_len_list), List.length e]
                in make_graphs_len_list t graphs graphs_len_list

let btmUp_choose_middle left_graphs graphs
= let graphs_len_list = [] in
  let graphs_len_list = make_graphs_len_list left_graphs graphs graphs_len_list in
  let graphs_len_list_sorted = List.sort (fun (k1, v1) (k2, v2) -> match compare v1 v2 with | 0 -> compare k1 k2 | c -> c) graphs_len_list in
  let (graph_idx, graph_len) = List.nth graphs_len_list_sorted ((List.length left_graphs)/2)
in graph_idx

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

let construct_absgraph_undirected graphs graph_idx x_node x_edge myA
= let abs_nodes = [] in
  let abs_edges = [] in
  let node_abs_node_map = [] in
  let (find_node, find_edge) = List.nth graphs graph_idx in
  let (abs_nodes, node_abs_node_map) = enum_graph_idx 0 find_node abs_nodes node_abs_node_map x_node in
  let abs_edges = saving_abs_edges find_edge abs_edges myA x_edge node_abs_node_map
in (abs_nodes, abs_edges)

let createInitial_btmUp training_graphs weight left_graphs graphs x_node x_edge myA
= let chosen_middle_graph = btmUp_choose_middle left_graphs graphs in
  let abs_graph = construct_absgraph_undirected graphs chosen_middle_graph x_node x_edge myA in
  let minimal_abstract_graph = abs_graph 
in minimal_abstract_graph

let update_score training_graphs learned_abstract_graphs ci weight labeled_graphs
= let intersection_labeled_and_trained_graphs = List.filter (fun n -> List.mem (List.nth labeled_graphs n) training_graphs) training_graphs
in (List.length intersection_labeled_and_trained_graphs / List.length training_graphs)

let rec check_same_thing (nodes, edges) correct_set
= match correct_set with
  | [] -> false
  | h::t -> if(List.mem h nodes || List.mem h edges) then true
  else check_same_thing (nodes, edges) t 

let eval_abs_graph_DFS

let rec try_set graphs idx corret_set incorrct_set train_graphs abs_edges_len
= match graphs with
  | [] -> (correct_set, incorrect_set)
  | h::t -> if (not List.mem train_graphs idx) then try_set t (idx+1) corret_set incorrct_set train_graphs abs_edges_len
            else let (nodes, edges) = graphs in
            if (abs_edges_len > List.length edges) then try_set t (idx+1) corret_set incorrct_set train_graphs abs_edges_len
            else let exist = eval_abs_graph_DFS in
              if(exist) try_set t (idx+1) (corret_set@[idx]) incorrct_set train_graphs abs_edges_len
              else try_set t (idx+1) corret_set (incorrct_set@[idx]) train_graphs abs_edges_len

let update_score_btmUp graphs abs_edges_len labeled_graphs left_graphs
= let (correct_set, incorrect_set) = try_set graphs 0 [] [] train_graphs abs_edges_len
  if (not (check_same_thing left_graph correct_set)) then 0
  else (List.length correct_set / (List.length correct_set + List.length incorrect_set + 1))

let rec remove_idx_edge _list idx z
= match _list with
  | [] -> []
  | h::t -> if (idx=z) then t
  else h@(remove_idx_edge t idx (z+1))

let rec remove_edges best_abs_graph best_score edge_idx flag graphs labeled_graphs left_graphs
= if (edge_idx >= 0) then
    let (absNodes, absEdges) = best_abs_graph in
    let absEdges = remove_idx_edge absEdges edge_idx 0 in
    let new_score = update_score_btmUp graphs (List.length absEdges) labeled_graphs left_graphs
    if (new_score >= best_score) then remove_edges (absNodes, absEdges) new_score (edge_idx-1) true
    else remove_edges best_abs_graph best_score (edge_idx-1) flag
  else (best_abs_graph, best_score)

let generalize_edge_top best_abs_graph best_score edge_idx 
= if(edge_idx >= 0) then
    let (absNodes, absEdges) = best_abs_graph in
    let (itv, efrom, eto) = (convert_array absEdges).(edge_idx) in
    let absEdges = convert_list (Array.set (convert_array absEdges) edge_idx ([], efrom, eto)) in
    let new_score = update_score_btmUp graphs abs_edges_len labeled_graphs left_graphs
    if (new_score >= best_score) then generalize_edge_top (absNodes, absEdges) new_score (edge_idx-1)
    else generalize_edge_top best_abs_graph best_score (edge_idx-1)
else (best_abs_graph, best_score)

let rec generalize_node_top best_abs_graph best_score node_idx 
= if(node_idx >= 0) then
    let (absNodes, absEdges) = best_abs_graph in
    let absNodes = convert_list (Array.set convert_array absNodes node_idx (-100)) in
    let new_score = update_score_btmUp graphs abs_edges_len labeled_graphs left_graphs
    if (new_score >= best_score) then generalize_node_top (absNodes, absEdges) new_score (node_idx-1)
    else generalize_node_top best_abs_graph best_score (node_idx-1)
else (best_abs_graph, best_score)

let rec enu_itvs_n _itvs best_abs_graph best_score flag labeled_graphs left_graphs
= match _itvs with
  | [] -> (best_abs_graph, best_score, flag)
  | h::t -> 
    let (a,b) = List.nth itvs h in
    let (abs_node, abs_edge) = best_abs_graph in

    if(a!=-99 && b!=99) then 
      let new_itvs = convert_list (convert_array itvs feat_idx (a, 99)) in
      let abs_node = convert_list (convert_array abs_node node_idx new_itvs) in
      let new_score = update_score_btmUp (abs_node, abs_edge) (List.length abs_edge) labeled_graphs left_graphs
      
      if(new_score >= best_score) then 
        let (flag, best_abs_graph, best_score) = (true, new_abs_graph, new_score) in 
        let new_itvs = convert_list (convert_array itvs feat_idx (-99, b)) in
        let abs_node = convert_list (convert_array abs_node node_idx new_itvs) in
        let new_score = update_score_btmUp (abs_node, abs_edge) (List.length abs_edge) labeled_graphs left_graphs
        if(new_score >= best_score) then 
          let (flag, best_abs_graph, best_score) = (true, new_abs_graph, new_score) in 
          enu_itvs_n t best_abs_graph best_score flag labeled_graphs left_graphs
        else enu_itvs_n t best_abs_graph best_score flag labeled_graphs left_graphs
      else 
        let new_itvs = convert_list (convert_array itvs feat_idx (-99, b)) in
        let abs_node = convert_list (convert_array abs_node node_idx new_itvs) in
        let new_score = update_score_btmUp (abs_node, abs_edge) (List.length abs_edge) labeled_graphs left_graphs
        if(new_score >= best_score) then 
          let (flag, best_abs_graph, best_score) = (true, new_abs_graph, new_score) in 
          enu_itvs_n t best_abs_graph best_score flag labeled_graphs left_graphs
        else enu_itvs_n t best_abs_graph best_score flag labeled_graphs left_graphs

    else if ((a!=-99 && b==99) || (a == -99 && b != 99)) then
      let new_itvs = convert_list (convert_array itvs feat_idx (-99, 99)) in
      let abs_node = convert_list (convert_array abs_node node_idx new_itvs) in
      let new_score = update_score_btmUp (abs_node, abs_edge) (List.length abs_edge) labeled_graphs left_graphs
      if(new_score >= best_score) then 
        let (flag, best_abs_graph, best_score) = (true, new_abs_graph, new_score) in 
          enu_itvs_n t best_abs_graph best_score flag labeled_graphs left_graphs
      else enu_itvs_n t best_abs_graph best_score flag labeled_graphs left_graphs
    
    else enu_itvs_n t best_abs_graph best_score flag labeled_graphs left_graphs
        
let rec widening_node (absNodes, absEdges) node_idx current_abs_graph 
= match absNodes with
  | [] -> (best_abs_graph, best_score, flag)
  | h::t -> 
    let (abs_node, abs_edge) = current_abs_graph in
    let _itvs = List.nth abs_node node_idx in
    if (empty_list _itvs) then widening_node (t, absEdges) (node_idx+1) current_abs_graph
    else let (best_abs_graph, best_score, flag) = enu_itvs_n
        in widening_node (t, absEdges) (node_idx+1) current_abs_graph


let rec enu_itvs_e _itvs best_abs_graph best_score flag labeled_graphs left_graphs p q
= match _itvs with
  | [] -> (best_abs_graph, best_score, flag)
  | feat_idx::t -> 
    let (a,b) = List.nth itvs feat_idx in
    let (abs_node, abs_edge) = best_abs_graph in

    if(a!=-99 && b!=99) then 
      let new_itvs = convert_list (convert_array itvs feat_idx (a, 99)) in
      let abs_edge = convert_list (convert_array abs_edge edge_idx (new_itvs, p, q)) in
      let new_score = update_score_btmUp (abs_node, abs_edge) (List.length abs_edge) labeled_graphs left_graphs
      
      if(new_score >= best_score) then 
        let (flag, best_abs_graph, best_score) = (true, new_abs_graph, new_score) in 
        let new_itvs = convert_list (convert_array itvs feat_idx (-99, b)) in
        let abs_edge = convert_list (convert_array abs_edge edge_idx (new_itvs, p, q)) in
        let new_score = update_score_btmUp (abs_node, abs_edge) (List.length abs_edge) labeled_graphs left_graphs
        if(new_score >= best_score) then 
          let (flag, best_abs_graph, best_score) = (true, new_abs_graph, new_score) in 
          enu_itvs_e t best_abs_graph best_score flag labeled_graphs left_graphs p q
        else enu_itvs_e t best_abs_graph best_score flag labeled_graphs left_graphs p q
      else 
        let new_itvs = convert_list (convert_array itvs feat_idx (-99, b)) in
        let abs_edge = convert_list (convert_array abs_edge edge_idx (new_itvs, p, q)) in
        let new_score = update_score_btmUp (abs_node, abs_edge) (List.length abs_edge) labeled_graphs left_graphs
        if(new_score >= best_score) then 
          let (flag, best_abs_graph, best_score) = (true, new_abs_graph, new_score) in 
          enu_itvs_e t best_abs_graph best_score flag labeled_graphs left_graphs p q
        else enu_itvs_e t best_abs_graph best_score flag labeled_graphs left_graphs p q

    else if ((a!=-99 && b==99) || (a == -99 && b != 99)) then
      let new_itvs = convert_list (convert_array itvs feat_idx (-99, 99)) in
      let abs_edge = convert_list (convert_array abs_edge edge_idx (new_itvs, p, q)) in
      let new_score = update_score_btmUp (abs_node, abs_edge) (List.length abs_edge) labeled_graphs left_graphs
      if(new_score >= best_score) then 
        let (flag, best_abs_graph, best_score) = (true, new_abs_graph, new_score) in 
          enu_itvs_e t best_abs_graph best_score flag labeled_graphs left_graphs p q
      else enu_itvs_e t best_abs_graph best_score flag labeled_graphs left_graphs p q
    
    else enu_itvs_e t best_abs_graph best_score flag labeled_graphs left_graphs p q

let widening_edge (absNodes, absEdges) edge_idx current_abs_graph flag best_abs_graph best_score 
= match absEdges with
  | [] -> (best_abs_graph, best_score, flag) 
  | h::t ->
    let (abs_node, abs_edge) = current_abs_graph in
    let (_itvs, p, q) = List.nth abs_edge edge_idx in
    if (empty_list _itvs) then widening_edge (absNodes, t) (edge_idx+1) current_abs_graph flag best_abs_graph best_score 
    else let (best_abs_graph, best_score, flag) = enu_itvs_e _itvs best_abs_graph best_score flag labeled_graphs left_graphs p q
        in widening_edge (absNodes, t) (edge_idx+1) current_abs_graph flag best_abs_graph best_score 

let rec refine abs_graph current_abs_graph best_abs_graph best_score flag 
= let (absNodes, absEdges) = abs_graph in
  let (best_abs_graph, best_score, flag) = widening_node abs_graph 0 current_abs_graph flag best_abs_graph best_score in
  let (best_abs_graph, best_score, flag) = widening_edge abs_graph 0 current_abs_graph flag best_abs_graph best_score in
  let (absNodes, absEdges) = abs_graph in
  let (best_abs_graph, best_score) = remove_edges best_abs_graph best_score (List.nth absEdges -1) flag in
  if (flag) refine abs_graph current_abs_graph best_abs_graph best_score flag 
  else (best_abs_graph, best_score)

let search_btmUp learned_abstract_graphs training_graphs ci weight
= let best_abs_graph = abs_graph in
  let (absNodes, absEdges) = best_abs_graph in
  let edge_idx = List.length absEdges -1 in
  let best_score = update_score_btmUp 
  
  (*remove edge*)
  let (absNodes, absEdges) = abs_graph in
  let (original_edge_len, edge_idx) = (List.nth absEdges, List.nth absEdges -1) in
  let (best_abs_graph, best_score) = remove_edges best_abs_graph best_score edge_idx true in

  (*remove node*)

  (*generalize node*)
  let (original_node_len, node_idx) = (List.nth absNodes, List.nth absNodes -1) in
  let (best_abs_graph, best_score) = generalize_node_top best_abs_graph best_score node_idx in

  (*generalize edge*)
  let (best_abs_graph, best_score) = generalize_edge_top best_abs_graph best_score edge_idx in
  let flag = false in
  let (best_abs_graph, best_score) = refine abs_graph current_abs_graph best_abs_graph best_score flag 

in best_abs_graph

let better_btmUp s candidate_s (*bool 값 return *)
= if (candidate_s >= s) then true
else false

let rec find_better_graph learned_abstract_graphs training_graphs ci weight s
= let candidate_learned_abstract_graphs = search_btmUp learned_abstract_graphs training_graphs ci weight in
  let candidate_s = update_score training_graphs learned_abstract_graphs ci weight in
  if better_btmUp s candidate_s then 
    let learned_abstract_graphs = candidate_learned_abstract_graphs in
    let s = candidate_s  
    in find_better_graph learned_abstract_graphs training_graphs ci weight s
  else learned_abstract_graphs

let synthesize_btmUp training_graphs ci weight labeled_graphs
= let learned_abstract_graphs = createInitial_btmUp training_graphs weight in
  let s = update_score training_graphs learned_abstract_graphs ci weight labeled_graphs in
  let learned_abstract_graphs = find_better_graph learned_abstract_graphs training_graphs ci weight s
in learned_abstract_graphs