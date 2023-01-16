let trim_candidates tmp_candidate
= if(List.length tmp_candidate > 0) then
    let [current_abs_edges_e1, current_abs_edges_e2] = current_abs_  



let rec tmp_candidate candidates 
= if (List.length candidates > 0) then
  let tmp_candidate = candidates in
  let candidates = trim_candidates tmp_candidate in
  candidates 
else candidates

let sort_abs_graph_edges abs_graph
= let [abs_node, abs_edge] = abs_graph in
  let current_abs_edges = abs_edge in
  let candidate_edges = current_abs_edges in
  let new_abs_edges = List.hd current_abs_edges in

  let reachable = [ [], [] ] in
  let (current_abs_edges_e1, current_abs_edges_e2) = new_abs_edges in
  let reachable = reacuable @ [current_abs_edges_e1, current_abs_edges_e2] in
  let reachable = List.tl reachable in

  let candidates = current_abs_edges in
  let candidates = List.tl candidates in

  let candidates = tmp_candidate candidates in (*filter 쓰는게 나을 듯*)
  
  let new_abs_graph = [abs_node, new_abs_edges] 
in new_abs_graph





let rec remove_left_graphs 
= let graph = (remove 기능 구현) in
  let graph = btm_up_graph_chooser_from_big
  let abs_graph = construct_absgraph_BBBP
in graph


let rec update_abs_graphs 
= if (List.length left_graphs > 0) then
  let graph = btm_up_graph_chooser_from_middle
  let abs_graph = construct_absgraph_undirected 
  
  if(check_not_connected) then
    remove_left_graphs 

  let learned_abs_graph = generalize

  let score = eval_abs_graph_on_graphs_GC
  let chosen_train_graphs = eval_abs_graph_on_graphs_exist

  if(score < default_score * expected || List.length chosen_train_graphs = 1) then
    print_int(0);
    let graph = remove_left_graphs 
  else graph
  
  let chosen_graphs = eval_abs_graph_on_graphs_exist 
  let left_graphs = left_graphs - chosen_graphs in
  let learned_abs_graph = learned_abs_graph @ 

in learned_parameters


let learn_abs_graphs_bottom_up 
= let default_score = (List.length labeled_graphs + List.length train_graphs) / List.length train_graphs in
  let learned_parameters = [] in
  let learned_parameters = update_abs_graphs 
in learned_parameters 