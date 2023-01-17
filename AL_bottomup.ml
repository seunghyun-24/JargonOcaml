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



let rec remove_left_graphs 
= let graph = (remove 기능 구현) in
  let graph = btm_up_graph_chooser_from_big
  let abs_graph = construct_absgraph_BBBP
in graph


let rec update_left_graphs
= if (List.length left_graphs > 0) then
    let graph = btm_up_graph_chooser_from_middle
    let abs_graph = construct_absgraph_undirected 

    let left_graphs = not_connected_abs_graph 

    let learned_abs_graph = generalize 

    let score = eval_abs_graph_on_graphs_GC
    let chosen_train_graphs = eval_abs_graph_on_graphs_exist

    if(score < default_score * expected || List.length chosen_train_graphs = 1) then
      print_int(0);
      let graph = remove_left_graphs 
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