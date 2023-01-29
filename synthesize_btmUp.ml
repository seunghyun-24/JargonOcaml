exception Error

type abstract_graph = abs_node list * abs_edge list
  and abs_node = itv list 
  and abs_edge = triple list
  and triple = itv list * from_idx * to_idx
  and itv = Itv of float * float 
  and from_idx = int
  and to_idx = int 

type tgraphs = tgraph list
  and tgraph = tnode list * tedge list
  and tnode = int
  and tedge = int * int

type parameter = {
  mutable graphs : tgraphs;
  mutable left_graphs : int list;
  mutable train_graphs : int list;
  mutable labeled_graphs : int list;
  mutable node_to_label : int list;
  mutable edge_to_label : int list
}

type my_maps = {
  mutable myA : (int * int) list;
  mutable x_edge : float list list;
  mutable x_node: float list list
}
(* 합집합, 차집합, 교집합을 위한 list에 사용될 함수 *)
let rec mem x = function
  | [] -> false
  | h::t -> h = x || mem x t

let rec remove x = function
  | [] -> failwith "x not in list"
  | h::t -> if h = x then t else h::(remove x t)

let mkset _list = List.sort_uniq compare _list 
(*
let intersect a b
= match a with
  | [] -> if b = [] then [] else intersect b a
  | h::t ->
    if mem h b then let b' = remove h b in h::(intersect t b')
    else intersect t b
*)

let intersect a b 
= List.filter (fun n -> List.mem (List.nth b n) a) a

let union a b = mkset (a@b) 

let rec difference a b
= match a with 
  | [] -> []
  | h::t -> if mem h b then difference t (remove h b) else h::(difference t b)

(* 합집합, 차집합, 교집합을 위한 list에 사용될 함수 구현 끝*)

(* tuple 때문에 사용하는 list 함수 *)

let rec saving_like_array _index _saving _list cnt
= match _list with
  | [] -> if(cnt=_index) then _list@[_saving]
          else saving_like_array _index _saving (_list@[]) (cnt+1)
  | h::t -> if(cnt=_index) then [_saving]@t 
            else h::(saving_like_array _index _saving t (cnt+1))

let tuple_sort _list
= List.sort (fun (k1, v1) (k2, v2) -> match compare v1 v2 with | 0 -> compare k1 k2 | c -> c) _list

let rec mem_tuple _tuple key
= match _tuple with
| [(), ()] -> false
| (e1, e2)::t -> if (e1=key || e2=key) then true else mem_tuple t key

(* *)

  (* createInitial_btmUp *)
let rec make_graphs_len_list left_graphs graphs graphs_len_list 
= match left_graphs with
  | [] -> graphs_len_list 
  | h::t -> 
    let (nodes, edges) = List.nth graphs h in 
    make_graphs_len_list t graphs (graphs_len_list@[(h, List.length edges)])

let btmUp_choose_middle left_graphs graphs
= let graphs_len_list = make_graphs_len_list left_graphs graphs [] in
  let graphs_len_list_sorted = tuple_sort graphs_len_list in
  let (graph_idx, graph_len) = List.nth graphs_len_list_sorted ((List.length left_graphs)/2)
in graph_idx
  
let rec undi_abs_node nodes my_maps abs_nodes node_abs_node_map cnt 
= match nodes with
  | [] -> (abs_nodes, node_abs_node_map)
  | h::t -> let node_feature = List.nth my_maps.x_node h in let abs_node = _undi_abs_node node_feature [] in
  undi_abs_node t my_maps (abs_nodes@abs_node) (saving_like_array h (cnt) node_abs_node_map 0) (cnt+1)

and _undi_abs_node node_feature abs_node 
= match node_feature with
  | [] -> abs_node | h::t -> _undi_abs_node t (abs_node@[(h, h)])

let rec undi_abs_edge edges my_maps node_abs_node_map abs_edges
= match edges with
  | [] -> abs_edges
  | h::t -> let (from_node, to_node) = List.nth my_maps.myA h in
  if(to_node > from_node) then 
    let edge_feature = List.nth my_maps.x_edge h in 
    let new_itv = _undi_abs_edge edge_feature [] in
    let abs_edge = (new_itv, List.nth node_abs_node_map from_node, List.nth node_abs_node_map to_node)
    in undi_abs_edge t my_maps node_abs_node_map (abs_edges@[abs_edge])
  else undi_abs_edge t my_maps node_abs_node_map abs_edges

and _undi_abs_edge edge_feature new_itv
= match edge_feature with | [] -> new_itv | h::t -> _undi_abs_edge t (new_itv@[h, h])

let construct_absgraph_undirected parameter my_maps graph_idx
= let (nodes, edges) = List.nth parameter.graphs graph_idx in
  let (abs_nodes, node_abs_node_map) = undi_abs_node nodes my_maps [] [] 0 in
  let abs_edges = undi_abs_edge nodes my_maps node_abs_node_map [[], 0, 0] in
  (abs_nodes, List.tl abs_edges)

  (* search_btmUp *)
let sort_abs_graph_edges 

let rec remove_edges ?(step=1) a b absEdges best_score
= if a > b then (best_abs_graph, best_score)
  else 
    let absEdges = pop
    let new_abs_graph = sort_abs_graph_edges new_abs_graph in
    let new_score = update_score
    if (new_score >= s) then remove_edges ~step (a + step) b new_abs_graph new_score
    else remove_edges ~step (a + step) b best_abs_graph best_score

let rec remove_nodes nodes edges 
= match nodes with
  | [] -> nodes
  | h::t -> 
    if (mem_tuple edges h) then h::(remove_nodes t edges)
    else remove_nodes t edges

and not_my_connect 
= 0

let search_btmUp abs_graphs training_graphs ci weight parameter my_maps
= let s = update_score abs_graph in
  let (abs_nodes, abs_edges) = abs_graphs in
  let (best_abs_graph, best_score) = remove_edges 0 (List.length abs_edges - 1) abs_edges s in
  let best_abs_graph = sort_abs_graph_edges best_abs_graph in
  let (best_abs_graph, best_score) = generalize_edge_top
  let (best_abs_graph, best_score) = generalize_node_top
  let (best_abs_graph, best_score) = refine 
  in best_abs_graph


let rec choose_graph abs_graph graphs my_maps
  let (n,e) = abs_graph in
  let chosen_graphs = choosing_graphs 0 (List.length n) abs_graph graphs my_maps
in chosen_graphs (*set*)

and choosing_graphs ?(step=1) a b abs_graph graphs my_maps
= if a > b then []
  else if (eval_abs_graph_DFS abs_graph graph my_maps) (a :: choosing_graphs ~step (a + step) b abs_graph graphs my_maps)
  else choosing_graphs ~step (a + step) b abs_graph graphs my_maps

(* 알고리즘을 위해 필요한 기본 함수 *)
let createInitial_btmUp train_graphs weight parameter my_maps
= let chosen_middle_graph = btmUp_choose_middle parameter.left_graphs parameter.graphs in
  let minimal_abstract_graph = construct_absgraph_undirected parameter my_maps chosen_middle_graph 
in minimal_abstract_graph

let default_score
= let intersection_labeled_and_trained_graphs = List.filter (fun n -> List.mem (List.nth labeled_graphs n) training_graphs) training_graphs
in (List.length intersection_labeled_and_trained_graphs / List.length training_graphs)

let rec update_score abs_graph graphs labeled_graphs left_graphs train_graphs my_maps
= let max_score = 0 in 
  let (abs_nodes, abs_edges) = abs_graph in
  let (correct_set, incorrect_set) = saving_set 0 graphs [] [] in
  if (List.length intersect left_graphs correct_set) = 0 then 0
  else (List.length correct_set / (List.length correct_set + List.length incorrect_set + 1))

and saving_set cnt graphs correct_set incorrect_set
= let (abs_node, abs_edge) = graphs in
  match abs_edge with
| [] -> (correct_set, incorrect_set)
| h::t -> 
  if (not List.mem train_graphs cnt) then saving_set (cnt+1) (List.tl abs_node, List.tl abs_edge) correct_set incorrect_set
  else 
    let exists = eval_abs_graph_DFS  in
    if (exists) then saving_set (cnt+1) (List.tl abs_node, List.tl abs_edge) correct_set@[cnt] incorrect_set
    else saving_set (cnt+1) (List.tl abs_node, List.tl abs_edge) correct_set incorrect_set@[cnt]
  
let rec learning_better train_graphs learned_abstract_graphs s ci weight parameter my_maps refine
= if (refine) then
    let candidate_learned_abstract_graphs = search_btmUp learned_abstract_graphs training_graphs ci weight parameter my_maps in
    let candidate_s = update_score in
    if (candidate_s >= s) then learning_better train_graphs candidate_learned_abstract_graphs candidate_s ci weight parameter my_maps refine
    else learning_better train_graphs learned_abstract_graphs s ci weight parameter my_maps false
else learned_abstract_graphs

let synthesize_btmUp train_graphs ci weight parameter my_maps
= let learned_abstract_graphs = createInitial_btmUp train_graphs weight parameter my_maps in
  let s = update_score
  let refine = true
  let learned_abstract_graphs = learning_better train_graphs learned_abstract_graphs s ci weight parameter my_maps refine
in learned_abstract_graphs


(*
= if(List.length parameter.left_graphs > 0)
    let s = update_score train_graphs learned_abstract_graphs ci weight labeled_graphs parameter my_maps in
    let chosen_graphs = find_better_graph learned_abstract_graphs train_graphs ci weight s parameter my_maps in
    let parameter.left_graphs = parameter.left_graphs - learned_abstract_graphs in
    let learned_abstract_graphs = learned_abstract_graphs@[chosen_graphs] in

    learning_better train_graphs learned_abstract_graphs ci weight labeled_graphs parameter my_maps
else learned_abstract_graphs*)