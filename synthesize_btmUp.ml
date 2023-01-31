exception Error

type abstract_graph = abs_node * abs_edge
  and abs_node = (int * int) list 
  and abs_edge = triple list
  and triple = (int * int) list * from_idx * to_idx
  and itv = Itv of float * float 
  and from_idx = int
  and to_idx = int 

type tgraphs = tgraph list
  and tgraph = tnode list * tedge list
  and tnode = int
  and tedge = int * int

type parameter = {
  mutable graphs : (int list * (int * int) list) list;
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

let rec update_score abs_graph graphs labeled_graphs left_graphs train_graphs my_maps
= let (abs_nodes, abs_edges) = abs_graph in
  let (correct_set, incorrect_set) = saving_set 0 graphs [] [] train_graphs abs_graph my_maps in
  if (List.length (intersect left_graphs correct_set)) = 0 then 0
  else (List.length correct_set / (List.length correct_set + List.length incorrect_set + 1))

and saving_set cnt graphs correct_set incorrect_set train_graphs abs_graph my_maps
= match graphs with
| (n, e)::t -> if(not (List.mem cnt train_graphs)) then saving_set (cnt+1) t correct_set incorrect_set train_graphs abs_graph my_maps
else let exists = eval_abs_graph_DFS abs_graph graphs my_maps in
if (exists) then saving_set (cnt+1) t (correct_set@[cnt]) incorrect_set train_graphs abs_graph my_maps
else saving_set (cnt+1) t correct_set (incorrect_set@[cnt]) train_graphs abs_graph my_maps
| [] -> (correct_set, incorrect_set)

and eval_abs_graph_DFS abs_graph graph my_maps =
false 
(* tuple 때문에 사용하는 list 함수 *)

let rec saving_like_array _index _saving _list cnt
= match _list with
  | [] -> if(cnt=_index) then _list@[_saving]
          else saving_like_array _index _saving (_list@[]) (cnt+1)
  | h::t -> if(cnt=_index) then [_saving]@t 
            else h::(saving_like_array _index _saving t (cnt+1))

let rec remove_like_array _index _list cnt
= match _list with
  | [] -> if(cnt=_index) then _list
          else remove_like_array _index (_list@[]) (cnt+1)
  | h::t -> if(cnt=_index) then t 
            else h::(remove_like_array _index t (cnt+1))

let tuple_sort _list
= List.sort (fun (k1, v1) (k2, v2) -> match compare v1 v2 with | 0 -> compare k1 k2 | c -> c) _list

let rec mem_tuple _tuple key
= match _tuple with
| [[], (), ()] -> false
| (itv, e1, e2)::t -> if (List.mem key itv) then true else mem_tuple t key

let rec set_new_itv edge_idx absEdges cnt
= match absEdges with
  | (a, b, c)::t -> if (edge_idx = cnt) then ([], b, c)::t 
  else (a,b,c)::(set_new_itv edge_idx t (cnt+1))
  | _ -> raise Error

let empty_list _list
= match _list with 
  | h::t -> false
  | [] -> true 
  

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

let rec graph_slicing_array graphs graph_idx cnt
= match graphs with
  | (n,e)::t -> 
    if (graph_idx = cnt) then (n, e)
    else graph_slicing_array t graph_idx (cnt+1)
  | _ -> raise Error 

let construct_absgraph_undirected parameter my_maps graph_idx
= let (nodes, edges) = graph_slicing_array parameter.graphs graph_idx 0 in
  let (abs_nodes, node_abs_node_map) = undi_abs_node nodes my_maps [] [] 0 in
  let abs_edges = undi_abs_edge nodes my_maps node_abs_node_map [[], 0, 0] in
  (abs_nodes, List.tl abs_edges)

  (* search_btmUp *)
let rec remove_nodes nodes edges 
= match nodes with
  | [(), ()] -> nodes
  | h::t -> 
    if (mem_tuple edges h) then h::(remove_nodes t edges)
    else remove_nodes t edges

let rec remove_edges_and_nodes abs_graph parameter my_maps current_score
= let (absNodes, absEdges) = abs_graph in
  let edge_idx = List.length absEdges -1 in
  let (best_abs_graph, best_score) = remove_edge_idx abs_graph current_score edge_idx parameter my_maps in
  let (abs_node, abs_edge) = best_abs_graph in
  let abs_node = remove_nodes abs_node abs_edge in
  ((abs_node, abs_edge), best_score)

and remove_edge_idx best_abs_graph best_score edge_idx parameter my_maps
= if (edge_idx >= 0) then
  let (abs_node, abs_edge) = best_abs_graph in
  let abs_edge = pop abs_edge edge_idx 0 in
  let abs_node = remove_nodes abs_node abs_edge in
  let new_score = update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
  if (new_score >= best_score) then remove_edge_idx (abs_node, abs_edge) new_score (edge_idx-1) parameter my_maps
  else remove_edge_idx best_abs_graph best_score (edge_idx-1) parameter my_maps
else (best_abs_graph, best_score)

and pop _list _idx cnt
= match _list with
| [] -> raise Error
| h::t -> if (_idx = cnt) then t
else h::(pop t _idx (cnt+1))

let rec generalize_nodes_top (abs_node, abs_edge) parameter my_maps current_score
= let node_idx = List.length abs_node - 1 in
  while_node_idx (abs_node, abs_edge) current_score node_idx parameter my_maps
  
and while_node_idx best_abs_graph best_score node_idx parameter my_maps
= if (node_idx >= 0) then 
  let (absNodes, absEdges) = best_abs_graph in
  let new_absNodes = remove_like_array node_idx absNodes 0 in
  let new_score = update_score (new_absNodes, absEdges) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
  if(new_score >= best_score) then while_node_idx (new_absNodes, absEdges) new_score (node_idx-1) parameter my_maps
  else while_node_idx best_abs_graph best_score (node_idx-1) parameter my_maps
else (best_abs_graph, best_score)

let rec generalize_edges_top (abs_node, abs_edge) parameter my_maps current_score
= let edge_idx = List.length abs_edge -1 in
  while_edge_idx (abs_node, abs_edge) current_score edge_idx parameter my_maps

and while_edge_idx best_abs_graph best_score edge_idx parameter my_maps
= if (edge_idx >= 0) then
  let (absNodes, absEdges) = best_abs_graph in
  let new_absEdges = set_new_itv edge_idx absEdges 0 in
  let new_score = update_score (absNodes, new_absEdges) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
  if(new_score >= best_score) then while_edge_idx (absNodes, new_absEdges) new_score (edge_idx-1) parameter my_maps
  else while_edge_idx best_abs_graph best_score (edge_idx-1) parameter my_maps
else (best_abs_graph, best_score)

  (* refine *)

let rec enu_itvs_n _itvs best_abs_graph best_score flag parameter my_maps node_idx
= match _itvs with
  | [] -> (best_abs_graph, best_score, flag)
  | (feat_idx, h)::t -> 
    let (a,b) = List.nth _itvs 0 in
    let (abs_node, abs_edge) = best_abs_graph in

    if(a!= (-99) && b!=99) then 
      let new_itvs = saving_like_array feat_idx (a, 99) _itvs 0 in
      let abs_node = saving_like_array node_idx new_itvs abs_node 0 in
      let new_score = update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
      let new_abs_graph = best_abs_graph in

      if(new_score >= best_score) then 
        let (flag, best_abs_graph, best_score) = (true, new_abs_graph, new_score) in 
        let new_itvs = saving_like_array feat_idx (-99, b) _itvs 0 in
        let abs_node = saving_like_array node_idx new_itvs abs_node 0 in
        let new_score = update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
        if(new_score >= best_score) then 
          let (flag, best_abs_graph, best_score) = (true, (abs_node, abs_edge), new_score) in 
          enu_itvs_n t best_abs_graph best_score flag parameter my_maps node_idx
        else enu_itvs_n t best_abs_graph best_score flag parameter my_maps node_idx
      else 
        let new_itvs = saving_like_array feat_idx (-99, b) _itvs 0 in
        let abs_node = saving_like_array node_idx new_itvs abs_node  0 in
        let new_score = update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
        if(new_score >= best_score) then 
          enu_itvs_n t best_abs_graph best_score flag parameter my_maps node_idx
        else enu_itvs_n t best_abs_graph best_score flag parameter my_maps node_idx

    else if ((a != -99 && b==99) || (a == -99 && b != 99)) then
      let new_itvs = saving_like_array feat_idx (-99, 99) _itvs 0 in
      let abs_node = saving_like_array node_idx new_itvs abs_node 0 in
      let new_score = update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
      if(new_score >= best_score) then 
        let (flag, best_abs_graph, best_score) = (true, (abs_node, abs_edge), new_score) in 
          enu_itvs_n t best_abs_graph best_score flag parameter my_maps node_idx
      else enu_itvs_n t best_abs_graph best_score flag parameter my_maps node_idx
    
    else enu_itvs_n t best_abs_graph best_score flag parameter my_maps node_idx

let rec widening_node (absNodes, absEdges) node_idx (current_abs_graph:abstract_graph) flag best_abs_graph best_score parameter my_maps
= match absNodes with
  | [] -> (current_abs_graph, best_score, flag)
  | h::t -> 
    let (abs_node, abs_edge) = current_abs_graph in
    let _itvs = List.nth abs_node node_idx in
    (*if (_itvs = []) then widening_node (t, absEdges) (node_idx+1) current_abs_graph flag best_abs_graph best_score parameter my_maps
    else*) let (best_abs_graph, best_score, flag) = enu_itvs_n _itvs best_abs_graph best_score flag parameter my_maps h
        in widening_node (t, absEdges) (node_idx+1) current_abs_graph flag best_abs_graph best_score parameter my_maps


let rec enu_itvs_e _itvs best_abs_graph best_score flag p q parameter my_maps edge_idx
= match _itvs with
  | [] -> (best_abs_graph, best_score, flag)
  | (feat_idx, h)::t -> 
    let (a,b) = List.nth _itvs feat_idx in
    let (abs_node, abs_edge) = best_abs_graph in

    if(a != -99 && b != 99) then 
      let new_itvs = saving_like_array feat_idx (a, 99) _itvs 0 in
      let abs_edge = saving_like_array edge_idx (new_itvs, p, q) abs_edge 0 in
      let new_score = update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
      
      if(new_score >= best_score) then 
        let (flag, best_abs_graph, best_score) = (true, (abs_node, abs_edge), new_score) in 
        let new_itvs = saving_like_array feat_idx (-99, b) _itvs 0 in
        let abs_edge = saving_like_array edge_idx (new_itvs, p, q) abs_edge 0 in
        let new_score = update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
        if(new_score >= best_score) then 
          let (flag, best_abs_graph, best_score) = (true, (abs_node, abs_edge), new_score) in 
          enu_itvs_e t best_abs_graph best_score flag p q parameter my_maps edge_idx
        else enu_itvs_e t best_abs_graph best_score flag p q parameter my_maps edge_idx
      else 
        let new_itvs = saving_like_array feat_idx (-99, b) _itvs 0 in
        let abs_edge = saving_like_array edge_idx (new_itvs, p, q) abs_edge 0 in
        let new_score = update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
        if(new_score >= best_score) then 
          let (flag, best_abs_graph, best_score) = (true, (abs_node, abs_edge), new_score) in 
          enu_itvs_e t best_abs_graph best_score flag p q parameter my_maps edge_idx
        else enu_itvs_e t best_abs_graph best_score flag p q parameter my_maps edge_idx
        
    else if ((a != -99 && b = 99) || (a = -99 && b != 99)) then
      let new_itvs = saving_like_array feat_idx (-99, 99) _itvs 0 in
      let abs_edge = saving_like_array edge_idx (new_itvs, p, q) abs_edge 0 in
      let new_score = update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
      if(new_score >= best_score) then 
        let (flag, best_abs_graph, best_score) = (true, (abs_node, abs_edge), new_score) in 
          enu_itvs_e t best_abs_graph best_score flag p q parameter my_maps edge_idx
        else enu_itvs_e t best_abs_graph best_score flag p q parameter my_maps edge_idx

    else enu_itvs_e t best_abs_graph best_score flag p q parameter my_maps edge_idx

let rec widening_edge (absNodes, absEdges) edge_idx current_abs_graph flag best_abs_graph best_score parameter my_maps
= match absEdges with
  | [] -> (best_abs_graph, best_score, flag) 
  | h::t ->
    let (abs_node, abs_edge) = current_abs_graph in
    let (_itvs, p, q) = List.nth abs_edge edge_idx in
    if (empty_list _itvs) then widening_edge (absNodes, t) (edge_idx+1) current_abs_graph flag best_abs_graph best_score parameter my_maps
    else 
      let (best_abs_graph, best_score, flag) = enu_itvs_e _itvs best_abs_graph best_score flag p q parameter my_maps edge_idx
      in widening_edge (absNodes, t) (edge_idx+1) current_abs_graph flag best_abs_graph best_score parameter my_maps

let rec remove_edges best_abs_graph best_score edge_idx flag parameter my_maps 
= if (edge_idx >= 0) then
    let (absNodes, absEdges) = best_abs_graph in
    let absEdges = remove_idx_edge absEdges edge_idx 0 in
    let new_score = update_score best_abs_graph parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
    if (new_score >= best_score) then remove_edges (absNodes, absEdges) new_score (edge_idx-1) true parameter my_maps
    else remove_edges best_abs_graph best_score (edge_idx-1) flag parameter my_maps
  else (best_abs_graph, best_score)

and remove_idx_edge _list idx z
= match _list with
  | [] -> []
  | h::t -> if (idx=z) then t
  else [h]@(remove_idx_edge t idx (z+1))


let rec refine abs_graph parameter my_maps current_score 
= let flag = false in
  let (absNodes, absEdges) = abs_graph in
  let (best_abs_graph, best_score, flag) = widening_node abs_graph 0 abs_graph flag abs_graph current_score parameter my_maps in
  let (best_abs_graph, best_score, flag) = widening_edge abs_graph 0 abs_graph flag best_abs_graph best_score parameter my_maps in
  let (absNodes, absEdges) = abs_graph in
  let (best_abs_graph, best_score) = remove_edges best_abs_graph best_score (List.nth absEdges -1) flag parameter my_maps in
  if (flag) then refine abs_graph parameter my_maps best_score 
  else (best_abs_graph, best_score)


let search_btmUp abs_graph train_graphs ci weight parameter my_maps
= let s = update_score abs_graph parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
  let (abs_nodes, abs_edges) = abs_graph in
  let (best_abs_graph, best_score) = remove_edges_and_nodes abs_graph parameter my_maps s in
  let (best_abs_graph, best_score) = generalize_edges_top best_abs_graph parameter my_maps best_score in
  let (best_abs_graph, best_score) = generalize_nodes_top best_abs_graph parameter my_maps best_score in
  let (best_abs_graph, best_score) = refine best_abs_graph parameter my_maps best_score 
  in best_abs_graph

let rec choose_graph abs_graph graphs my_maps
= let (n,e) = abs_graph in
  let chosen_graphs = choosing_graphs 0 (List.length n) abs_graph graphs my_maps
in chosen_graphs (*set*)

and choosing_graphs ?(step=1) a b abs_graph graphs my_maps
= if a > b then []
  else if (eval_abs_graph_DFS abs_graph graphs my_maps) then (a :: choosing_graphs ~step (a + step) b abs_graph graphs my_maps)
  else choosing_graphs ~step (a + step) b abs_graph graphs my_maps

(* 알고리즘을 위해 필요한 기본 함수 *)
let createInitial_btmUp train_graphs weight parameter my_maps
= let chosen_middle_graph = btmUp_choose_middle parameter.left_graphs parameter.graphs in
  let minimal_abstract_graph = construct_absgraph_undirected parameter my_maps chosen_middle_graph 
in minimal_abstract_graph
(*
let default_score
= let intersection_labeled_and_trained_graphs = List.filter (fun n -> List.mem (List.nth labeled_graphs n) training_graphs) training_graphs
in (List.length intersection_labeled_and_trained_graphs / List.length training_graphs)
*)
(* update_score *)

let rec learning_better train_graphs learned_abstract_graph s ci weight parameter my_maps refine
= if (refine) then
    let candidate_learned_abstract_graphs = search_btmUp learned_abstract_graph train_graphs ci weight parameter my_maps in
    let candidate_s = update_score train_graphs parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
    if (candidate_s >= s) then learning_better train_graphs candidate_learned_abstract_graphs candidate_s ci weight parameter my_maps refine
    else learning_better train_graphs learned_abstract_graph s ci weight parameter my_maps false
else learned_abstract_graph

let synthesize_btmUp train_graphs ci weight parameter my_maps
= let learned_abstract_graph = createInitial_btmUp train_graphs weight parameter my_maps in
  let s = update_score train_graphs parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
  let refine = true in
  let learned_abstract_graph = learning_better train_graphs learned_abstract_graph s ci weight parameter my_maps refine 
in learned_abstract_graph


(*
= if(List.length parameter.left_graphs > 0)
    let s = update_score train_graphs learned_abstract_graphs ci weight labeled_graphs parameter my_maps in
    let chosen_graphs = find_better_graph learned_abstract_graphs train_graphs ci weight s parameter my_maps in
    let parameter.left_graphs = parameter.left_graphs - learned_abstract_graphs in
    let learned_abstract_graphs = learned_abstract_graphs@[chosen_graphs] in

    learning_better train_graphs learned_abstract_graphs ci weight labeled_graphs parameter my_maps
else learned_abstract_graphs*)