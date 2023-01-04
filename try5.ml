exception NotImplemented
exception CannotBeHappened 

type abstract_graph = abs_node list * abs_edge list
and abs_node = itv list 
and abs_edge = triple list
and triple = itv * from_idx * to_idx
and itv = Itv of float * float 
and from_idx = int
and to_idx = int 



let rec features_belong_to_itvs features itvs 
= match features, itvs with
  | ([],[]) -> true
  | (f :: features', (l,h) :: itvs') -> if f < l || f > h then false else features_belong_to_itvs features' itvs'
  | _ -> raise CannotBeHappened

let eval_abs_node abs_node graph_nodes x_node 
= List.filter (fun n -> features_belong_to_itvs (List.nth x_node n) abs_node) graph_nodes

let abs_node = [(0.0,1.0); (1.0,2.0)]
let nodes = [0;1;2;3]
let x_node = [[0.0;0.0];[1.0;1.0];[2.0;2.0];[3.0;3.0]]

let filtered_nodes = eval_abs_node abs_node nodes x_node



let rec features_belong_to_itvs_e features triple_itvs
= match features, triple_itvs with
  | ([],[]) -> true
  | (f :: features', (itv, from_i, to_i) :: itvs') -> if f < from_i || f > to_i then false else features_belong_to_itvs_e features' itvs'
  | _ -> raise CannotBeHappened

let eval_abs_edge abs_edge graph_edges x_edge
= List.filter (fun n -> features_belong_to_itvs_e (List.nth x_edge n) abs_edge) graph_edges

let filtered_edges = eval_abs_edge abs_edge abs_edge x_edge

(*
let up_case_0 nodes edges my_new_subgraph p_con q_con my_set key new_subgraphs candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val
= let my_new_subgraph = [ (nodes, edges) ] in
  List.hd (List.tl my_new_subgraph) @ ([(p_con, q_con)]); (List.hd my_new_subgraph)@q_con; re_update_ my_new_subgraph my_set key new_subgraphs; 
  update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs

let up_case_1 nodes edges my_new_subgraph p_con q_con my_set key new_subgraphs candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val
= let my_new_subgraph = [ (nodes, edges) ] in
  List.hd (List.tl my_new_subgraph) @ ([(p_con, q_con)]); (List.hd my_new_subgraph)@p_con; re_update_ my_new_subgraph my_set key new_subgraphs
  update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs
*)

let re_update_ my_new_subgraph my_set key new_subgraphs
= let key = my_new_subgraph in
  if (List.mem my_set key) then new_subgraphs
  else my_set@[key]; new_subgraphs@[my_new_subgraph]; new_subgraphs

let rec updating_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs
= match nodes, edges with
  | [], [] -> new_subgraphs
  | _, _ -> update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs;
  updating_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val (List.tl nodes) (List.tl edges) my_set new_subgraphs

let rec update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs
= if (_val <= List.length candidate_concrete_edges) then begin
  let (p_con, q_con) = List.nth op_a _val in

  if (case = 0 && p_con = List.nth nodes p_sub_abs && List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con && not (List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con) && not List.nth nodes q_con)
    then let my_new_subgraph = [ (nodes, edges) ] in
      List.hd (List.tl my_new_subgraph) @ ([(p_con, q_con)]); (List.hd my_new_subgraph)@p_con; re_update_ my_new_subgraph my_set key new_subgraphs
      update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs

  else if (case = 1 &&  q_con = List.nth nodes q_sub_abs && List.mem (List.nth abs_node_idx_to_concrete_nodes p_abs) q_con && not List.nth nodes p_con)
    then let my_new_subgraph = [ (nodes, edges) ] in
      List.hd (List.tl my_new_subgraph) @ ([(p_con, q_con)]); (List.hd my_new_subgraph)@p_con; re_update_ my_new_subgraph my_set key new_subgraphs
      update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs
  
  else if (case = 2 && p_con = List.nth nodes p_sub_abs && q_con = List.nth nodes q_sub_abs)
    then let my_new_subgraph = [ (nodes, edges) ] in
    List.hd (List.tl my_new_subgraph) @ ([(p_con, q_con)]); re_update_ my_new_subgraph my_set key new_subgraphs
    update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs
  
  else if (case = 3 && List.mem (List.nth abs_node_idx_to_concrete_nodes p_abs) p_con && List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con)
    then let my_new_subgraph = [ (nodes, edges) ] in
    List.hd (List.tl my_new_subgraph) @ ([(p_con, q_con)]); (List.hd my_new_subgraph)@p_con; (List.hd my_new_subgraph)@q_con; re_update_ my_new_subgraph my_set key new_subgraphs
    update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs
  
  
  else update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs
  end
  else new_subgraphs

let update_subgraphs abs_edge sub_graph_node_indices subgraphs sub_abs_graph case abs_edge_idx_to_concrete_edges abs_node_idx_to_concrete_nodes op_a
= let abs_edge = (f_abs, t_abs, abs_edge_idx) in
  let sub_graph_node_indices = (f_sub_abs, q_sub_abs) in
  let new_subgraphs = [] in
  let candidate_concrete_edges = List.nth abs_edge_idx_to_concrete_edges abs_edge_idx in
  let (nodes, edges) = subgraphs in updating_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a 0 nodes edges [] new_subgraphs

let case_2 sub_abs_graph fidx tidx case idx 
= (List.hd (List.tl sub_abs_graph))@(fidx, tidx); 
  let a = (find_idx (List.hd sub_abs_graph) fidx 0) in 
  let b = (find_idx (List.hd sub_abs_graph) fidx 0) in 
  ( (fidx, tidx, idx), a, b, sub_abs_graph, case )

let case_1 sub_abs_graph fidx tidx case idx 
= (List.hd)@fidx; 
  (List.hd (List.tl sub_abs_graph))@(fidx, tidx); 
  let a = (find_idx (List.hd sub_abs_graph) fidx 0) in 
  let b = (find_idx (List.hd sub_abs_graph) fidx 0) in 
  ( (fidx, tidx, idx), a, b, sub_abs_graph, case )

let case_3 sub_abs_graph fidx tidx case idx
= (List.hd)@fidx; 
  (List.hd)@tidx; 
  (List.hd (List.tl sub_abs_graph))@(fidx, tidx); 
  let a = (find_idx (List.hd sub_abs_graph) fidx 0) in 
  let b = (find_idx (List.hd sub_abs_graph) fidx 0) in 
  ( (fidx, tidx, idx), a, b, sub_abs_graph, case )

let rec find_idx _list key idx
= match _list with
  | h::t -> if (List.mem h key) then idx else find_idx t key (idx+1)
  | [] -> raise CannotBeHappened

let choose_an_abs_edge_and_update_sub_abs_graph sub_abs_graph candidate_abs_edges
= let ( _itv, fidx, tidx) = List.hd candidate_abs_edges in
  let idx = List.length (List.hd (List.tl sub_abs_graph)) in

  if (List.mem (List.hd sub_abs_graph) fidx && List.mem (List.hd sub_abs_graph) tidx) then
    case_2 sub_abs_graph fidx tidx 2 idx 

  else if (List.mem (List.hd sub_abs_graph) tidx) then
    case_1 sub_abs_graph fidx tidx 1 idx

  else if (List.mem (List.hd sub_abs_graph) fidx) then
    case_3 sub_abs_graph fidx tidx 3 idx 

  else raise NotImplemented

let rec save_subgraphs subgraphs candidate_abs_edges sub_abs_graph filtered_edges filtered_nodes op_a
= if( List.length candidate_abs_edges > 0 ) then 
  let (abs_edge, sub_abs_graph_edge, sub_abs_graph, case) = choose_an_abs_edge_and_update_sub_abs_graph sub_abs_graph candidate_abs_edges
  in let candidate_abs_edges = List.tl candidate_abs_edge
  in let subgraphs = update_subgraphs abs_edge sub_graph_node_indices subgraphs sub_abs_graph case filtered_edges filtered_nodes op_a
  in save_subgraphs subraphs candidate_abs_edge sub_abs_graph filtered_edges filtered_nodes op_a
else save_subgraphs 

let eval_abs_graph abs_graph nodes edges op_a x_node x_edge subgraph
= let (abs_node_list, abs_edge_list) = abs_graph in
  let candidate_abs_edges = abs_edge_list 
  in (save_subgraphs subgraph candidate_abs_edges [ [], [] ] filtered_edges filtered_nodes op_a)

 (* ToDo *) 
    
