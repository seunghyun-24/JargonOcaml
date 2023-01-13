exception NotImplemented
exception CannotBeHappened 

type abstract_graph = abs_node list * abs_edge list
and abs_node = itv list 
and abs_edge = triple list
and triple = itv list * from_idx * to_idx
and itv = Itv of float * float 
and from_idx = int
and to_idx = int 

(*give the simple test-case*)
let abs_node0 = [Itv (0.0, 1.0); Itv (1.0, 2.0)]
let abs_node1 = [Itv (-2.0, 2.0); Itv (-2.0, 2.0)]
let abs_nodes = [abs_node0 ; abs_node1]
let abs_edge0 = ([Itv (0.0, 1.0); Itv (0.0, 1.0)] , 0, 1)
let abs_edges = [abs_edge0]
let abs_graph0 = [abs_nodes, abs_edges]


let nodes = [0;1;2;3]
let edges = [(0,1);(1,2);(2,3)]
let x_node = [[0.0;0.0];[1.0;1.0];[2.0;2.0];[3.0;3.0]]
let x_edge = [[0.0;0.0];[0.0;0.0];[0.0;0.0]]


(*
let rec features_belong_to_itvs features itvs 
= match features, itvs with
  | ([],[]) -> true
  | (f :: features', (l,h) :: itvs') -> if f < l || f > h then false else features_belong_to_itvs features' itvs'
  | _ -> raise CannotBeHappened

let eval_abs_node abs_node graph_nodes x_node 
= List.filter (fun n -> features_belong_to_itvs (List.nth x_node n) abs_node) graph_nodes
*)


let rec features_belong_to_itvs features itvs 
= match features, itvs with
  | ([],[]) -> true
  | (f :: features', (*Check this*) Itv (l,h) :: itvs') -> if f < l || f > h then false else features_belong_to_itvs features' itvs'
  | _ -> raise CannotBeHappened

let eval_abs_node abs_node graph_nodes x_node 
= List.filter (fun n -> features_belong_to_itvs (List.nth x_node n) abs_node) graph_nodes

let filtered_nodes = eval_abs_node abs_node0 nodes x_node

(*
let rec features_belong_to_itvs_e features triple_itvs (*float_list, triple list*)
= match (features, triple_itvs) with
  | ([], [])-> true
  | (f :: features', Itv (l, h) :: itvs') -> if (f < l || f > h) then false else features_belong_to_itvs_e features' itvs'
  | _ -> raise CannotBeHappened

let eval_abs_edge abs_edge graph_edges x_edge
= let (abs_edge_itv_list, p, q) = abs_edge in
  List.filter (fun n -> features_belong_to_itvs_e (List.nth x_edge n) abs_edge_itv_list) graph_edges

let filtered_edges = eval_abs_edge abs_edge0 edges x_edge
*)

let rec feature_belong_to a b 
= match (a, b) with
  | ([], []) -> true
  | (f::features', Itv (l, h) :: itvs') -> if (f < l || f > h) then false else feature_belong_to features' itvs'
  | _ -> raise CannotBeHappened

let rec feature _x_edge_list graph_edges abs_edge_itv_list
= match _x_edge_list with
  | [] -> graph_edges
  | h::t -> let graph_edges = (List.filter (fun n -> feature_belong_to h abs_edge_itv_list) graph_edges) in feature t graph_edges abs_edge_itv_list

let eval_abs_ed abs_edge graph_edges x_edge
= let (abs_edge_itv_list, p, q) = abs_edge in
  feature x_edge graph_edges abs_edge_itv_list

let filtered_edges = eval_abs_ed abs_edge0 edges x_edge


let rec find_idx _list key indx
= if(List.length _list > indx) then
  if(List.nth _list indx = key) then indx
  else find_idx _list key (indx+1)
else (-1)

let choose_an_abs_edge_and_update_sub_abs_graph sub_abs_graph candidate_abs_edges
= let ( _itv, p, q) = List.hd candidate_abs_edges in
  let idx = List.length (List.hd (List.tl sub_abs_graph)) in
  let k = List.hd sub_abs_graph in (*sub_abs_graph[0]*)

  if (List.mem k p && List.mem k q) then
    let case = 2 in
    let sub_abs_graph = (List.hd sub_abs_graph)::( (List.hd (List.tl sub_abs_graph))::(p) )@(List.tl (List.tl sub_abs_graph)) in
    ((p, q, idx), 0, 0, sub_abs_graph, case)

  else if (List.mem k q) then
    let case = 1 in
    let sub_abs_graph = ((List.hd sub_abs_graph)::p)@(List.tl sub_abs_graph) in
    let sub_abs_graph = (List.hd sub_abs_graph)::((List.hd (List.tl sub_abs_graph))::(p) )@(List.tl (List.tl sub_abs_graph)) in
    ((p, q, idx), 0, 0, sub_abs_graph, case)

  else if (List.mem k p) then
    let case = 0 in
    let sub_abs_graph = ((List.hd sub_abs_graph)::q)@(List.tl sub_abs_graph) in
    let sub_abs_graph = (List.hd sub_abs_graph)::( (List.hd (List.tl sub_abs_graph))::(p) )@(List.tl (List.tl sub_abs_graph)) in
    ((p, q, idx), 0, 0, sub_abs_graph, case)

  else 
    let case = 3 in
    let sub_abs_graph = ((List.hd sub_abs_graph)::p)@(List.tl sub_abs_graph) in
    let sub_abs_graph = ((List.hd sub_abs_graph)::q)@(List.tl sub_abs_graph) in
    let sub_abs_graph = (List.hd sub_abs_graph)::( (List.hd (List.tl sub_abs_graph))::(p) )@(List.tl (List.tl sub_abs_graph)) in
    ((p, q, idx), 0, 0, sub_abs_graph, case)



let rec upupgrade_subgraphs num subgraphs _val op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs new_subgraphs
= if (List.length subgraphs > num) then 
  let new_subgraphs = upupgrade_candidate_concrete_edges num subgraphs 0 op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs new_subgraphs
in new_subgraphs 
else new_subgraphs

and 

upupgrade_candidate_concrete_edges num subgraphs _val op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs new_subgraphs
= if(List.length op_a > _val) then
  
  let (p_con, q_con) = List.nth op_a _val in

  if (case = 0 && p_con = List.nth nodes p_sub_abs && List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con && not (List.mem nodes q_con)) then
    let my_new_subgraph = [ nodes, edges ] in
    let my_new_subgraph = (List.hd my_new_subgraph)::((List.tl my_new_subgraph)@([([p_con], q_con)])) in
    let my_new_subgraph = ([List.hd my_new_subgraph]@[ ([], q_con)] )@(List.tl my_new_subgraph) in
    let key = my_new_subgraph in (
    if not (List.mem my_set key) then 
      let my_set = my_set (*@(key)*) in 
      let new_subgraphs = new_subgraphs@my_new_subgraph in
      upupgrade_candidate_concrete_edges num subgraphs (_val+1) op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs new_subgraphs
    else upupgrade_candidate_concrete_edges num subgraphs (_val+1) op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs new_subgraphs
    )

  else if (case = 1 && q_con = List.nth nodes q_sub_abs && List.mem (List.nth abs_node_idx_to_concrete_nodes p_abs) p_con && !(List.mem nodes p_con)) then
    let my_new_subgraph = [ nodes, edges ] in
    let my_new_subgraph = [List.hd my_new_subgraph]@[(List.tl my_new_subgraph)@([p_con, q_con])] in
    let my_new_subgraph = [[List.hd my_new_subgraph]@[p_con]]@(List.tl my_new_subgraph) in
    let key = my_new_subgraph in
    if not (List.mem my_set key) then let my_set = my_set@(key) in let new_subgraphs = new_subgraphs@my_new_subgraph in
      upupgrade_candidate_concrete_edges num subgraphs (_val+1) op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs new_subgraphs

  else if (case = 2 && p_con = List.nth nodes p_sub_abs && q_con = List.nth nodes q_sub_abs) then 
    let my_new_subgraph = [ nodes, edges ] in
    let my_new_subgraph = [List.hd my_new_subgraph]@[(List.tl my_new_subgraph)@([p_con, q_con])] in
    let key = my_new_subgraph in
    if (! List.mem my_set key) then let my_set = my_set@(key) in let new_subgraphs = new_subgraphs@my_new_subgraph in
      upupgrade_candidate_concrete_edges num subgraphs (_val+1) op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs new_subgraphs

  else if (case = 3 && List.mem (List.nth abs_node_idx_to_concrete_nodes p_abs) p_con && List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con) then 
    let my_new_subgraph = [ nodes, edges ] in
    let my_new_subgraph = [List.hd my_new_subgraph]@[(List.tl my_new_subgraph)@([p_con, q_con])] in
    let my_new_subgraph = [[List.hd my_new_subgraph]@[p_con]]@(List.tl my_new_subgraph) in
    let my_new_subgraph = [[List.hd my_new_subgraph]@[q_con]]@(List.tl my_new_subgraph) in
    let key = my_new_subgraph in
    if (! List.mem my_set key) then let my_set = my_set@(key) in let new_subgraphs = new_subgraphs@my_new_subgraph in
      upupgrade_candidate_concrete_edges num subgraphs (_val+1) op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs new_subgraphs

  else upupgrade_candidate_concrete_edges num subgraphs (_val+1) op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs new_subgraphs

else upupgrade_subgraphs (num+1) subgraphs _val op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs new_subgraphs

let update_subgraphs abs_edge sub_graph_node_indices subgraphs sub_abs_graph case abs_edge_idx_to_concrete_edges abs_node_idx_to_concrete_nodes op_a
= let (p_abs, q_abs, abs_edge_idx) = abs_edge in
  let (p_sub_abs, q_sub_abs) = sub_graph_node_indices in

  let my_set = [] in
  let new_subgraphs = [] in
  let candidate_concrete_edges = List.nth abs_edge_idx_to_concrete_edges abs_edge_idx in
  let new_subgraphs = upupgrade_subgraphs 0 subgraphs 0 op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs new_subgraphs

in new_subgraphs


(*
let abs_node_idx_to_concrete_nodes = filtered_nodes
let abs_node_idx_to_concrete_edges = filtered_edges
*)
let save_subgraphs subgraphs candidate_abs_edges sub_abs_graph filtered_edges filtered_nodes op_a
= if (List.length candidate_abs_edges > 0 ) then
  let (abs_edge, sub_abs_graph_edge, sub_abs_graph, case) = choose_an_abs_edge_and_update_sub_abs_graph sub_abs_graph candidate_abs_edges
  in let candidate_abs_edges = List.tl candidate_abs_edges
  in let subgraphs = update_subgraphs abs_edge sub_graph_node_indices subgraphs sub_abs_graph case filtered_edges filtered_nodes op_a
  in save_subgraphs subgraphs candidate_abs_edges sub_abs_graph filtered_edges filtered_nodes op_a
else subgraphs 

let eval_abs_graph abs_graph nodes edges op_a x_node x_edge 
= let (abs_node_list, abs_edge_list) = abs_graph in
  let candidate_abs_edges = abs_edge_list in
  let sub_abs_graph = [ [], [] ] in
  let subgraphs = [ ([], []) ]
  in (save_subgraphs subgraphs candidate_abs_edges sub_abs_graph filtered_edges filtered_nodes op_a)