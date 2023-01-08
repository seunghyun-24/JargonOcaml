let rec find_idx _list key idx
= match _list with
  | h::t -> if (h = key) then idx else find_idx t key (idx+1)
  | [] -> raise CannotBeHappened

let choose_an_abs_edge_and_update_sub_abs_graph sub_abs_graph candidate_abs_edges
= let ( _itv, p, q) = List.hd candidate_abs_edges in
  let idx = List.length (List.hd (List.tl sub_abs_graph)) in
  let k = List.hd sub_abs_graph in (*sub_abs_graph[0]*)

  if (List.mem k p && List.mem k q) then
    let case = 2 in
    let sub_abs_graph = [(List.hd sub_abs_graph)]@[ (List.hd (List.tl sub_abs_graph))@[(p, q)] ]@(List.tl (List.tl sub_abs_graph)) in
    ((p, q, inx), find_idx k p idx, find_idx k q idx, sub_abs_graph, case)

  else if (List.mem k q) then
    let case = 1 in
    let sub_abs_graph = [(List.hd sub_abs_graph)@[p]]@(List.tl sub_abs_graph) in
    let sub_abs_graph = [(List.hd sub_abs_graph)]@[ (List.hd (List.tl sub_abs_graph))@[(p, q)] ]@(List.tl (List.tl sub_abs_graph)) in
    ((p, q, inx), find_idx k p idx, find_idx k q idx, sub_abs_graph, case)

  else if (List.mem k p) then
    let case = 3 in
    let sub_abs_graph = [(List.hd sub_abs_graph)@[p]]@(List.tl sub_abs_graph) in
    let sub_abs_graph = [(List.hd sub_abs_graph)@[q]]@(List.tl sub_abs_graph) in
    let sub_abs_graph = [(List.hd sub_abs_graph)]@[ (List.hd (List.tl sub_abs_graph))@[(p, q)] ]@(List.tl (List.tl sub_abs_graph)) in
    ((p, q, inx), find_idx k p idx, find_idx k q idx, sub_abs_graph, case)

  else raise NotImplemented

let update_subgraphs abs_edge sub_graph_node_indices subgraphs sub_abs_graph case abs_node_idx_to_concrete_edges abs_node_idx_to_concrete_nodes op_a
= let (p_abs, q_abs, abs_edge_idx) = abs_edge in
  let (p_sub_abs, q_sub_abs) = sub_graph_node_indices in

  let my_set = [] in
  let new_subgraphs = [] in
  let candidate_concrete_edges = List.mem abs_edge_idx_to_concrete_edges abs_edge_idx in
  let new_subgraphs = upupgrade_subgraphs 0 subgraphs 0
in new_subgraphss


let rec upupgrade_subgraphs num subgraphs _val
= if (List.length subgraphs > num) then 
  let new_subgraphs = upupgrade_candidate_concrete_edges num subgraphs 0 op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs
in new_subgraphs 
else new_subgraphs

and 

upupgrade_candidate_concrete_edges num subgraphs _val op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs
= if(List.length op_a > _val) then
  
  let (p_con, q_con) = List.nth op_a _val in

  if (case = 0 && p_con = List.nth nodes p_sub_abs && List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con && !(List.mem nodes q_con)) then
    let my_new_subgraph = [ nodes, edges ] in
    let my_new_subgraph = [List.hd my_new_subgraph]@[(List.tl my_new_subgraph)@([p_con, q_con])] in
    let my_new_subgraph = [[List.hd my_new_subgraph]@[q_con]]@(List.tl my_new_subgraph) in
    let key = my_new_subgraph in
    if (! List.mem my_set key) then let my_set = my_set@(key) in let new_subgraphs = new_subgraphs@my_new_subgraph in
      upupgrade_candidate_concrete_edges num subgraphs (_val+1) op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs

  else if (case = 1 && q_con = List.nth nodes q_sub_abs && List.mem (List.nth abs_node_idx_to_concrete_nodes p_abs) p_con && !(List.mem nodes p_con)) then
    let my_new_subgraph = [ nodes, edges ] in
    let my_new_subgraph = [List.hd my_new_subgraph]@[(List.tl my_new_subgraph)@([p_con, q_con])] in
    let my_new_subgraph = [[List.hd my_new_subgraph]@[p_con]]@(List.tl my_new_subgraph) in
    let key = my_new_subgraph in
    if (! List.mem my_set key) then let my_set = my_set@(key) in let new_subgraphs = new_subgraphs@my_new_subgraph in
      upupgrade_candidate_concrete_edges num subgraphs (_val+1) op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs

  else if (case = 2 && p_con = List.nth nodes p_sub_abs && q_con = List.nth nodes q_sub_abs) then 
    let my_new_subgraph = [ nodes, edges ] in
    let my_new_subgraph = [List.hd my_new_subgraph]@[(List.tl my_new_subgraph)@([p_con, q_con])] in
    let key = my_new_subgraph in
    if (! List.mem my_set key) then let my_set = my_set@(key) in let new_subgraphs = new_subgraphs@my_new_subgraph in
      upupgrade_candidate_concrete_edges num subgraphs (_val+1) op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs

  else if (case = 3 && List.mem (List.nth abs_node_idx_to_concrete_nodes p_abs) p_con && List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con) then 
    let my_new_subgraph = [ nodes, edges ] in
    let my_new_subgraph = [List.hd my_new_subgraph]@[(List.tl my_new_subgraph)@([p_con, q_con])] in
    let my_new_subgraph = [[List.hd my_new_subgraph]@[p_con]]@(List.tl my_new_subgraph) in
    let my_new_subgraph = [[List.hd my_new_subgraph]@[q_con]]@(List.tl my_new_subgraph) in
    let key = my_new_subgraph in
    if (! List.mem my_set key) then let my_set = my_set@(key) in let new_subgraphs = new_subgraphs@my_new_subgraph in
      upupgrade_candidate_concrete_edges num subgraphs (_val+1) op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs

  else upupgrade_candidate_concrete_edges num subgraphs (_val+1) op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs

else upupgrade_subgraphs (num+1) subgraphs _val op_a case p_con nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs q_con edges my_new_subgraph my_set q_sub_abs p_abs
(*
let abs_node_idx_to_concrete_nodes = filtered_nodes
let abs_node_idx_to_concrete_edges = filtered_edges
*)
let save_subgraphs subgraphs candidate_abs_edges sub_abs_graph filtered_edges filtered_nodes op_a
= if (List.length candidate_abs_edges > 0 ) then
  let (abs_edge, sub_abs_graph_edge, sub_abs_graph, case) = choose_an_abs_edge_and_update_sub_abs_graph sub_abs_graph candidate_abs_edges
  in let candidate_abs_edges = List.tl candidate_abs_edge
  in let subgraphs = update_subgraphs abs_edge sub_graph_node_indices subgraphs sub_abs_graph case filtered_edges filtered_nodes op_a
  in save_subgraphs subgraphs candidate_abs_edge sub_abs_graph filtered_edges filtered_nodes op_a
else subgraphs 

let eval_abs_graph abs_graph nodes edges op_a x_node x_edge 
= let (abs_node_list, abs_edge_list) = abs_graph in
  let candidate_abs_edges = abs_edge_list in
  let sub_abs_graph = [ [], [] ] in
  let subgraphs = [ ([], []) ]
  in (save_subgraphs subgraphs candidate_abs_edges sub_abs_graph filtered_edges filtered_nodes op_a)