let rec upupupdate_subgraphs subgraphs _val candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs nodes edges my_set new_subgraphs
= if (_val <= List.length candidate_concrete_edges) then begin
  let (p_con, q_con) = List.nth op_a _val in

  if (case = 0 && p_con = List.nth nodes p_sub_abs && List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con && not (List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con) && not (List.mem nodes q_con) )
    then let my_new_subgraph = [ (nodes, edges) ] in
      (List.hd (List.tl my_new_subgraph))::(p_con, q_con); (List.hd my_new_subgraph)::q_con;
      upupupdate_ subgraphs new_subgraphs key my_new_subgraph _val candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs nodes edges my_set new_subgraphs

      
  else if (case = 1 &&  q_con = List.nth nodes q_sub_abs && List.mem (List.nth abs_node_idx_to_concrete_nodes p_abs) q_con && not List.nth nodes p_con)
    then let my_new_subgraph = [ (nodes, edges) ] in
    (List.hd (List.tl my_new_subgraph))::(p_con, q_con); (List.hd my_new_subgraph)::p_con;
    upupupdate_ subgraphs new_subgraphs key my_new_subgraph _val candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs nodes edges my_set new_subgraphs


  else if (case = 2 && p_con = List.nth nodes p_sub_abs && q_con = List.nth nodes q_sub_abs)
    then let my_new_subgraph = [ (nodes, edges) ] in
    (List.hd (List.tl my_new_subgraph))::(p_con, q_con);
    upupupdate_ subgraphs new_subgraphs key my_new_subgraph _val candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs nodes edges my_set new_subgraphs

  else if (case = 3 && List.mem (List.nth abs_node_idx_to_concrete_nodes p_abs) p_con && List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con)
    then let my_new_subgraph = [ (nodes, edges) ] in
    (List.hd (List.tl my_new_subgraph))::(p_con, q_con); (List.hd my_new_subgraph)::p_con; (List.hd my_new_subgraph)::q_con;
    upupupdate_ subgrpahs new_subgraphs key my_new_subgraph _val candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs nodes edges my_set new_subgraphs

  else upupupdate_subgraphs subgraphs _val candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs nodes edges my_set new_subgraphs
  
and 

let rec upupupdate_ subgrpahs new_subgraphs key my_new_subgraph _val candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs nodes edges my_set
= if not List.mem my_set my_new_subgraph 
  then my_set::key; new_subgraphs::my_new_subgraph; upupupdate_subgraphs subgraphs _val candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs nodes edges my_set 
  else upupupdate_subgraphs subgraphs _val candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs nodes edges my_set new_subgraphs

let upupdate_subgraphs subgraphs _val candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs nodes edges my_set new_subgraphs
= match subgraphs with
  | [] -> new_subgraphs
  | h::t -> let (nodes, edges) = h in upupupdate_subgraphs subgraphs _val candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs nodes edges my_set new_subgraphs

let update_subgraphs abs_edge sub_graph_node_indices subgraphs sub_abs_graph case abs_edge_idx_to_concrete_edges abs_node_idx_to_concrete_nodes op_a
= let (p_abs, q_abs, abs_edge_idx) = abs_edge in
  let (p_sub_abs, q_sub_abs) = sub_graph_node_indices in
  let my_set = [] in
  let new_subgraphs = [] in
  let candidate_concrete_edges = List.nth abs_edge_idx_to_concrete_edges abs_edge_idx in
  let new_subgraphs = upupdate_subgraphs subgraphs _val candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs nodes edges my_set new_subgraphs
in new_subgraphs