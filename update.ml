def update_subgraphs(abs_edge, sub_graph_node_indices, subgraphs, sub_abs_graph, case, abs_edge_idx_to_concrete_edges, abs_node_idx_to_concrete_nodes, A):
  (p_abs, q_abs, abs_edge_idx) = abs_edge
  (p_sub_abs, q_sub_abs) = sub_graph_node_indices
  
  my_set = set()
  new_subgraphs = []
  candidate_concrete_edges = abs_edge_idx_to_concrete_edges[abs_edge_idx]


  for _, [nodes, edges] in enumerate(subgraphs):


    for _, val in enumerate(candidate_concrete_edges):
      (p_con, q_con) = A[val]
      if case == 0 and p_con == nodes[p_sub_abs] and (q_con in abs_node_idx_to_concrete_nodes[q_abs]) and not (q_con in nodes):
        my_new_subgraph = copy.deepcopy([nodes,edges])
        my_new_subgraph[1].append((p_con, q_con))
        my_new_subgraph[0].append(q_con)
        
      elif case == 1 and q_con == nodes[q_sub_abs] and (p_con in abs_node_idx_to_concrete_nodes[p_abs]) and not (p_con in nodes):
        my_new_subgraph = copy.deepcopy([nodes,edges])
        my_new_subgraph[1].append((p_con, q_con))
        my_new_subgraph[0].append(p_con)
​
      elif case == 2 and p_con == nodes[p_sub_abs] and q_con == nodes[q_sub_abs]:
        my_new_subgraph = copy.deepcopy([nodes,edges])
        my_new_subgraph[1].append((p_con, q_con))
      
      elif case == 3 and (p_con in abs_node_idx_to_concrete_nodes[p_abs]) and (q_con in abs_node_idx_to_concrete_nodes[q_abs]): 
        my_new_subgraph = copy.deepcopy([nodes,edges])
        my_new_subgraph[1].append((p_con, q_con))
        my_new_subgraph[0].append(p_con)
        my_new_subgraph[0].append(q_con)
​
      else:
        continue
     
      #Why we need this?
      key = (json.dumps(my_new_subgraph))
      if not (key in my_set):
        my_set.add(key)
        new_subgraphs.append(my_new_subgraph)
​
  return new_subgraphs

  let rec update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs case p_sub_abs q_abs
= if (_val <= List.length candidate_concrete_edges) then begin
  let (p_con, q_con) = List.nth op_a _val in

  if (case = 0 && p_con = List.nth nodes p_sub_abs && List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con && not (List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con) && not List.mem nodes q_con)
    then let my_new_subgraph = [ (nodes, edges) ] in
      List.hd (List.tl my_new_subgraph) @ ([(p_con, q_con)]); (List.hd my_new_subgraph)@p_con; re_update_ my_new_subgraph my_set key new_subgraphs
      update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs case p_sub_abs q_abs

  else if (case = 1 &&  q_con = List.nth nodes q_sub_abs && List.mem (List.nth abs_node_idx_to_concrete_nodes p_abs) q_con && not List.nth nodes p_con)
    then let my_new_subgraph = [ (nodes, edges) ] in
      List.hd (List.tl my_new_subgraph) @ ([(p_con, q_con)]); (List.hd my_new_subgraph)@p_con; re_update_ my_new_subgraph my_set key new_subgraphs
      update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs case p_sub_abs q_abs
  
  else if (case = 2 && p_con = List.nth nodes p_sub_abs && q_con = List.nth nodes q_sub_abs)
    then let my_new_subgraph = [ (nodes, edges) ] in
    List.hd (List.tl my_new_subgraph) @ ([(p_con, q_con)]); re_update_ my_new_subgraph my_set key new_subgraphs
    update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs case p_sub_abs q_abs
  
  else if (case = 3 && List.mem (List.nth abs_node_idx_to_concrete_nodes p_abs) p_con && List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con)
    then let my_new_subgraph = [ (nodes, edges) ] in
    List.hd (List.tl my_new_subgraph) @ ([(p_con, q_con)]); (List.hd my_new_subgraph)@p_con; (List.hd my_new_subgraph)@q_con; re_update_ my_new_subgraph my_set key new_subgraphs
    update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs case p_sub_abs q_abs

  
  else update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs case p_sub_abs q_abs
  end
  else new_subgraphs

let rec updating_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs case p_sub_abs q_abs
= match nodes, edges with
  | [], [] -> new_subgraphs
  | _, _ -> update_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val nodes edges my_set new_subgraphs case p_sub_abs;
  updating_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a _val (List.tl nodes) (List.tl edges) my_set new_subgraphs case p_sub_abs


let rec upupupdate_subgraphs _val candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs nodes edges my_set 
= if (_val <= List.length candidate_concrete_edges) then begin
  let (p_con, q_con) = List.nth op_a _val in

  if (case = 0 && p_con = List.nth nodes p_sub_abs && List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con && not (List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con) && not (List.mem nodes q_con) )
    then let my_new_subgraph = [ (nodes, edges) ] in
      (List.hd (List.tl my_new_subgraph))::(p_con, q_con); (List.hd my_new_subgraph)::q_con;
      -
      
  else if (case = 1 &&  q_con = List.nth nodes q_sub_abs && List.mem (List.nth abs_node_idx_to_concrete_nodes p_abs) q_con && not List.nth nodes p_con)
    then let my_new_subgraph = [ (nodes, edges) ] in
    (List.hd (List.tl my_new_subgraph))::(p_con, q_con); (List.hd my_new_subgraph)::p_con;
    
    -

  else if (case = 2 && p_con = List.nth nodes p_sub_abs && q_con = List.nth nodes q_sub_abs)
    then let my_new_subgraph = [ (nodes, edges) ] in
    (List.hd (List.tl my_new_subgraph))::(p_con, q_con);
  
    -
  else if (case = 3 && List.mem (List.nth abs_node_idx_to_concrete_nodes p_abs) p_con && List.mem (List.nth abs_node_idx_to_concrete_nodes q_abs) q_con)
    then let my_new_subgraph = [ (nodes, edges) ] in
    (List.hd (List.tl my_new_subgraph))::(p_con, q_con); (List.hd my_new_subgraph)::p_con; (List.hd my_new_subgraph)::q_con;
    
    -

  else upupupdate_subgraphs
  
and 

let rec upupupdate_subgraphs
= if not List.mem my_set my_new_subgraph 
  then my_set::key; new_subgraphs::my_new_subgraph; 
  else new_subgraphs; 



let upupdate_subgraphs subgraphs 
= match subgraphs with
  | [] ->
  | h::t -> let (nodes, edges) = h in upupupdate_subgraphs

let update_subgraphs abs_edge sub_graph_node_indices subgraphs sub_abs_graph case abs_edge_idx_to_concrete_edges abs_node_idx_to_concrete_nodes op_a
= let (p_abs, q_abs, abs_edge_idx) = abs_edge in
  let (p_sub_abs, q_sub_abs) = sub_graph_node_indices in
  let my_set = [] in
  let new_subgraphs = [] in
  let candidate_concrete_edges = List.nth abs_edge_idx_to_concrete_edges abs_edge_idx in
  let new_subgraphs = upupdate_subgraphs 
in new_subgraphs

  let (nodes, edges) = subgraphs in updating_ candidate_concrete_edges abs_node_idx_to_concrete_nodes op_a 0 nodes edges [] new_subgraphs case p_sub_abs q_abs
