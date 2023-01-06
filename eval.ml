let abs_node_idx_to_concrete_nodes = filtered_nodes
let abs_node_idx_to_concrete_edges = filtered_edges

let eval_abs_graph abs_graph nodes edges op_a x_node x_edge 
= let (abs_node_list, abs_edge_list) = abs_graph in
  let candidate_abs_edges = abs_edge_list in
  let sub_abs_graph = [ [], [] ] in
  let subgraphs = [ ([], []) ] in
  in (save_subgraphs subgraphs candidate_abs_edges sub_abs_graph filtered_edges filtered_nodes op_a)

let save_subgraphs subgraphs candidate_abs_edges sub_abs_graph filtered_edges filtered_nodes op_a
= if (List.length candidate_abs_edges > 0 ) then
  let (abs_edge, sub_abs_graph_edge, sub_abs_graph, case) = choose_an_abs_edge_and_update_sub_abs_graph sub_abs_graph candidate_abs_edges
  in let candidate_abs_edges = List.tl candidate_abs_edge
  in let subgraphs = update_subgraphs abs_edge sub_graph_node_indices subgraphs sub_abs_graph case filtered_edges filtered_nodes op_a
  in save_subgraphs subgraphs candidate_abs_edge sub_abs_graph filtered_edges filtered_nodes op_a
else subgraphs 

let choose_an_abs_edge_and_update_sub_abs_graph sub_abs_graph candidate_abs_edges
= let ( _itv, p, q) = List.hd candidate_abs_edges in
  let idx = List.length (List.hd (List.tl sub_abs_graph)) in
  let k = List.hd sub_abs_graph in (*sub_abs_graph[0]*) in

  if (List.mem k p && List.mem k q) then
    let case = 2 in
    let sub_abs_graph = [(List.hd sub_abs_graph)]@[ (List.hd (List.tl sub_abs_graph))@[(p, q)] ]@(List.tl (List.tl sub_abs_graph))

  else if (List.mem (List.hd sub_abs_graph) tidx) then
    case_1 sub_abs_graph fidx tidx 1 idx

  else if (List.mem (List.hd sub_abs_graph) fidx) then
    case_3 sub_abs_graph fidx tidx 3 idx 

  else raise NotImplemented

let update_subgraphs abs_edge sub_graph_node_indices subgraphs sub_abs_graph case filtered_edges filtered_nodes op_a
= 