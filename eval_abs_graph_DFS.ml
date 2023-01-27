let convert_array _list
= Array.of_list _list

let convert_list _array 
= Array.to_list _array

let rec saving_like_array _index _saving _list cnt
= match _list with
  | [] -> if(cnt=_index) then _list@[_saving]
          else saving_like_array _index _saving (_list@[]) (cnt+1)
  | h::t -> if(cnt=_index) then [_saving]@t 
            else h::(saving_like_array _index _saving t (cnt+1))

let rec enu_itv itvs x_edge edge
= match itvs with
  | [] -> true
  | h::t -> 
    let (bot, top) = h in
  if(List.nth (List.nth x_edge edge) 0 < bot || top < List.nth (List.nth x_edge edge) 0) then false
  else enu_itv t x_edge edge

let concrete_edge_belong_abs_edge abs_edge edge x_edge
= let (itvs, p, q) = abs_edge in
  enu_itv itvs x_edge edge

let concrete_node_belong_abs_node abs_node node x_node
= enu_itv abs_node x_node node

let rec condition_candidate_edges candidate_edges edges abs_edge_first edge x_edge myA absNode abs_node_fr abs_node_to x_node 
= match edges with
  | [] -> candidate_edges
  | h::t -> let cond1 = concrete_edge_belong_abs_edge abs_edge_first h x_edge in
  let (e1, e2) = List.nth myA h in
  let cond2 = concrete_node_belong_abs_node (List.nth absNode abs_node_fr) e1 x_node in
  let cond3 = concrete_node_belong_abs_node (List.nth absNode abs_node_to) e2 x_node in
  if (cond1 && cond2 && cond3) then let candidate_edges = candidate_edges@[h] in condition_candidate_edges candidate_edges t abs_edge_first edge x_edge myA absNode abs_node_fr abs_node_to x_node 
  else condition_candidate_edges candidate_edges t abs_edge_first edge x_edge myA absNode abs_node_fr abs_node_to x_node 

let get_abs_edge_case_and_update_sub_abs_graph sub_abs_graph abs_edge
= let (itv, p, q) = abs_edge in
  let (sub_abs_nodes, sub_abs_edges) = sub_abs_graph in
  if (List.mem p sub_abs_nodes && List.mem q sub_abs_nodes) then
    let sub_abs_edges = sub_abs_edges@[(p,q)] in 
    let sub_abs_graph = (sub_abs_nodes, sub_abs_edges) in
    let case = 2 in (sub_abs_graph, case)
  else if (List.mem q sub_abs_nodes) then
    let sub_abs_nodes = sub_abs_nodes@[p] in
    let sub_abs_edges = sub_abs_edges@[(p,q)] in
    let case = 1 in
    let sub_abs_graph = (sub_abs_nodes, sub_abs_edges) in
    (sub_abs_graph, case)
  else if (List.mem p sub_abs_nodes) then
    let sub_abs_nodes = sub_abs_nodes@[q] in
    let sub_abs_edges = sub_abs_edges@[(p,q)] in
    let sub_abs_graph = (sub_abs_nodes, sub_abs_edges) in
    let case = 0 in (sub_abs_graph, case) 
  else (sub_abs_graph, (-1))

let checking_tuple_mem subgraph k 
= match subgraph with
  | (_, k) -> true
  | (k, _) -> true
  | (_,_) -> false
  
let rec candidating_fr_nodes candidate_fr_nodes fr_con con_edge subgraph concrete_edge_belong_abs_edge concrete_node_belong_abs_node absNodes abs_node_fr myA x_node target_abs_edge x_edge abs_edge_idx_to_concrete_edge abs_node_idx_to_concrete_node abs_edge_idx sub_abs_graph abs_graph graph nodes_to_edge 
= match candidate_fr_nodes with
  | [] -> 1 
  | h::t ->
  let condition1 = not (checking_tuple_mem subgraph fr_con) in
  let condition2 = concrete_node_belong_abs_node (List.nth absNodes abs_node_fr, List.hd (List.nth myA h) x_node) in
  let condition3 = concrete_edge_belong_abs_edge target_abs_edge con_edge x_edge in
  if(condition1 && condition2 && condition3) then
    let (itv, e1, e2) = target_abs_edge in
    let new_abs_edge_idx_to_concrete_edge = abs_edge_idx_to_concrete_edge in
    let new_abs_node_idx_to_concrete_node = abs_node_idx_to_concrete_node in
    let new_abs_edge_idx_to_concrete_edge = saving_like_array abs_edge_idx con_edge new_abs_edge_idx_to_concrete_edge 0 in
    let new_abs_node_idx_to_concrete_node = saving_like_array e1 fr_con new_abs_node_idx_to_concrete_node 0 in
    let (new_node, new_edge) = subgraph in
    let new_node = new_node@fr_con in
    let new_edge = new_edge@con_edge in
    if(exist_subgraph_DFS subgraph sub_abs_graph abs_graph graph abs_edge_idx abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge nodes_to_edge
      ) then 0
    else candidating_fr_nodes candidate_fr_nodes fr_con con_edge subgraph concrete_edge_belong_abs_edge concrete_node_belong_abs_node absNodes abs_node_fr myA x_node target_abs_edge x_edge abs_edge_idx_to_concrete_edge abs_node_idx_to_concrete_node abs_edge_idx sub_abs_graph abs_graph graph nodes_to_edge 
  else candidating_fr_nodes candidate_fr_nodes fr_con con_edge subgraph concrete_edge_belong_abs_edge concrete_node_belong_abs_node absNodes abs_node_fr myA x_node target_abs_edge x_edge abs_edge_idx_to_concrete_edge abs_node_idx_to_concrete_node abs_edge_idx sub_abs_graph abs_graph graph nodes_to_edge 

and 

candidating_to_nodes candidate_to_nodes abs_graph myA con_edge subgraph to_con x_node abs_node_to target_abs_edge x_edge abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge abs_edge_idx sub_abs_graph graph 
= match candidate_to_nodes with
  | [] -> 1 
  | h::t ->
    let (absNodes, absEdges) = abs_graph in
    let (e1, e2) = List.nth myA con_edge in
    let (con_edge, to_con) = h in
    let condition1 = not (checking_tuple_mem subgraph to_con) in
    let condition2 = concrete_node_belong_abs_node (List.nth absNodes abs_node_to) e2 x_node in
    let condition3 = concrete_edge_belong_abs_edge target_abs_edge con_edge x_edge in
    if(condition1 && condition2 && condition3) then
      let (itv, e1, e2) = target_abs_edge in
      let new_abs_node_idx_to_concrete_node = abs_node_idx_to_concrete_node in
      Array.set (Array.of_list new_abs_node_idx_to_concrete_node) e2 to_con;
      let new_abs_node_idx_to_concrete_node = new_abs_node_idx_to_concrete_node in
      let new_abs_edge_idx_to_concrete_edge = abs_edge_idx_to_concrete_edge in
      let new_abs_edge_idx_to_concrete_edge = saving_like_array abs_edge_idx con_edge new_abs_edge_idx_to_concrete_edge 0 in
      let new_subgraph = subgraph in
      let (new_node, new_edge) = new_subgraph in
      let new_node = new_node@[to_con] in
      let new_edge = new_edge@[con_edge] in 
      if(exist_subgraph_DFS subgraph sub_abs_graph abs_graph graph abs_edge_idx abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge nodes_to_edge) then 0
      else candidating_to_nodes candidate_to_nodes abs_graph myA con_edge subgraph to_con x_node abs_node_to target_abs_edge x_edge abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge abs_edge_idx sub_abs_graph graph 
    else candidating_to_nodes candidate_to_nodes abs_graph myA con_edge subgraph to_con x_node abs_node_to target_abs_edge x_edge abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge abs_edge_idx sub_abs_graph graph 

and 

exist_subgraph_DFS subgraph sub_abs_graph abs_graph graph abs_edge_idx abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge nodes_to_edge
= let (absNodes, absEdges) = abs_graph in
  if (List.length absEdges = abs_edge_idx) then false 
  else let target_abs_edge = List.nth absEdges abs_edge_idx in
  let new_sub_abs_graph = sub_abs_graph in
  let (new_sub_abs_graph, case) = get_abs_edge_case_and_update_sub_abs_graph sub_abs_graph abs_edge in
  if (case = 2) then
    let (itv_, abs_node_fr, abs_node_to) = target_abs_edge in
    let fr_con = List.nth abs_node_idx_to_concrete_node abs_node_fr in
    let to_con = List.nth abs_node_idx_to_concrete_node abs_node_to in
    if List.mem (fr_con, to_con) nodes_to_edge then 
      let con_edge = List.nth nodes_to_edge (fr_con, to_con) in
      if concrete_edge_belong_abs_edge target_abs_edge con_edge x_edge then
        let new_abs_edge_idx_to_concrete_edge = abs_edge_idx_to_concrete_edge in
        let new_abs_edge_idx_to_concrete_edge = saving_like_array abs_edge_idx con_edge new_abs_edge_idx_to_concrete_edge 0 in
        let (new_node, new_edge) = subgraph in
        let new_edge = new_edge@[con_edge] in
        let new_subgraph = (new_node, new_edge) in
        if (exist_subgraph_DFS ) then 0
        else 1
      else 1
    else if (case = 1) then
      let (itv_, abs_node_fr, abs_node_to) = target_abs_edge in
      let to_con = List.nth abs_node_idx_to_concrete_node abs_node_to in
      let candidate_fr_nodes = List.nth pred_node_to_nodes to_con in
      let result = candidating_fr_nodes in
      result
    else if (case = 0) then
      let (itv_, abs_node_fr, abs_node_to) = target_abs_edge in
      let fr_con = List.nth abs_node_idx_to_concrete_node abs_node_fr in
      let candidate_to_nodes = succ_node_to_nodes fr_con in
      let result = candidating_to_nodes candidate_to_nodes abs_graph myA con_edge subgraph to_con x_node abs_node_to target_abs_edge x_edge abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge abs_edge_idx sub_abs_graph graph in
      result
    else 1

let rec checking_exist_subgraph_DFS candidate_edges abs_node_fr abs_node_to sub_abs_graph_edge myA init_graph_edge subgraph 
= match candidate_edges with
  | [] -> false
  | h::t -> let abs_node_idx_to_concrete_node = [] in let abs_node_idx_to_concrete_edge = [] in
  let sub_abs_graph_edge = (abs_node_fr, abs_node_to) in let sub_ags_graph = [[abs_node_fr, abs_node_to], [sub_abs_graph_edge]] in
  let subgraph = [] in let (e1, e2) = List.nth myA init_graph_edge in let subgraph = subgraph @ [[e1@[e2]], init_graph_edge] in
  Array.set (Array.of_list abs_node_idx_to_concrete_edge) 0 init_graph_edge;
  let abs_node_idx_to_concrete_edge = abs_node_idx_to_concrete_edge in
  Array.set (Array.of_list abs_node_idx_to_concrete_edge) abs_node_fr e1;
  Array.set (Array.of_list abs_node_idx_to_concrete_edge) abs_node_to e2;
  let abs_node_idx_to_concrete_node = abs_node_idx_to_concrete_edge in
  if(exit_subgraph_DFS ) then true
  else checking_exist_subgraph_DFS t abs_node_fr abs_node_to sub_abs_graph_edge myA init_graph_edge subgraph 

let eval_abs_graph_DFS x_node x_edge myA graph abs_graph 
= let (nodes, edges) = graph in
  let (absNodes, absEdges) = abs_graph in
  let abs_edge_first = List.hd absEdges in
  let (abs_node_fr, abs_node_to) = abs_edge_first in
  let candidate_edges = [] in
  let candidate_edges = condition_candidate_edges candidate_edges edges abs_edge_first edge x_edge myA absNode abs_node_fr abs_node_to x_node in
  let bool_exist_subgraph_DFS = checking_exist_subgraph_DFS candidate_edges abs_node_fr abs_node_to sub_abs_graph_edge myA init_graph_edge subgraph 
in bool_exist_subgraph_DFS
