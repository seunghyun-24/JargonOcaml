exception Error
module M = Map.Make(Int)

(*
let rec enu_itv itvs x_edge edge
= M.filter (fun n -> let (bot, top) = M.find n itvs in (List.nth (List.nth x_edge edge) n) < bot || top < (List.nth (List.nth x_edge edge) n)) itvs
*)

let rec enu_itv itvs_list x_edge edge
= match itvs_list with
  | [] -> true
  | (key, (bot, top))::t -> if (List.nth (List.nth x_edge edge) key) < bot || top < (List.nth (List.nth x_edge edge) key) then false
  else enu_itv t x_edge edge

let concrete_edge_belong_abs_edge abs_edge edge x_edge
= let (itvs, p, q) = abs_edge in
  enu_itv (M.bindings itvs) x_edge edge

let concrete_node_belong_abs_node abs_node node x_node
= enu_itv (M.bindings abs_node) x_node node

let rec condition_candidate_edges candidate_edges edges my_maps absNodes abs_edge_first 
= match edges with
  | [] -> candidate_edges
  | h::t -> 
    let cond1 = concrete_edge_belong_abs_edge abs_edge_first h my_maps.x_edge in
    let (abs_node_fr, abs_node_to) = abs_edge_first in
    let (e1, e2) = List.nth my_maps.myA h in
    let cond2 = concrete_node_belong_abs_node (List.nth absNode abs_node_fr) e1 my_maps.x_node in
    let cond3 = concrete_node_belong_abs_node (List.nth absNode abs_node_to) e2 my_maps.x_node in
    if (cond1 && cond2 && cond3) then condition_candidate_edges (candidate_edges@[h]) t my_maps absNodes abs_edge_first
    else condition_candidate_edges candidate_edges t my_maps absNodes abs_edge_first



let get_abs_edge_case_and_update_sub_abs_graph sub_abs_graph abs_edge
= let (itv, p, q) = abs_edge in
  let (sub_abs_nodes, sub_abs_edges) = sub_abs_graph in
  if (List.mem p sub_abs_nodes && List.mem q sub_abs_nodes) then
    let sub_abs_edges = sub_abs_edges@[(p,q)] in 
    let sub_abs_graph = (sub_abs_nodes, sub_abs_edges) in
    (sub_abs_graph, 2)
  else if (List.mem q sub_abs_nodes) then
    let sub_abs_nodes = sub_abs_nodes@[p] in
    let sub_abs_edges = sub_abs_edges@[(p,q)] in
    let sub_abs_graph = (sub_abs_nodes, sub_abs_edges) in
    (sub_abs_graph, 1)
  else if (List.mem p sub_abs_nodes) then
    let sub_abs_nodes = sub_abs_nodes@[q] in
    let sub_abs_edges = sub_abs_edges@[(p,q)] in
    let sub_abs_graph = (sub_abs_nodes, sub_abs_edges) in
    (sub_abs_graph, 0) 
  else (sub_abs_graph, (-1))

  
let rec candidating_fr_nodes candidate_fr_nodes subgraph concrete_edge_belong_abs_edge concrete_node_belong_abs_node absNodes abs_node_fr my_maps target_abs_edge abs_edge_idx_to_concrete_edge abs_node_idx_to_concrete_node abs_edge_idx sub_abs_graph abs_graph graph nodes_to_edge
= match candidate_fr_nodes with
  | [] -> true
  | (con_edge, fr_con)::t ->
  let (n, e) = subgraph in
  let condition1 = not (List.mem fr_con n) in
  let (a, b) = List.nth my_maps.myA con_edge in
  let condition2 = concrete_node_belong_abs_node (M.find abs_node_fr absNodes) a my_maps.x_node in
  let condition3 = concrete_edge_belong_abs_edge target_abs_edge con_edge my_maps.x_edge in
  
  if(condition1 && condition2 && condition3) then
    let (itv, e1, e2) = target_abs_edge in
    let new_abs_edge_idx_to_concrete_edge = abs_edge_idx_to_concrete_edge in
    let new_abs_node_idx_to_concrete_node = abs_node_idx_to_concrete_node in
    
    let new_abs_edge_idx_to_concrete_edge = M.add abs_edge_idx con_edge new_abs_edge_idx_to_concrete_edge in
    let new_abs_node_idx_to_concrete_node = M.add e1 fr_con new_abs_node_idx_to_concrete_node in
    
    let (new_node, new_edge) = subgraph in
    let new_node = new_node@[fr_con] in
    let new_edge = new_edge@[con_edge] in
    if (exist_subgraph_DFS (new_node, new_edge) new_sub_abs_graph abs_graph graph (abs_edge_idx + 1) new_abs_node_idx_to_concrete_node new_abs_edge_idx_to_concrete_edge my_maps ) then true
    else candidating_fr_nodes t subgraph concrete_edge_belong_abs_edge concrete_node_belong_abs_node absNodes abs_node_fr my_maps target_abs_edge abs_edge_idx_to_concrete_edge abs_node_idx_to_concrete_node abs_edge_idx sub_abs_graph abs_graph graph nodes_to_edge
  else candidating_fr_nodes t subgraph concrete_edge_belong_abs_edge concrete_node_belong_abs_node absNodes abs_node_fr my_maps target_abs_edge abs_edge_idx_to_concrete_edge abs_node_idx_to_concrete_node abs_edge_idx sub_abs_graph abs_graph graph nodes_to_edge
and 

candidating_to_nodes candidate_to_nodes abs_graph my_maps subgraph abs_node_to target_abs_edge abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge abs_edge_idx sub_abs_graph graph nodes_to_edge
= match candidate_to_nodes with
  | [] -> true
  | h::t ->
    let (absNodes, absEdges) = abs_graph in
    let (itv, e1, e2) = List.nth my_maps.myA con_edge in
    let (con_edge, to_con) = h in
    let (a, b) = List.nth my_maps.myA con_edge in 
    let (n, e) = subgraph in
    let condition1 = not (List.mem to_con (n)) in
    let condition2 = concrete_node_belong_abs_node (M.find abs_node_to absNodes) b my_maps.x_node in
    let condition3 = concrete_edge_belong_abs_edge target_abs_edge con_edge my_maps.x_edge in
    
    if(condition1 && condition2 && condition3) then
      let (itv, e1, e2) = target_abs_edge in
      
      let new_abs_edge_idx_to_concrete_edge = abs_edge_idx_to_concrete_edge in
      let new_abs_node_idx_to_concrete_node = abs_node_idx_to_concrete_node in
      
      let new_abs_edge_idx_to_concrete_edge = M.add e2 to_edge new_abs_node_idx_to_concrete_node in
      let new_abs_node_idx_to_concrete_node = M.add abs_edge_idx con_edge new_abs_edge_idx_to_concrete_edge in
      
    
      let new_subgraph = subgraph in
      let (new_node, new_edge) = new_subgraph in
      let new_node = new_node@[to_con] in
      let new_edge = new_edge@[con_edge] in 
      if(exist_subgraph_DFS (new_node, new_edge) new_sub_abs_graph abs_graph graph abs_edge_idx abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge nodes_to_edge) then true
      else candidating_to_nodes t abs_graph my_maps subgraph abs_node_to target_abs_edge abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge abs_edge_idx sub_abs_graph graph nodes_to_edge
    else candidating_to_nodes t abs_graph my_maps subgraph abs_node_to target_abs_edge abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge abs_edge_idx sub_abs_graph graph nodes_to_edge

and 

(*0 ???????????? true ????????? false*)
exist_subgraph_DFS subgraph sub_abs_graph abs_graph graph abs_edge_idx abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge my_maps
= let (absNodes, absEdges) = abs_graph in
  if (List.length absEdges = abs_edge_idx) then true
  else 
    let target_abs_edge = List.nth absEdges abs_edge_idx in
    let (new_sub_abs_graph, case) = get_abs_edge_case_and_update_sub_abs_graph sub_abs_graph target_abs_edge in
    
    (*
    let (itv_, abs_node_fr, abs_node_to) = target_abs_edge in
    let fr_con = M.find abs_node_fr abs_node_idx_to_concrete_node in
    let to_con = M.find abs_node_to abs_node_idx_to_concrete_node in
    *)
    
  if (case = 2) then
    let (itv_, abs_node_fr, abs_node_to) = target_abs_edge in
    let fr_con = M.find abs_node_fr abs_node_idx_to_concrete_node in
    let to_con = M.find abs_node_to abs_node_idx_to_concrete_node in
    if List.mem (fr_con, to_con) my_maps.nodes_to_edge then 
      let con_edge = M.mem (fr_con, to_con) nodes_to_edge in
      if concrete_edge_belong_abs_edge target_abs_edge con_edge x_edge then
        let new_abs_edge_idx_to_concrete_edge = abs_edge_idx_to_concrete_edge in
        let new_abs_edge_idx_to_concrete_edge = M.find abs_edge_idx con_edge new_abs_edge_idx_to_concrete_edge in
        let (new_node, new_edge) = subgraph in
        let new_edge = new_edge@[con_edge] in
        let new_subgraph = (new_node, new_edge) in
        if (exist_subgraph_DFS new_subgraph new_sub_abs_graph abs_graph graph (abs_edge_idx + 1) abs_node_idx_to_concrete_node new_abs_edge_idx_to_concrete_edge my_maps) then true
        else false
      else false
    else false

  else if (case = 1) then
    let (itv_, abs_node_fr, abs_node_to) = target_abs_edge in
    let to_con = M.find abs_node_to abs_node_idx_to_concrete_node in
    let candidate_fr_nodes = M.find to_con pred_node_to_nodes in
    candidating_fr_nodes candidate_fr_nodes subgraph concrete_edge_belong_abs_edge concrete_node_belong_abs_node absNodes abs_node_fr my_maps target_abs_edge abs_edge_idx_to_concrete_edge abs_node_idx_to_concrete_node abs_edge_idx sub_abs_graph abs_graph graph nodes_to_edge
      
  else if (case = 0) then
      let (itv_, abs_node_fr, abs_node_to) = target_abs_edge in
      let fr_con = M.find abs_node_fr abs_node_idx_to_concrete_node in
      let candidate_to_nodes = List.nth my_maps.succ_node_to_nodes fr_con in
      candidating_to_nodes candidate_to_nodes abs_graph my_maps subgraph abs_node_to target_abs_edge abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge abs_edge_idx sub_abs_graph graph nodes_to_edge

  else false


let rec checking_exist_subgraph_DFS candidate_edges abs_node_fr abs_node_to my_maps abs_graph graph
= match candidate_edges with
  | [] -> false
  | h::t -> 
  let abs_node_idx_to_concrete_node = M.empty in
  let abs_edge_idx_to_concrete_edge = M.empty in
  
  let sub_abs_graph_edge = (abs_node_fr, abs_node_to) in 
  let sub_ags_graph = [[abs_node_fr, abs_node_to], [sub_abs_graph_edge]] in
  
  let (e1, e2) = List.nth my_maps.myA h in
  let subgraph = [ [(e1, e2)], [init_graph_edge]]in

  let abs_node_idx_to_concrete_node = M.add abs_node_fr e1 abs_node_idx_to_concrete_node in
  let abs_node_idx_to_concrete_node = M.add abs_node_to e2 abs_node_idx_to_concrete_node in
  let abs_edge_idx_to_concrete_edge = M.add 0 init_graph_edge abs_edge_idx_to_concrete_edge in
  
  if (exist_subgraph_DFS subgraph sub_abs_graph abs_graph graph 1 abs_node_idx_to_concrete_node abs_edge_idx_to_concrete_edge my_maps) then true
  else checking_exist_subgraph_DFS t abs_node_fr abs_node_to myA abs_graph graph


let eval_abs_graph_DFS my_maps graph abs_graph 
= let (nodes, edges) = graph in
  let (absNodes, absEdges) = abs_graph in
  let abs_edge_first = List.hd absEdges in
  let (abs_node_fr, abs_node_to) = abs_edge_first in
  let candidate_edges = condition_candidate_edges [] edges my_maps absNodes abs_edge_first in
  let bool_exist_subgraph_DFS = checking_exist_subgraph_DFS candidate_edges abs_node_fr abs_node_to my_maps abs_graph graph
in bool_exist_subgraph_DFS
