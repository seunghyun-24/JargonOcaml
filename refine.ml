module M = Map.Make(Int)
module IntPairs
= struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        | 0 -> Stdlib.compare y0 y1
        | c -> c
      end
module IM = Map.Make(IntPairs)

exception InputError
exception Error

(* M.t = map 의 타입 *)
type abstract_graph = (abs_node) * (abs_edge)
  and abs_node = (float*float) M.t list
  and abs_edge = triple list
  and triple = (float*float) M.t * from_idx * to_idx
  and itv = Itv of float * float 
  and from_idx = int
  and to_idx = int 

type igraphs = igraph list
  and igraph = inode * iedge
  and inode = int list
  and iedge = int list

type parameter = {
  mutable graphs : igraphs;
  mutable left_graphs : int M.t;
  mutable train_graphs : int M.t;
  mutable labeled_graphs : int M.t; (*사실은 set*)
  mutable node_to_label : int M.t;
  mutable edge_to_label : int M.t
}

type my_maps = {
  mutable myA : (int * int) list;
  mutable x_edge : float list list;
  mutable x_node: float list list;
  mutable nodes_to_edge : (int) IM.t;
  mutable pred_node_to_nodes : ((int * int) list) M.t;
  mutable succ_node_to_nodes : ((int * int) list) M.t
}

(* tuple 때문에 사용하는 list 함수 *)
let rec saving_like_array _index _saving _list cnt
= match _list with
  | [] -> if(cnt=_index) then _list@[_saving]
          else saving_like_array _index _saving (_list@[]) (cnt+1)
  | h::t -> if(cnt=_index) then [_saving]@t 
            else h::(saving_like_array _index _saving t (cnt+1)) (* <- 오류가 있습니당 *)



(* *)

let rec enu_itvs_n _itvs best_abs_graph best_score flag parameter my_maps node_idx
= match _itvs with
  | [] -> (best_abs_graph, best_score, flag)
  | (feat_idx, h)::t -> 
    let (a,b) = List.nth _itvs 0 in
    let (abs_node, abs_edge) = best_abs_graph in

    if(a!= (-99) && b!=99) then 
      let new_itvs = saving_like_array feat_idx (a, 99) _itvs 0 in
      let abs_node = saving_like_array node_idx new_itvs abs_node 0 in
      let new_score = 0 in (*update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in*)
      let new_abs_graph = best_abs_graph in

      if(new_score >= best_score) then 
        let (flag, best_abs_graph, best_score) = (true, new_abs_graph, new_score) in 
        let new_itvs = saving_like_array feat_idx (-99, b) _itvs 0 in
        let abs_node = saving_like_array node_idx new_itvs abs_node 0 in
        let new_score = 0 in (*update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in*)
        if(new_score >= best_score) then 
          let (flag, best_abs_graph, best_score) = (true, (abs_node, abs_edge), new_score) in 
          enu_itvs_n t best_abs_graph best_score flag parameter my_maps node_idx
        else enu_itvs_n t best_abs_graph best_score flag parameter my_maps node_idx
      else 
        let new_itvs = saving_like_array feat_idx (-99, b) _itvs 0 in
        let abs_node = saving_like_array node_idx new_itvs abs_node  0 in
        let new_score = 0 in (*update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in*)
        if(new_score >= best_score) then 
          enu_itvs_n t best_abs_graph best_score flag parameter my_maps node_idx
        else enu_itvs_n t best_abs_graph best_score flag parameter my_maps node_idx

    else if ((a != -99 && b==99) || (a == -99 && b != 99)) then
      let new_itvs = saving_like_array feat_idx (-99, 99) _itvs 0 in
      let abs_node = saving_like_array node_idx new_itvs abs_node 0 in
      let new_score = 0 in (* update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in*)
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
      let new_score = 0 in (*update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in*)
      
      if(new_score >= best_score) then 
        let (flag, best_abs_graph, best_score) = (true, (abs_node, abs_edge), new_score) in 
        let new_itvs = saving_like_array feat_idx (-99, b) _itvs 0 in
        let abs_edge = saving_like_array edge_idx (new_itvs, p, q) abs_edge 0 in
        let new_score = 0 in (* update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in*)
        if(new_score >= best_score) then 
          let (flag, best_abs_graph, best_score) = (true, (abs_node, abs_edge), new_score) in 
          enu_itvs_e t best_abs_graph best_score flag p q parameter my_maps edge_idx
        else enu_itvs_e t best_abs_graph best_score flag p q parameter my_maps edge_idx
      else 
        let new_itvs = saving_like_array feat_idx (-99, b) _itvs 0 in
        let abs_edge = saving_like_array edge_idx (new_itvs, p, q) abs_edge 0 in
        let new_score = 0 in (*update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in*)
        if(new_score >= best_score) then 
          let (flag, best_abs_graph, best_score) = (true, (abs_node, abs_edge), new_score) in 
          enu_itvs_e t best_abs_graph best_score flag p q parameter my_maps edge_idx
        else enu_itvs_e t best_abs_graph best_score flag p q parameter my_maps edge_idx
        
    else if ((a != -99 && b = 99) || (a = -99 && b != 99)) then
      let new_itvs = saving_like_array feat_idx (-99, 99) _itvs 0 in
      let abs_edge = saving_like_array edge_idx (new_itvs, p, q) abs_edge 0 in
      let new_score = 0 in (*update_score (abs_node, abs_edge) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in*)
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
    let new_score = 0 in (*update_score best_abs_graph parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in*)
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
