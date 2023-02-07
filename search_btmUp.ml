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

type abstract_graph = (abs_node) * (abs_edge)
  and abs_node = (float * float) M.t list
  and abs_edge = triple list
  and triple = (float * float) M.t * from_idx * to_idx
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
  mutable edge_to_label : int M.t (**)
}

type my_maps = {
  mutable myA : (int * int) list;
  mutable x_edge : float list list;
  mutable x_node: float list list;
  mutable nodes_to_edge : (int) IM.t;
  mutable pred_node_to_nodes : ((int * int) list) M.t;
  mutable succ_node_to_nodes : ((int * int) list) M.t
}

(**)

let rec mem_triple _triple key
= match _triple with
| [_, (), ()] -> false
| (itv, e1, e2)::t -> if (M.mem key itv) then true else mem_triple t key
| _ -> false

let rec saving_like_array _index _saving _list cnt
= match _list with
  | [] -> if(cnt=_index) then _list@[_saving]
          else saving_like_array _index _saving (_list@[]) (cnt+1)
  | h::t -> if(cnt=_index) then [_saving]@t 
            else h::(saving_like_array _index _saving t (cnt+1)) (* <- 오류가 있습니당 *)

let rec remove_like_array _index _list cnt
= match _list with
  | [] -> if(cnt=_index) then _list
          else remove_like_array _index (_list@[]) (cnt+1)
  | h::t -> if(cnt=_index) then t 
            else h::(remove_like_array _index t (cnt+1)) (* <- 오류가 있습니당 *)

(**)

let rec remove_nodes nodes edges 
= match nodes with
  | [] -> nodes
  | h::t -> 
    if (mem_triple edges h) then h::(remove_nodes t edges)
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

and set_new_itv edge_idx absEdges num
= let (_itv, new_from, new_to) = List.nth absEdges edge_idx in
  let k = (M.empty, new_from, new_to) in
  saving_like_array edge_idx k absEdges num 

and while_edge_idx best_abs_graph best_score edge_idx parameter my_maps
= if (edge_idx >= 0) then
  let (absNodes, absEdges) = best_abs_graph in
  let new_absEdges = set_new_itv edge_idx absEdges 0 in
  let new_score = update_score (absNodes, new_absEdges) parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
  if(new_score >= best_score) then while_edge_idx (absNodes, new_absEdges) new_score (edge_idx-1) parameter my_maps
  else while_edge_idx best_abs_graph best_score (edge_idx-1) parameter my_maps
else (best_abs_graph, best_score)

let search_btmUp abs_graph train_graphs ci weight parameter my_maps
= let s = update_score abs_graph parameter.graphs parameter.labeled_graphs parameter.left_graphs parameter.train_graphs my_maps in
  let (abs_nodes, abs_edges) = abs_graph in
  let (best_abs_graph, best_score) = remove_edges_and_nodes abs_graph parameter my_maps s in
  let (best_abs_graph, best_score) = generalize_edges_top best_abs_graph parameter my_maps best_score in
  let (best_abs_graph, best_score) = generalize_nodes_top best_abs_graph parameter my_maps best_score in
  let (best_abs_graph, best_score) = refine best_abs_graph parameter my_maps best_score 
  in best_abs_graph

(*refine 함수는 넘 긴ㅔ..*)