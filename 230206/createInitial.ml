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

let rec map_to_listV after_bindings list
= match after_bindings with
  | [] -> list
  | (_, _val)::t -> map_to_listV t (list@[(_val)])

(*left_graphs를 map_to_listV로 바꾼 뒤 이 함수에 집어넣는다.*)
let rec make_graphs_len_list mk_left_graphs graphs graphs_len_list 
= match mk_left_graphs with
  | [] -> graphs_len_list 
  | h::t -> 
    let (nodes, edges) = List.nth graphs h in 
    make_graphs_len_list t graphs (graphs_len_list@[(h, List.length edges)])

let btmUp_choose_middle left_graphs graphs
= let mk_left_graphs = map_to_listV (M.bindings left_graphs) [] in
  let graphs_len_list = make_graphs_len_list mk_left_graphs graphs [] in
  (*let graphs_len_list_sorted = tuple_sort graphs_len_list in*)
  let graphs_len_list_sorted = List.sort (fun (k1, v1) (k2, v2) -> match compare v1 v2 with | 0 -> compare k1 k2 | c -> c) graphs_len_list in
  let (graph_idx, graph_len) = List.nth graphs_len_list_sorted ((List.length (M.bindings left_graphs))/2)
in graph_idx
  
let rec undi_abs_node nodes my_maps abs_nodes node_abs_node_map cnt 
= match nodes with
  | [] -> (abs_nodes, node_abs_node_map)
  | h::t -> 
    let node_feature = List.nth my_maps.x_node h in let abs_node = _undi_abs_node node_feature [] in
  undi_abs_node t my_maps (abs_nodes@abs_node) (M.add h cnt node_abs_node_map) (cnt+1)

and _undi_abs_node node_feature abs_node 
= match node_feature with
  | [] -> abs_node | h::t -> _undi_abs_node t (abs_node@[(h, h)])

let rec undi_abs_edge edges my_maps node_abs_node_map abs_edges
= match edges with
  | [] -> abs_edges
  | h::t -> let (from_node, to_node) = List.nth my_maps.myA h in
  if(to_node > from_node) then 
    let edge_feature = List.nth my_maps.x_edge h in
    let _itv = M.empty in
    let new_itv = _undi_abs_edge edge_feature _itv 0 in
    if(M.mem from_node node_abs_node_map && M.mem to_node node_abs_node_map) then 
      let abs_edge = (new_itv, M.find from_node node_abs_node_map, M.find to_node node_abs_node_map)
      in undi_abs_edge t my_maps node_abs_node_map (abs_edges@[abs_edge])
    else undi_abs_edge t my_maps node_abs_node_map abs_edges
  else undi_abs_edge t my_maps node_abs_node_map abs_edges

and _undi_abs_edge edge_feature new_itv cnt
= match edge_feature with 
  | [] -> new_itv 
  | h::t -> _undi_abs_edge t (M.add cnt (h, h) new_itv) (cnt+1)

let rec graph_slicing_array graphs graph_idx cnt
  = match graphs with
    | (n,e)::t -> 
      if (graph_idx = cnt) then (n, e)
      else graph_slicing_array t graph_idx (cnt+1)
    | _ -> raise Error 

  let construct_absgraph_undirected parameter my_maps graph_idx
  = let (nodes, edges) = graph_slicing_array parameter.graphs graph_idx 0 in let k = M.empty in
    let (abs_nodes, node_abs_node_map) = undi_abs_node nodes my_maps [] k 0 in
    let abs_edges = undi_abs_edge nodes my_maps node_abs_node_map [] in
    (abs_nodes, abs_edges)

let createInitial_btmUp train_graphs weight parameter my_maps
= let chosen_middle_graph = btmUp_choose_middle parameter.left_graphs parameter.graphs in
  let minimal_abstract_graph = construct_absgraph_undirected parameter my_maps chosen_middle_graph 
in minimal_abstract_graph