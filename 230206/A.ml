module M = Map.Make(Int);;

exception InputError
exception Error

(* M.t = map 의 타입 *)
type abstract_graph = abs_node list * abs_edge list
  and abs_node = itv M.t
  and abs_edge = triple list
  and triple = itv M.t * from_idx * to_idx
  and itv = Itv of float * float 
  and from_idx = int
  and to_idx = int 

type igraphs = igraph list
  and igraph = inode * iedge
  and inode = int list
  and iedge = int list

type parameter = {
  mutable graphs : igraphs;
  mutable left_graphs : int list;
  mutable train_graphs : int list;
  mutable labeled_graphs : int list;
  mutable node_to_label : int list;
  mutable edge_to_label : int list
}

type my_maps = {
  mutable myA : (int * int) list;
  mutable x_edge : float list list;
  mutable x_node: float list list
}