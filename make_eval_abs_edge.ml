exception NotImplemented
exception CannotBeHappened 

type abstract_graph = abs_node list * abs_edge list
and abs_node = itv list 
and abs_edge = triple list
and triple = itv list * from_idx * to_idx
and itv = Itv of float * float 
and from_idx = int
and to_idx = int 

let abs_node0 = [Itv (0.0, 1.0); Itv (1.0, 2.0)]
let abs_node1 = [Itv (-2.0, 2.0); Itv (-2.0, 2.0)]
let abs_nodes = [abs_node0 ; abs_node1]
let abs_edge0 = ([Itv (0.0, 1.0); Itv (0.0, 1.0)] , 0, 1)
let abs_edges = [abs_edge0]
let abs_graph0 = [abs_nodes, abs_edges]

let nodes = [0;1;2;3]
let edges = [(0,1);(1,2);(2,3)]
let x_node = [[0.0;0.0];[1.0;1.0];[2.0;2.0];[3.0;3.0]]
let x_edge = [[0.0;0.0];[0.0;0.0];[0.0;0.0]]

let rec filter p = function
  | [] -> [(0, 0)]
  | h::t -> if p then h :: filter p t else filter p t;;

let rec features_belong_to_itvs_e features triple_itvs (*float_list, triple list*)
= match (features, triple_itvs) with
  | ([], [])-> true
  | (f :: features', Itv (l, h) :: itvs') -> if (f < l || f > h) then false else features_belong_to_itvs_e features' itvs'
  | _ -> raise CannotBeHappened

let eval_abs_edge abs_edge graph_edges x_edge
= let (abs_edge_itv_list, p, q) = abs_edge in
  List.filter (fun n -> features_belong_to_itvs_e (List.nth x_edge n) abs_edge_itv_list) graph_edges

let filtered_edges = eval_abs_edge abs_edge0 edges x_edge

let rec features_belong_to_itvs_ features triple_itvs n (*float_list, triple list*)
= match (features, triple_itvs) with
  | ([], [])-> true
  | (f :: features', Itv (l, h) :: itvs') -> if (List.nth f n < l || List.nth f n > h) then false else features_belong_to_itvs_ features' itvs' (n+1)
  | _ -> raise CannotBeHappened

let eval_abs_ed abs_edge graph_edges x_edge
= let (abs_edge_itv_list, p, q) = abs_edge in
  filter (features_belong_to_itvs_ x_edge abs_edge_itv_list 0) graph_edges

let filt = eval_abs_ed abs_edges abs_edge0 x_edge



(*


let filter_for_int_int_list abs_edge graph_edges x_edge
= let (abs_edge_itv_list, p, q) = abs_edge in 
  match x_edge with
  | [] -> graph_edges
  | h::t -> 
    match h, abs_edge_itv_list with 
    | ([], []) -> graph_edges
    | (f::features', Itv (l, h)::itvs') -> if( f < l || f > h) then 

*)