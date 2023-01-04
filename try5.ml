exception NotImplemented
exception CannotBeHappened 

type abstract_graph = abs_node list * abs_edge list
and abs_node = itv list 
and abs_edge = triple list
and triple = itv * from_idx * to_idx
and itv = Itv of float * float 
and from_idx = int
and to_idx = int 



let rec features_belong_to_itvs features itvs 
= match features, itvs with
  | ([],[]) -> true
  | (f :: features', (l,h) :: itvs') -> if f < l || f > h then false else features_belong_to_itvs features' itvs'
  | _ -> raise CannotBeHappened

let eval_abs_node abs_node graph_nodes x_node 
= List.filter (fun n -> features_belong_to_itvs (List.nth x_node n) abs_node) graph_nodes

let abs_node = [(0.0,1.0); (1.0,2.0)]
let nodes = [0;1;2;3]
let x_node = [[0.0;0.0];[1.0;1.0];[2.0;2.0];[3.0;3.0]]

let filtered_nodes = eval_abs_node abs_node nodes x_node



let rec features_belong_to_itvs_e features triple_itvs
= match features, triple_itvs with
  | ([],[]) -> true
  | (f :: features', (itv, from_i, to_i) :: itvs') -> if f < from_i || f > to_i then false else features_belong_to_itvs_e features' itvs'
  | _ -> raise CannotBeHappened

let eval_abs_edge abs_edge graph_edges x_edge
= List.filter (fun n -> feature_belong_to_itvs_e (List.nth x_edge n) abs_edge) graph_edges

let filtered_edges = eval_abs_edge abs_node abs_edge x_edge

def eval_abs_graph(abs_graph, nodes, edges, A, X_node, X_edge):
  
  subgraphs = [] 
  abs_node_idx_to_concrete_nodes = {}
  abs_edge_idx_to_concrete_edges = {}

  for idx, abs_node in enumerate(abs_graph.absNodes):
    abs_node_idx_to_concrete_nodes[idx] = filtered_nodes
  
  sub_abs_graph = [[],[]]

  for _, node in enumerate(abs_node_idx_to_concrete_nodes[0]):
    subgraphs.append(([],[]))

  for idx, abs_edge in enumerate(abs_graph.absEdges):
    abs_edge_idx_to_concrete_edges[idx] = eval_abs_edge(abs_edge, edges, X_edge)
   
  candidate_abs_edges = copy.deepcopy(abs_graph.absEdges) 
  while(len(candidate_abs_edges) > 0):
    (abs_edge, sub_abs_graph_edge, sub_abs_graph, case) = choose_an_abs_edge_and_update_sub_abs_graph(sub_abs_graph, candidate_abs_edges)
    del candidate_abs_edges[0]
    subgraphs = update_subgraphs(abs_edge, sub_abs_graph_edge, subgraphs, sub_abs_graph, case, abs_edge_idx_to_concrete_edges, abs_node_idx_to_concrete_nodes, A)
    
  return subgraphs 

let choose_an_abs_edge_and_update_sub_abs_graph sub_abs_graph candidate_abs_edges
= let ( _itv, fidx, tidx) = List.hd candidate_abs_edges in
  let idx = List.length (List.hd (List.tl sub_abs_graph)) in
  match _ with
  | (List.mem (List.hd sub_abs_graph) fidx && List.mem (List.hd sub_abs_graph) tidx) -> (List.hd (List.tl sub_abs_graph))@(fidx, tidx); let case = 2 in )
  | (List.mem (List.hd sub_abs_graph) tidx) -> (List.hd)@fidx; (List.hd (List.tl sub_abs_graph))@(fidx, tidx); let case = 1 in
  | (List.mem (List.hd sub_abs_graph) fidx) -> (List.hd)@fidx; (List.hd)@tidx; (List.hd (List.tl sub_abs_graph))@(fidx, tidx); let case = 3 in
  | _ -> NotImplemented

  ( (fidx, tidx, idx), List.nth (List.hd sub_abs_graph) fidx  


let eval_abs_graph abs_graph nodes edges a_op x_node x_edge subgraph
= let (abs_node_list, abs_edge_list) = abs_graph in
  let candidate_abs_edges = abs_edge_list in



 (* ToDo *) 