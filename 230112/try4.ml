exception NotImplemented
​
type abstract_graph = abstract_nodes list * abstract_edges list
and abstract_nodes = AbsNodes of abs_node list 
and abs_node = Itv of float * float 
and abstract_edges = AbsEdges of int list
and abs_edge = itv * from_idx * to_idx
and itv = Itv of float * float
and from_idx = int
and to_idx = int 
​
​
​
let eval_abs_node = raise NotImplemented (* ToDo *) 
let eval_abs_edge = raise NotImplemented (* ToDo *) 
let eval_abs_graph = raise NotImplemented (* ToDo *) 

let eval_abs_node abs_node nodes x_node 
= match abs_node with
  | h::t -> _eval_abs_node abs_node nodes x_node 0
  | [] -> nodes

let _eval_abs_node abs_node nodes x_node idx
= match abs_node with
  | h::t -> let (bot, top) = h in
            let k = 
            if List.nth (List.nth x_node 0) 0 < bot || 
let filtered_nodes = eval_abs_node channel []



type abstract_graph = abs_node list * abs_edge list
and abs_node = itv list
and abs_edge = triple list
and triple = itv * from_idx * to_idx
and itv = Itv of float * float
and from_idx = int
and to_idx = int

let learn_abs_graph graph c_i graph_i
= let w = c_i 



let rec learn_graph graph_list abs_graph 
= match graph_list with
  | h::t -> let c_i = h in (abs_graph @ (learn_abs_graph h c_i)); learn_graph t abs_graph
  | [] -> abs_graph 


exception NotImplemented
exception CannotBeHappened 
​
type abstract_graph = abs_node list * abs_edge list
and abs_node = itv list 
and abs_edge = triple list
and triple = itv * from_idx * to_idx
and itv = Itv of float * float 
and from_idx = int
and to_idx = int 
​
​
​
let rec features_belong_to_itvs features itvs 
= match features, itvs with
  | ([],[]) -> true
  | (f :: features', (l,h) :: itvs') -> if f < l || f > h then false else features_belong_to_itvs features' itvs'
  | _ -> raise CannotBeHappened
​
let eval_abs_node abs_node graph_nodes x_node 
= List.filter (fun n -> features_belong_to_itvs (List.nth x_node n) abs_node) graph_nodes
​
 
​
let abs_node = [(0.0,1.0); (1.0,2.0)]
let nodes = [0;1;2;3]
let x_node = [[0.0;0.0];[1.0;1.0];[2.0;2.0];[3.0;3.0]]
​
let filtered_nodes = eval_abs_node abs_node nodes x_node
​
​
​
let eval_abs_edge = raise NotImplemented (* ToDo *) 
let eval_abs_graph = raise NotImplemented (* ToDo *) 