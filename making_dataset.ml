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
  and abs_node = (int * int) M.t list
  and abs_edge = triple list
  and triple = (int * int) M.t * from_idx * to_idx
  and itv = Itv of int * int
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
  mutable x_edge : int list list;
  mutable x_node: int list list;
  mutable nodes_to_edge : (int) IM.t;
  mutable pred_node_to_nodes : ((int * int) list) M.t;
  mutable succ_node_to_nodes : ((int * int) list) M.t
}

let rec saving_like_array _index _saving _list cnt
= match _list with
  | [] -> if(cnt=_index) then _list@[_saving]
          else saving_like_array _index _saving (_list@[]) (cnt+1)
  | h::t -> if(cnt=_index) then [_saving]@t 
            else h::(saving_like_array _index _saving t (cnt+1))

let rec read_feature oFile features 
= try 
  let line = input_line oFile in
    read_feature oFile (features @ [[Float.of_int(int_of_string line)]]) 
with 
  End_of_file -> features 


let rec read_graph_labels oFile labels 
= try 
  let line = input_line oFile in
    let l = int_of_string line in
    if l == 1 then read_graph_labels oFile (labels @ [1]) 
    else read_graph_labels oFile (labels @ [0]) 
with 
  End_of_file -> labels 


let rec read_indicator oFile indicator
= try 
  let line = input_line oFile in
    read_indicator oFile (indicator @ [(int_of_string line) - 1]) 
with 
  End_of_file -> indicator 


let make_tuple oFileLine 
= let rawline = input_line oFileLine in (*raw line 형태는 'int, int'*)
  let sep = ',' in
  let _str = String.split_on_char sep rawline in
  match _str with
  | [] -> raise InputError
  | h::t -> 
    match t with
      | [] -> raise InputError
      | h2::t -> (int_of_string h, int_of_string (String.trim h2))

let rec read_graph_edge oFile edges
= try
  let line = make_tuple oFile in
    read_graph_edge oFile (edges @ [line])
with 
  End_of_file -> edges


let channel = open_in "MUTAG_node_labels.txt" 
(*let x_node = read_feature channel []
let nodes_len = List.length x_node*)


let node_to_label = M.empty
let rec find_max_node_label oFile features max idx
= try 
    let line = input_line oFile in
      if(int_of_string line > max) then find_max_node_label oFile (M.add idx (int_of_string line) features) (int_of_string line) (idx+1)
      else find_max_node_label oFile (M.add idx (int_of_string line) features) max (idx+1)
  with 
    End_of_file -> (features, max)

let (node_to_label, max_node_label) = find_max_node_label channel node_to_label 0 0


let channel = open_in "MUTAG_edge_labels.txt" 
(*let x_edge = read_feature channel []
let edges_len = List.length x_edge*)

let edge_to_label = M.empty
let (edge_to_label, max_edge_label) = find_max_node_label channel edge_to_label 0 0


let channel = open_in "MUTAG_graph_indicator.txt" 
let indicator = read_indicator channel []
let indicator_len = List.length indicator


let channel = open_in "MUTAG_graph_labels.txt" 
let labels = read_graph_labels channel []
let labels_len = List.length labels 

let channel = open_in "MUTAG_A.txt"
let myA = read_graph_edge channel []
let myA_len = List.length myA


let rec couting_graph_num indicator num 
= match indicator with
  | [] -> num
  | h::[] -> num
  | h::t -> if (h = (List.hd t)) then couting_graph_num t num 
            else couting_graph_num t (num+1)

let graph_num = couting_graph_num indicator 1


let rec make_node first final list
= if(first = final) then list@[first-1]
else make_node (first+1) final (list@[first-1])

let rec make_edge (first:int) (final:int) (myA:(int*int) list) list a
= match myA with 
  | [] -> list
  | (e1,e2)::t -> if( (first<e1) && (final >= e1) && (first<e2) && (final >= e2) ) then make_edge first final t (list@[a]) (a+1)
  else make_edge first final t list (a+1)

let make_ num myA graph_list cnt lcnt
= let n = make_node (lcnt+1) cnt [] in
  let e = make_edge lcnt cnt myA [] lcnt in
  let newGraph = (n,e) in graph_list@[newGraph]

let rec making_graph_edge indicator myA graph_list cnt lcnt
= match indicator with
  | [] -> graph_list
  | h::[] -> let graph_list = make_ h myA graph_list cnt lcnt 
              in graph_list
  | h::t -> if (h = (List.hd t)) then making_graph_edge t myA graph_list (cnt+1) lcnt 
            else 
              let graph_list = make_ h myA graph_list cnt lcnt
              in making_graph_edge t myA graph_list (cnt+1) cnt

let index_graph_list = making_graph_edge indicator myA [] 1 0


(* 이거 잘 안되네  하다보니 밑에 i,, 이거 기능을 안했으,,,, 흥힝홍헹,,,,,,,,,
let rec mk_indicator indicator num graph_to_edges graph_to_nodes node_to_graph
= match indicator with
  | [] -> (graph_to_edges, graph_to_nodes, node_to_graph)
  | h::t -> let node_to_graph = M.add num [(h-1)] node_to_graph in
  if (M.mem (h-1) graph_to_nodes) then
    let graph_to_nodes = M.add (h-1) [num] graph_to_nodes in
    mk_indicator t (num+1) graph_to_edges graph_to_nodes node_to_graph
  else 
    let a = (*M.find (h-1) graph_to_nodes in*) [] in
    let graph_to_nodes = M.add (h-1) (a@[num]) graph_to_nodes in
    mk_indicator t (num+1) graph_to_edges graph_to_nodes node_to_graph

let (graph_to_edges, graph_to_nodes, node_to_graph) = mk_indicator indicator 0 graph_to_edges graph_to_nodes node_to_graph
*)
(* 미래의 나에게.. 이거.. 일일히 mem 체크 후에 find 써야했어.. 그래서 구현하지 않았어.. *) (*
let rec mk_A myA i j graph_to_edges graph_to_nodes
= match myA with
  | [] -> graph_to_edges
  | (fr_node, to_node)::t -> 
    if List.mem fr_node (M.find j graph_to_nodes) then
      let a = M.find j graph_to_nodes in
      let graph_to_edges = M.add j (a@[i]) graph_to_edges in
      mk_A myA (i+1) j graph_to_edges graph_to_nodes
    else if List.mem fr_node (M.find (j+1) graph_to_nodes) then
      let graph_to_edges = M.add (j+1) [i] graph_to_edges in
      mk_A myA (i+1) (j+1) graph_to_edges graph_to_nodes
    else mk_A myA (i+1) j graph_to_edges graph_to_nodes

let graph_to_edges = mk_A myA 0 0 graph_to_edges graph_to_nodes *)


let graph_to_label = M.empty

let rec mk_graphToLabel graph_to_label num labels
= match labels with
  | [] -> graph_to_label
  | h::t -> let graph_to_label = M.add num (h) graph_to_label in
  mk_graphToLabel graph_to_label (num+1) t

let graph_to_label = mk_graphToLabel graph_to_label 0 labels

let rec mk_labeled_graphs bind_graph_to_label labeled_graphs
= match bind_graph_to_label with
  | [] -> labeled_graphs
  | (key, _val)::t -> if(_val = 1) then mk_labeled_graphs t labeled_graphs@[key]
  else mk_labeled_graphs t labeled_graphs

let labeled_graphs = mk_labeled_graphs (M.bindings graph_to_label) []

let rec uniq_list a b 
= match a with
  | [] -> b
  | h::t -> if(List.mem h b) then uniq_list t b
  else uniq_list t (b@[h])

let labeled_graphs = uniq_list labeled_graphs []

let b = M.empty
let rec list_to_map a b 
= match a with
  | [] -> b
  | h::t -> list_to_map t (M.add h h b)
let b = list_to_map labeled_graphs b
let labeled_graphs = b



let nodes_to_edge = IM.empty
let pred_node_to_nodes = M.empty
let succ_node_to_nodes = M.empty

let rec mk_x _list max_node_label len node_to_label
= if (max_node_label+1 > len) then 
  if(List.mem len node_to_label) then mk_x (_list@[1]) max_node_label (len+1) node_to_label
  else mk_x (_list@[0]) max_node_label (len+1) node_to_label
else _list

let rec _Mk_x _list node_to_label max_node_label
= match node_to_label with
  | [] -> _list
  | h::t -> _Mk_x (_list@[(mk_x [] max_node_label 0 node_to_label)]) t max_node_label

let rec map_to_listV after_bindings list
= match after_bindings with
  | [] -> list
  | (_, _val)::t -> map_to_listV t (list@[_val])

let x_node = _Mk_x [] (map_to_listV (M.bindings node_to_label) []) max_node_label
let x_edge = _Mk_x [] (map_to_listV (M.bindings edge_to_label) []) max_edge_label


let rec map_to_list after_bindings list
= match after_bindings with
  | [] -> list
  | (key, _)::t -> map_to_list t (list@[key])

let intersect a b 
= List.filter (fun n -> List.mem n b) a

let rec tuplelist_mem x b 
= match b with
  | [] -> false
  | (k,j)::t -> if (x=k) then true
  else tuplelist_mem x t

let intersect_tuple a b
= List.filter (fun (x,y) -> tuplelist_mem x b) a

let rec map_to_tuplelist after_bindings list
= match after_bindings with
  | [] -> list
  | (key, _val)::t -> map_to_tuplelist t (list@[(key,_val)])

  let rec tuplelist_to_map list map
  = match list with
    | [] -> map
    | (key, _val)::t -> tuplelist_to_map t (M.add key _val map)

let train_graphs = labeled_graphs
let left_graphs = intersect_tuple (map_to_tuplelist (M.bindings labeled_graphs) []) (map_to_tuplelist (M.bindings train_graphs) [])
let left_graphs = tuplelist_to_map left_graphs M.empty

let pred_node_to_nodes = M.empty
let rec fr_make pred_node_to_nodes myA
= match myA with 
  | [] -> pred_node_to_nodes
  | (a,b)::t -> if (not (M.mem a pred_node_to_nodes)) then fr_make (M.add a [(a,b)] pred_node_to_nodes) t
  else _fr_make a b pred_node_to_nodes t

and _fr_make a b pred t 
= let k = M.find a pred in
  fr_make (M.add a (k@[(a,b)]) pred) t

let succ_node_to_nodes = M.empty
let rec to_make succ_node_to_nodes myA
= match myA with 
  | [] -> succ_node_to_nodes
  | (a,b)::t -> if (not (M.mem b pred_node_to_nodes)) then to_make (M.add b [(a,b)] succ_node_to_nodes) t
  else _to_make a b succ_node_to_nodes t

and _to_make a b succ t
= let k = M.find b succ in
  to_make (M.add b (k@[(a,b)]) succ) t

let pred_node_to_nodes = fr_make pred_node_to_nodes myA
let succ_node_to_nodes = to_make succ_node_to_nodes myA

let nodes_to_edge = IM.empty
let rec mk_nodes_to_edge nodes_to_edge myA idx
= match myA with
  | [] -> nodes_to_edge
  | h::t -> mk_nodes_to_edge (IM.add h idx nodes_to_edge) t (idx+1)

let nodes_to_edge = mk_nodes_to_edge nodes_to_edge myA 0

let my_maps = {
  x_node = x_node;
  x_edge = x_edge;
  myA = myA;
  nodes_to_edge = nodes_to_edge;
  pred_node_to_nodes = pred_node_to_nodes;
  succ_node_to_nodes = succ_node_to_nodes
}

let parameter = {
  graphs = index_graph_list;
  train_graphs = train_graphs; (*힝*)
  left_graphs = left_graphs;
  labeled_graphs = left_graphs;
  node_to_label = node_to_label;
  edge_to_label = edge_to_label
}




