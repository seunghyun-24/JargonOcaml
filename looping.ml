exception NotImplemented
exception CannotBeHappened 

type abstract_graph = abs_node list * abs_edge list
and abs_node = itv list 
and abs_edge = triple list
and triple = itv list * from_idx * to_idx
and itv = Itv of float * float 
and from_idx = int
and to_idx = int 

(*give the simple test-case*)
let abs_node0 = [Itv (0.0, 1.0); Itv (1.0, 2.0)]
let abs_node1 = [Itv (-2.0, 2.0); Itv (-2.0, 2.0)]
let abs_nodes = [abs_node0 ; abs_node1]
let abs_edge0 = ([Itv (0.0, 1.0); Itv (0.0, 1.0)] , 0, 1)
let abs_edges = [abs_edge0]
let abs_graph0 = (abs_nodes, abs_edges)

(*Case 4*)
let nodes = [0; 1; 2]
let edges = [0; 1]
let myA = [(0, 1); (1, 2)]
let x_node = [[1.0; 1.0];[1.0; 1.0]; [10.0; 10.0]]
let x_edge = [[1.0; 1.0];[1.0; 1.0]]

(*Case 4
  filtered_nodes : - : int list = [0; 1]
  filtered_edges : - : int list = [0; 1] 
*)

let rec features_belong_to_itvs features itvs 
= match features, itvs with
  | ([],[]) -> true
  | (f :: features', Itv (l,h) :: itvs') -> if f < l || f > h then false else features_belong_to_itvs features' itvs'
  | _ -> raise CannotBeHappened

let eval_abs_node abs_node graph_nodes x_node 
= List.filter (fun n -> features_belong_to_itvs (List.nth x_node n) abs_node) graph_nodes

let filtered_nodes = eval_abs_node abs_node0 nodes x_node 

let rec feature _x_edge_list graph_edges abs_edge_itv_list
= match _x_edge_list with
  | [] -> graph_edges
  | h::t -> let graph_edges = (List.filter (fun n -> features_belong_to_itvs h abs_edge_itv_list) graph_edges) in feature t graph_edges abs_edge_itv_list

let eval_abs_edge abs_edge graph_edges x_edge
= let (abs_edge_itv_list, p, q) = abs_edge in 
  List.filter (fun n -> features_belong_to_itvs (List.nth x_edge n) abs_edge_itv_list) graph_edges

let filtered_edges = eval_abs_edge abs_edge0 edges x_edge

let rec make_filtered_nodes abs_node_list nodes x_node
= match abs_node_list with
  | abs_node :: tail -> (eval_abs_node abs_node nodes x_node) :: (make_filtered_nodes tail nodes x_node) 
  | [] -> []

let rec make_filtered_edges abs_edge_list edges x_edge 
= match abs_edge_list with
  | abs_node :: tail -> (eval_abs_edge abs_node nodes x_node) :: (make_filtered_edges tail edges x_edge) 
  | [] -> [] 

let rec construct_subgraphs first_nodes_list = 
  match first_nodes_list with
  | [] -> []
  | node :: tail -> ([node],[]) :: (construct_subgraphs tail)

  let rec find_idx _list key idx
  = match _list with
    | h::t -> if (List.mem key t) then find_idx t key (idx+1) else idx
    | [] -> (-1)
  
  let choose_an_abs_edge_and_update_sub_abs_graph sub_abs_graph abs_edge
  = let ( _itv, p, q) = abs_edge in
    let [ node_l, edge_l ] = sub_abs_graph in
    let idx = List.length edge_l in
  
    if (List.mem p node_l && List.mem q node_l) then
      let case = 2 in
      let edge_l = (edge_l)@[(p,q)] in
      let sub_abs_graph = [ node_l, edge_l ] in
      ((p, q, idx), (find_idx node_l p 0, find_idx node_l q 0), sub_abs_graph, case)
  
    else if (List.mem q node_l) then
      let case = 1 in
      let node_l = (node_l)@[p] in
      let edge_l = (edge_l)@[(p,q)] in
      let sub_abs_graph = [ node_l, edge_l ] in
      ((p, q, idx), (find_idx node_l p 0, find_idx node_l q 0), sub_abs_graph, case)
  
    else if (List.mem p node_l) then
      let case = 0 in
      let node_l = (node_l)@[q] in
      let edge_l = (edge_l)@[(p,q)] in
      let sub_abs_graph = [ node_l, edge_l ] in
      ((p, q, idx), (find_idx node_l p 0, find_idx node_l q 0), sub_abs_graph, case)
  
    else
      let case = 3 in 
      let node_l = (node_l)@[p]@[q] in
      let edge_l = (edge_l)@[(p,q)] in
      let sub_abs_graph = [ node_l, edge_l ] in
      ((p, q, idx), (find_idx node_l p 0, find_idx node_l q 0), sub_abs_graph, case)  


let rec update_mynew_subgraphs subgraphs_set abs_node_idx_to_concrete_nodes candidate_concrete_edges myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs nodes edges _val
= match candidate_concrete_edges with
  | [] -> new_subgraphs
  | h::t -> let (p_con, q_con) = List.nth myA _val in

  if (case = 0 && p_con = List.nth nodes p_sub_abs && (List.mem q_con (List.nth abs_node_idx_to_concrete_nodes q_abs)) && (not (List.mem q_con nodes ))) then
    let e = edges@[(p_con, q_con)] in
    let n = nodes@[q_con] in
    let my_new_subgraph = [ n, e ] in
    let key = my_new_subgraph in
    if (not (List.mem key my_set)) then let my_set = my_set@[key] in let new_subgraphs = new_subgraphs @ my_new_subgraph in 
      update_mynew_subgraphs subgraphs_set abs_node_idx_to_concrete_nodes t myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs nodes edges (_val+1)
    else update_mynew_subgraphs subgraphs_set abs_node_idx_to_concrete_nodes t myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs nodes edges (_val+1)
    
  else if (case = 1 && p_con = (List.nth nodes q_sub_abs)  && List.mem p_con (List.nth abs_node_idx_to_concrete_nodes p_abs)  && (not (List.mem p_con nodes))) then
    let e = edges@[(p_con, q_con)] in
    let n = nodes@[p_con] in
    let my_new_subgraph = [ n, e ] in 
    let key = my_new_subgraph in
    if (not (List.mem key my_set)) then let my_set = my_set@[key] in let new_subgraphs = new_subgraphs @ my_new_subgraph in 
      update_mynew_subgraphs subgraphs_set abs_node_idx_to_concrete_nodes t myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs nodes edges (_val+1)
    else update_mynew_subgraphs subgraphs_set abs_node_idx_to_concrete_nodes t myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs nodes edges (_val+1)
    
  else if (case = 2 && p_con = List.nth nodes p_sub_abs && q_con = List.nth nodes q_sub_abs ) then 
    let e = edges@[(p_con, q_con)] in
    let my_new_subgraph = [ nodes, e ] in 
    let key = my_new_subgraph in
    if (not (List.mem key my_set)) then let my_set = my_set@[key] in let new_subgraphs = new_subgraphs @ my_new_subgraph in 
      update_mynew_subgraphs subgraphs_set abs_node_idx_to_concrete_nodes t myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs nodes edges (_val+1)
    else update_mynew_subgraphs subgraphs_set abs_node_idx_to_concrete_nodes t myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs nodes edges (_val+1)
    
  else if (case = 3 && List.mem p_con (List.nth abs_node_idx_to_concrete_nodes p_abs)  && List.mem q_con (List.nth abs_node_idx_to_concrete_nodes q_abs) ) then 
    let e = edges@[(p_con, q_con)] in
    let n = nodes@[p_con]@[q_con] in 
    let my_new_subgraph = [ n, e ] in
    let key = my_new_subgraph in
    if (not (List.mem key my_set)) then let my_set = my_set@[key] in let new_subgraphs = new_subgraphs @ my_new_subgraph in 
      update_mynew_subgraphs subgraphs_set abs_node_idx_to_concrete_nodes t myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs nodes edges (_val+1)
    else update_mynew_subgraphs subgraphs_set abs_node_idx_to_concrete_nodes t myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs nodes edges (_val+1)
    
  else update_mynew_subgraphs subgraphs_set abs_node_idx_to_concrete_nodes t myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs nodes edges (_val+1)
    
  
let rec update_new_subgraphs subgraphs_set abs_node_idx_to_concrete_nodes candidate_concrete_edges myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs
= match subgraphs_set with
  | [] -> new_subgraphs
  | (nodes, edges)::tail -> let new_subgraphs = update_mynew_subgraphs subgraphs_set abs_node_idx_to_concrete_nodes candidate_concrete_edges myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs nodes edges 0
    in update_new_subgraphs tail abs_node_idx_to_concrete_nodes candidate_concrete_edges myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs

let update_subgraphs _head sub_abs_graph_edge (subgraphs_set : (int list * (int * int) list) list ) sub_abs_graph case abs_node_idx_to_concrete_nodes abs_edge_idx_to_concrete_edges myA
= let (p_abs, q_abs, abs_edge_idx) = _head in 
  let (p_sub_abs, q_sub_abs) = sub_abs_graph_edge in
  let my_set = [] in
  let new_subgraphs = [[], []] in
  let candidate_concrete_edges = List.nth abs_edge_idx_to_concrete_edges abs_edge_idx in
  let new_subgraphs = update_new_subgraphs subgraphs_set abs_node_idx_to_concrete_nodes candidate_concrete_edges myA case p_sub_abs q_sub_abs p_abs q_abs my_set new_subgraphs
in new_subgraphs

      
let rec update_subgraphs_set subgraphs_set candidate_abs_edges sub_abs_graph abs_graph abs_node_idx_to_concrete_nodes abs_edge_idx_to_concrete_edges myA
= match candidate_abs_edges with
  | [] -> subgraphs_set
  | abs_edge :: tail -> 
    (*ToDo*)
    (*update subAbsGraph*)
    let ( _head, sub_abs_graph_edge, sub_abs_graph, case) = choose_an_abs_edge_and_update_sub_abs_graph sub_abs_graph abs_edge
    (*update subgraphs in subgraphs_set*)
    in let subgraphs_set = update_subgraphs _head sub_abs_graph_edge subgraphs_set sub_abs_graph case abs_node_idx_to_concrete_nodes abs_edge_idx_to_concrete_edges myA
    in update_subgraphs_set subgraphs_set tail sub_abs_graph abs_graph abs_node_idx_to_concrete_nodes abs_edge_idx_to_concrete_edges myA



let eval_abs_graph abs_graph nodes edges myA x_node x_edge 
= let (abs_node_list, abs_edge_list) = abs_graph in
  let candidate_abs_edges = abs_edge_list in
  let abs_node_idx_to_concrete_nodes = make_filtered_nodes abs_node_list nodes x_node in
  let abs_edge_idx_to_concrete_edges = make_filtered_edges abs_edge_list edges x_edge in
  let sub_abs_graph = [[],[]] in
  let subgraphs_set = [([],[])] in (*construct_subgraphs (List.nth abs_node_idx_to_concrete_nodes 0) in*)
  update_subgraphs_set subgraphs_set candidate_abs_edges sub_abs_graph abs_graph abs_node_idx_to_concrete_nodes abs_edge_idx_to_concrete_edges myA


(* abs_graph = abs_node_list * abs_edge_list
  abs_node_list : itv list list 
                  = [[Itv (0., 1.); Itv (1., 2.)]; [Itv (-2., 2.); Itv (-2., 2.)]]
  abs_edge_list : (itv list * int * int) list 
                  = [([Itv (0., 1.); Itv (0., 1.)], 0, 1)]
  abs_node_idx_to_concrete_nodes : int list list 
                                  = [[0; 1]; [0; 1]] 
  abs_edge_idx_to_concrete_edges : int list list 
                                  = [[0; 1]]             
  subgraphs_set : (int list * 'a list) list = [([0], []); ([1], [])]
                  *)


  (*subgraphs_belong_to_abstract_graph construct_subgraphs first_nodes_len in*)

  (*let sub_abs_graph = ([],[]) in
  let first_nodes_len = List.length (List.nth abs_node_idx_to_concrete_nodes 0) in*)
  (*
  let subgraphs = (save_subgraphs subgraphs candidate_abs_edges sub_abs_graph abs_edge_idx_to_concrete_edges abs_node_idx_to_concrete_nodes op_a) in
  1*)
  (*subgraphs*)
let myA = [(0,1); (3,2); (4,5); (5,6); (7,8); (8,9)]
let x_node = [[1.0; 1.0];[1.0; 1.0];[1.0; 1.0];[1.0; 1.0];[1.0; 1.0];[1.0; 1.0];[1.0; 1.0];[1.0; 1.0];[1.0; 1.0];[10.0; 10.0]]
let x_edge = [[1.0; 1.0];[1.0; 1.0];[1.0; 1.0];[1.0; 1.0];[1.0; 1.0];[1.0; 1.0]]

(*Case 1*)
let nodes = [0;1]
let edges = [0]

let case1 = List.tl (eval_abs_graph abs_graph0 nodes edges myA x_node x_edge)

(*Case 2*)
let nodes = [2;3]
let edges = [1]

let case2 = List.tl (eval_abs_graph abs_graph0 nodes edges myA x_node x_edge)

(*Case 3*)
let nodes = [4;5;6]
let edges = [2;3]

let case3 = List.tl (eval_abs_graph abs_graph0 nodes edges myA x_node x_edge)

let nodes = [7;8;9]
let edges = [4;5]

let case4 = List.tl (eval_abs_graph abs_graph0 nodes edges myA x_node x_edge)

(* construct ~ 있을 때
# case1;;
- : (int list * (int * int) list) list =
[([0; 0; 1], [(0, 1)]); ([1; 0; 1], [(0, 1)])]
# case2;;
- : (int list * (int * int) list) list =
[([0; 0; 1], [(0, 1)]); ([1; 0; 1], [(0, 1)])]
# case3;; 
- : (int list * (int * int) list) list =
[([0; 0; 1], [(0, 1)]); ([0; 1; 2], [(1, 2)]); ([1; 0; 1], [(0, 1)]);
([1; 1; 2], [(1, 2)]); ([2; 0; 1], [(0, 1)]); ([2; 1; 2], [(1, 2)])]
# case4;;
- : (int list * (int * int) list) list =
[([0; 0; 1], [(0, 1)]); ([1; 0; 1], [(0, 1)])]
*)

(* construct ~ 없을 때
# case1;;
- : (int list * (int * int) list) list = [([0; 1], [(0, 1)])]
# case2;;
- : (int list * (int * int) list) list = [([0; 1], [(0, 1)])]
# case3;;
- : (int list * (int * int) list) list =
[([0; 1], [(0, 1)]); ([1; 2], [(1, 2)])]
# case4;;
- : (int list * (int * int) list) list = [([0; 1], [(0, 1)])]
*)