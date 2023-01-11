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

let rec update_subgraphs_set subgraphs_set candidate_abs_edges sub_abs_graph abs_graph abs_node_idx_to_concrete_nodes abs_edge_idx_to_concrete_edges myA
= match candidate_abs_edges with
  | [] -> subgraphs_set
  | abs_edge :: tail -> 
    (*ToDo*)
    (*update subAbsGraph*)
    let 
    (*update subgraphs in subgraphs_set*)     
    [] 


let eval_abs_graph abs_graph nodes edges myA x_node x_edge 
= let (abs_node_list, abs_edge_list) = abs_graph in
  let candidate_abs_edges = abs_edge_list in
  let abs_node_idx_to_concrete_nodes = make_filtered_nodes abs_node_list nodes x_node in
  let abs_edge_idx_to_concrete_edges = make_filtered_edges abs_edge_list edges x_edge in
  let sub_abs_graph = ([0],[]) in
  let subgraphs_set = construct_subgraphs (List.nth abs_node_idx_to_concrete_nodes 0) in
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



(*
let rec find_idx _list key idx
= match _list with
  | h::t -> if (List.mem key t) then find_idx t key (idx+1) else idx
  | [] -> (-1)

let choose_an_abs_edge_and_update_sub_abs_graph sub_abs_graph (candidate_abs_edges:triple list)
= let ( _itv, p, q) = (List.hd candidate_abs_edges) in
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


let rec upupgrade_subgraphs subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs num _val
= if (List.length subgraphs > num) then 
    let new_subgraphs = upupgrade_candidate_concrete_edges subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs num _val in
    let new_subgraphs = upupgrade_subgraphs subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs (num+1) 0 in
    new_subgraphs
  else new_subgraphs
  
and 
  
upupgrade_candidate_concrete_edges subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs num _val
= if(List.length candidate_concrete_edges > _val) then begin
    let (p_con, q_con) = List.nth op_a _val in

    if (case = 0 && p_con = List.nth nodes p_sub_abs && (List.mem q_con (List.nth abs_node_idx_to_concrete_nodes q_abs)) && (not (List.mem q_con nodes ))) then
      let e = edges@[(p_con, q_con)] in
      let n = nodes@[q_con] in
      let my_new_subgraph = [ nodes, edges ] in
      let key = my_new_subgraph in
      if (not (List.mem key my_set)) then let my_set = my_set@[key] in let new_subgraphs = new_subgraphs @ my_new_subgraph in 
        upupgrade_candidate_concrete_edges subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs num (_val+1)
      else upupgrade_candidate_concrete_edges subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs num (_val+1)

    else if (case = 1 && p_con = (List.nth nodes q_sub_abs)  && List.mem p_con (List.nth abs_node_idx_to_concrete_nodes p_abs)  && (not (List.mem p_con nodes))) then
      let e = edges@[(p_con, q_con)] in
      let n = nodes@[p_con] in
      let my_new_subgraph = [ nodes, edges ] in 
      let key = my_new_subgraph in
      if (not (List.mem key my_set)) then let my_set = my_set@[key] in let new_subgraphs = new_subgraphs @ my_new_subgraph in 
        upupgrade_candidate_concrete_edges subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs num (_val+1)
      else upupgrade_candidate_concrete_edges subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs num (_val+1)

    else if (case = 2 && p_con = List.nth nodes p_sub_abs && q_con = List.nth nodes q_sub_abs ) then 
      let e = edges@[(p_con, q_con)] in
      let my_new_subgraph = [ nodes, edges ] in 
      let key = my_new_subgraph in
      if (not (List.mem key my_set)) then let my_set = my_set@[key] in let new_subgraphs = new_subgraphs @ my_new_subgraph in 
        upupgrade_candidate_concrete_edges subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs num (_val+1)
      else upupgrade_candidate_concrete_edges subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs num (_val+1)

    else if (case = 3 && List.mem p_con (List.nth abs_node_idx_to_concrete_nodes p_abs)  && List.mem q_con (List.nth abs_node_idx_to_concrete_nodes q_abs) ) then 
      let e = edges@[(p_con, q_con)] in print_int(0);
      let n = nodes@[p_con] in
      let n = n@[q_con] in
      let my_new_subgraph = [ nodes, edges ] in
      let key = my_new_subgraph in
      if (not (List.mem key my_set)) then 
        let my_set = my_set@[key] in 
        let new_subgraphs = new_subgraphs @ my_new_subgraph in 
        let new_subgraphs = upupgrade_candidate_concrete_edges subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs num (_val+1)
        in new_subgraphs
      else let new_subgraphs = upupgrade_candidate_concrete_edges subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs num (_val+1)
          in new_subgraphs
    else upupgrade_candidate_concrete_edges subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs num (_val+1)
  end
else new_subgraphs

let update_subgraphs abs_edge sub_graph_node_indices subgraphs sub_abs_graph case abs_edge_idx_to_concrete_edges abs_node_idx_to_concrete_nodes op_a
= let (p_abs, q_abs, abs_edge_idx) = abs_edge in 
  let (p_sub_abs, q_sub_abs) = sub_graph_node_indices in

  let my_set = [] in
  let new_subgraphs = [[], []] in
  let candidate_concrete_edges = List.nth abs_edge_idx_to_concrete_edges abs_edge_idx in
  let new_subgraphs = upupgrade_subgraphs subgraphs candidate_concrete_edges op_a case nodes p_sub_abs abs_node_idx_to_concrete_nodes q_abs edges my_set q_sub_abs p_abs new_subgraphs 0 0
in new_subgraphs


let rec save_subgraphs subgraphs (candidate_abs_edges:triple list) sub_abs_graph abs_edge_idx_to_concrete_edges abs_node_idx_to_concrete_nodes op_a
= if (List.length candidate_abs_edges > 0 ) then
  let (abs_edge, sub_abs_graph_edge, sub_abs_graph, case) = choose_an_abs_edge_and_update_sub_abs_graph sub_abs_graph candidate_abs_edges
  in let candidate_abs_edges = List.tl candidate_abs_edges
  in let subgraphs = update_subgraphs abs_edge sub_abs_graph_edge subgraphs sub_abs_graph case abs_edge_idx_to_concrete_edges abs_node_idx_to_concrete_nodes op_a
  in save_subgraphs subgraphs candidate_abs_edges sub_abs_graph abs_edge_idx_to_concrete_edges abs_node_idx_to_concrete_nodes op_a
else subgraphs



let rec make_filtered_nodes abs_node_list nodes x_node abs_node_idx_to_concrete_nodes
= match abs_node_list with
  | abs_node::t -> let abs_node_idx_to_concrete_nodes = abs_node_idx_to_concrete_nodes @ [(eval_abs_node abs_node nodes x_node)] in make_filtered_nodes t nodes x_node abs_node_idx_to_concrete_nodes
  | [] -> abs_node_idx_to_concrete_nodes

let rec make_filtered_edges abs_edge_list edges x_edge abs_edge_idx_to_concrete_edges
= match abs_edge_list with
  | abs_edge::t -> let abs_edge_idx_to_concrete_edges = abs_edge_idx_to_concrete_edges @ [(eval_abs_edge abs_edge edges x_edge)] in make_filtered_edges t edges x_edge abs_edge_idx_to_concrete_edges
  | [] -> abs_edge_idx_to_concrete_edges

let eval_abs_graph abs_graph nodes edges op_a x_node x_edge 
= let [abs_node_list, abs_edge_list] = abs_graph in
  let candidate_abs_edges = abs_edge_list in
  let abs_node_idx_to_concrete_nodes = [] in
  let abs_edge_idx_to_concrete_edges = [] in
  let abs_node_idx_to_concrete_nodes = make_filtered_nodes abs_node_list nodes x_node abs_node_idx_to_concrete_nodes in
  let abs_edge_idx_to_concrete_edges = make_filtered_edges abs_edge_list edges x_edge abs_edge_idx_to_concrete_edges in
  let sub_abs_graph = [[],[]] in
  let subgraphs = [[],[]]
  in let subgraphs = (save_subgraphs subgraphs candidate_abs_edges sub_abs_graph abs_edge_idx_to_concrete_edges abs_node_idx_to_concrete_nodes op_a) in
  subgraphs

let subgraphs = List.tl (eval_abs_graph abs_graph0 nodes edges [(0,0); (1,0)] x_node x_edge) *)
