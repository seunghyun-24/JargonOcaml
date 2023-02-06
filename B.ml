let abs_node0 = M.empty
let abs_node0 = M.add 1 [(0.0, 1.0)]
let abs_node0 = M.empty
let abs_node0 = M.add 0 (0.0, 1.0) abs_node0
let abs_node0 = M.add 1 (1.0, 2.0) abs_node0
let abs_node1 = M.empty
let abs_node1 = M.add 0 (-2.0, 2.0) abs_node1
let abs_node1 = M.add 1 (-2.0, 2.0) abs_node1
let abs_nodes = [abs_node0; abs_node1]
let k = M.empty
let k = M.add 0 (0.0, 1.0) k
let k = M.add 1 (0.0, 1.0) k
let abs_edge0 = (k, 0, 1)
let abs_edges = [abs_edge0]
let abs_graph0 = (abs_nodes, abs_edges)

let abs_node0 = M.empty
let abs_node0 = M.add 1 [(0, 1)]
let abs_node0 = M.empty
let abs_node0 = M.add 0 (0, 1) abs_node0
let abs_node0 = M.add 1 (1, 2) abs_node0
let abs_node1 = M.empty
let abs_node1 = M.add 0 (-2, 2) abs_node1
let abs_node1 = M.add 1 (-2, 2) abs_node1
let abs_nodes = [abs_node0; abs_node1]
let k = M.empty
let k = M.add 0 (0, 1) k
let k = M.add 1 (0, 1) k
let abs_edge0 = (k, 0, 1)
let abs_edges = [abs_edge0]
let abs_graph0 = (abs_nodes, abs_edges)

update_score abs_graph0 parameter.graphs parameter my_maps;;