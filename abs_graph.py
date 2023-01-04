def eval_abs_graph(abs_graph, nodes, edges, A, X_node, X_edge):
  
  subgraphs = [] 
  abs_node_idx_to_concrete_nodes = {}
  abs_edge_idx_to_concrete_edges = {}
​
  for idx, abs_node in enumerate(abs_graph.absNodes):
    abs_node_idx_to_concrete_nodes[idx] = eval_abs_node(abs_node, nodes, X_node)
  
  sub_abs_graph = [[],[]]
  for _, node in enumerate(abs_node_idx_to_concrete_nodes[0]):
    subgraphs.append(([],[]))
​
  for idx, abs_edge in enumerate(abs_graph.absEdges):
    abs_edge_idx_to_concrete_edges[idx] = eval_abs_edge(abs_edge, edges, X_edge)
   
  candidate_abs_edges = copy.deepcopy(abs_graph.absEdges) 
  while(len(candidate_abs_edges) > 0):
    (abs_edge, sub_abs_graph_edge, sub_abs_graph, case) = choose_an_abs_edge_and_update_sub_abs_graph(sub_abs_graph, candidate_abs_edges)
    del candidate_abs_edges[0]
    subgraphs = update_subgraphs(abs_edge, sub_abs_graph_edge, subgraphs, sub_abs_graph, case, abs_edge_idx_to_concrete_edges, abs_node_idx_to_concrete_nodes, A)
    
  return subgraphs 
​
​
​
def choose_an_abs_edge_and_update_sub_abs_graph(sub_abs_graph, candidate_abs_edges):
  (_, p, q) = candidate_abs_edges[0]
  idx = len(sub_abs_graph[1]) 
  if (p in sub_abs_graph[0]) and (q in sub_abs_graph[0]):
    sub_abs_graph[1].append((p, q))
    case = 2
     
  elif (q in sub_abs_graph[0]): 
    sub_abs_graph[0].append(p)
    sub_abs_graph[1].append((p, q))
    case = 1 
 
  elif (p in sub_abs_graph[0]): 
    sub_abs_graph[0].append(q)
    sub_abs_graph[1].append((p, q))
    case = 0
    
  else:
    sub_abs_graph[0].append(p)    
    sub_abs_graph[0].append(q) 
    sub_abs_graph[1].append((p, q))
    case = 3
 
  return ((p, q, idx), (sub_abs_graph[0].index(p), sub_abs_graph[0].index(q)), sub_abs_graph, case)      
​
​
def update_subgraphs(abs_edge, sub_graph_node_indices, subgraphs, sub_abs_graph, case, abs_edge_idx_to_concrete_edges, abs_node_idx_to_concrete_nodes, A):
  (p_abs, q_abs, abs_edge_idx) = abs_edge
  (p_sub_abs, q_sub_abs) = sub_graph_node_indices
  
  #ToDo
  my_set = set()
  new_subgraphs = []
  candidate_concrete_edges = abs_edge_idx_to_concrete_edges[abs_edge_idx]
  for _, [nodes, edges] in enumerate(subgraphs):
    for _, val in enumerate(candidate_concrete_edges):
      (p_con, q_con) = A[val]
      if case == 0 and p_con == nodes[p_sub_abs] and (q_con in abs_node_idx_to_concrete_nodes[q_abs]) and not (q_con in nodes):
        my_new_subgraph = copy.deepcopy([nodes,edges])
        my_new_subgraph[1].append((p_con, q_con))
        my_new_subgraph[0].append(q_con)
        
      elif case == 1 and q_con == nodes[q_sub_abs] and (p_con in abs_node_idx_to_concrete_nodes[p_abs]) and not (p_con in nodes):
        my_new_subgraph = copy.deepcopy([nodes,edges])
        my_new_subgraph[1].append((p_con, q_con))
        my_new_subgraph[0].append(p_con)
​
      elif case == 2 and p_con == nodes[p_sub_abs] and q_con == nodes[q_sub_abs]:
        my_new_subgraph = copy.deepcopy([nodes,edges])
        my_new_subgraph[1].append((p_con, q_con))
      
      elif case == 3 and (p_con in abs_node_idx_to_concrete_nodes[p_abs]) and (q_con in abs_node_idx_to_concrete_nodes[q_abs]): 
        my_new_subgraph = copy.deepcopy([nodes,edges])
        my_new_subgraph[1].append((p_con, q_con))
        my_new_subgraph[0].append(p_con)
        my_new_subgraph[0].append(q_con)
​
      else:
        continue
     
      #Why we need this?
      key = (json.dumps(my_new_subgraph))
      if not (key in my_set):
        my_set.add(key)
        new_subgraphs.append(my_new_subgraph)
​
  return new_subgraphs
​
​