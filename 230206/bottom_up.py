def eval_abs_graph_on_graphs_GC(abs_graph, graphs, labeled_graphs, left_graphs, train_graphs, my_maps):
  correct_set = set()
  incorrect_set = set()

  max_score = 0

  abs_edges_len = len(abs_graph.absEdges)

  for i, graph in enumerate(graphs):
    if not (i in train_graphs):
      continue
    #nodes_len = len(graph[0])
    edges_len = len(graph[1])

    if (abs_edges_len > edges_len):
      continue
    exists = eval_abs_graph_DFS(abs_graph, graph, my_maps)

    if exists:
      if (i in labeled_graphs):
        correct_set.add(i)
      else:
        incorrect_set.add(i)

  if len(left_graphs & correct_set) == 0:
    print("There is no new one")
    return 0.0

  correct_graphs_len = len(correct_set)
  incorrect_graphs_len = len(incorrect_set)
  accuracy =  correct_graphs_len/(correct_graphs_len + incorrect_graphs_len + 1)
  score = accuracy
  
  return score 

def concrete_node_belong_abs_node(abs_node, node, X_node):
  for _, feat_idx in enumerate(abs_node):
    (bot, top) = abs_node[feat_idx]
    if X_node[node][feat_idx] < bot or top < X_node[node][feat_idx]:
      return False
  return True 

def concrete_edge_belong_abs_edge(abs_edge, edge, X_edge):
  #print(abs_edge)
  (itvs, p, q) = abs_edge 
  for _, feat_idx in enumerate(itvs):
    (bot, top) = itvs[feat_idx]
    if X_edge[edge][feat_idx] < bot or top < X_edge[edge][feat_idx]:
      return False
  return True 


def eval_abs_graph_DFS(abs_graph, graph, my_maps):
      #nodes = graph[0]
  edges = graph[1]
  #print(edges)
  abs_edge_first = abs_graph.absEdges[0]
  abs_node_fr = abs_edge_first[1]
  abs_node_to = abs_edge_first[2]
  candidate_edges = set()
  
  for _, edge in enumerate(edges):
    condition1 = concrete_edge_belong_abs_edge(abs_edge_first, edge, my_maps.X_edge)
    condition2 = concrete_node_belong_abs_node(abs_graph.absNodes[abs_node_fr], my_maps.A[edge][0], my_maps.X_node)
    condition3 = concrete_node_belong_abs_node(abs_graph.absNodes[abs_node_to], my_maps.A[edge][1], my_maps.X_node)
    if condition1 and condition2 and condition3:
      candidate_edges.add(edge) 
 
  #print(candidate_edges)

  for _, init_graph_edge in enumerate(candidate_edges):
    abs_node_idx_to_concrete_node = {}
    abs_edge_idx_to_concrete_edge = {}

    sub_abs_graph_edge = (abs_node_fr, abs_node_to)
    sub_abs_graph = [[abs_node_fr, abs_node_to],[sub_abs_graph_edge]]
    subgraph = [[],[]]
    subgraph[0].append(my_maps.A[init_graph_edge][0])
    subgraph[0].append(my_maps.A[init_graph_edge][1])
    subgraph[1].append(init_graph_edge)
    abs_node_idx_to_concrete_node[abs_node_fr] = my_maps.A[init_graph_edge][0]
    abs_node_idx_to_concrete_node[abs_node_to] = my_maps.A[init_graph_edge][1]
    abs_edge_idx_to_concrete_edge[0] = init_graph_edge
    
    if exist_subgraph_DFS(subgraph, sub_abs_graph, abs_graph, graph, 1, abs_node_idx_to_concrete_node, abs_edge_idx_to_concrete_edge, my_maps) == 0:
      return True
  return False


def exist_subgraph_DFS(subgraph, sub_abs_graph, abs_graph, graph, abs_edge_idx, abs_node_idx_to_concrete_node, abs_edge_idx_to_concrete_edge, my_maps):
  #print(subgraph) 
  #sys.exit()

  if len(abs_graph.absEdges) == abs_edge_idx:
    return 0 
  target_abs_edge = abs_graph.absEdges[abs_edge_idx]
  new_sub_abs_graph = copy.deepcopy(sub_abs_graph)
  (new_sub_abs_graph, case) = get_abs_edge_case_and_update_sub_abs_graph(new_sub_abs_graph, target_abs_edge)
  if case == 2:
    abs_node_fr = target_abs_edge[1]
    abs_node_to = target_abs_edge[2]
    fr_con = abs_node_idx_to_concrete_node[abs_node_fr]
    to_con = abs_node_idx_to_concrete_node[abs_node_to]
    if (fr_con, to_con) in my_maps.nodes_to_edge:
      con_edge = my_maps.nodes_to_edge[(fr_con, to_con)]
      #print(con_edge)
      if concrete_edge_belong_abs_edge(target_abs_edge, con_edge, my_maps.X_edge):
        new_abs_edge_idx_to_concrete_edge = copy.deepcopy(abs_edge_idx_to_concrete_edge)
        new_abs_edge_idx_to_concrete_edge[abs_edge_idx] = con_edge 
        new_subgraph = copy.deepcopy(subgraph)
        new_subgraph[1].append(con_edge)
        if exist_subgraph_DFS(new_subgraph, new_sub_abs_graph, abs_graph, graph, abs_edge_idx + 1, abs_node_idx_to_concrete_node, new_abs_edge_idx_to_concrete_edge, my_maps) == 0:
          return 0
  elif case == 1: 
    #new_fr
    abs_node_fr = target_abs_edge[1] # checkthis
    abs_node_to = target_abs_edge[2]
    to_con = abs_node_idx_to_concrete_node[abs_node_to]
    candidate_fr_nodes = my_maps.pred_node_to_nodes[to_con]
    for _, (con_edge, fr_con) in enumerate(candidate_fr_nodes):
      #condition1 = not (fr_node in subgraph[0])
      condition1 = not (fr_con in subgraph[0])
      condition2 = concrete_node_belong_abs_node(abs_graph.absNodes[abs_node_fr], my_maps.A[con_edge][0], my_maps.X_node)
      condition3 = concrete_edge_belong_abs_edge(target_abs_edge, con_edge, my_maps.X_edge)
      if condition1 and condition2 and condition3:
        new_abs_node_idx_to_concrete_node = copy.deepcopy(abs_node_idx_to_concrete_node)
        new_abs_edge_idx_to_concrete_edge = copy.deepcopy(abs_edge_idx_to_concrete_edge)
        new_abs_edge_idx_to_concrete_edge[abs_edge_idx] = con_edge
        new_abs_node_idx_to_concrete_node[target_abs_edge[1]] = fr_con

        new_subgraph = copy.deepcopy(subgraph)
        new_subgraph[0].append(fr_con)
        new_subgraph[1].append(con_edge)
        if exist_subgraph_DFS(new_subgraph, new_sub_abs_graph, abs_graph, graph, abs_edge_idx + 1, new_abs_node_idx_to_concrete_node, new_abs_edge_idx_to_concrete_edge, my_maps) == 0:
          return 0
  elif case == 0:
    #new_to
    abs_node_fr = target_abs_edge[1]
    abs_node_to = target_abs_edge[2]
    fr_con = abs_node_idx_to_concrete_node[abs_node_fr]
    #print(my_maps.succ_node_to_nodes)
    candidate_to_nodes = my_maps.succ_node_to_nodes[fr_con]
    for _, val  in enumerate(candidate_to_nodes):
      (con_edge, to_con) = val
      condition1 = not (to_con in subgraph[0])
      condition2 = concrete_node_belong_abs_node(abs_graph.absNodes[abs_node_to], my_maps.A[con_edge][1], my_maps.X_node)
      condition3 = concrete_edge_belong_abs_edge(target_abs_edge, con_edge, my_maps.X_edge)
      if condition1 and condition2 and condition3:
        new_abs_node_idx_to_concrete_node = copy.deepcopy(abs_node_idx_to_concrete_node)
        new_abs_node_idx_to_concrete_node[target_abs_edge[2]] = to_con
        new_abs_edge_idx_to_concrete_edge = copy.deepcopy(abs_edge_idx_to_concrete_edge)
        new_abs_edge_idx_to_concrete_edge[abs_edge_idx] = con_edge

        new_subgraph = copy.deepcopy(subgraph)
        new_subgraph[0].append(to_con)
        new_subgraph[1].append(con_edge)
        if exist_subgraph_DFS(new_subgraph, new_sub_abs_graph, abs_graph, graph, abs_edge_idx + 1, new_abs_node_idx_to_concrete_node, new_abs_edge_idx_to_concrete_edge, my_maps) == 0:
          return 0
  else:
    raise("Cannot be happened")
  return 1
