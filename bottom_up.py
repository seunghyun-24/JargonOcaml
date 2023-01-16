def learn_abs_graphs_bottom_up(parameter, my_maps):
      print("Left graphs len : {}".format(len(parameter.left_graphs)))
  default_score = float(len(parameter.labeled_graphs & parameter.train_graphs)/len(parameter.train_graphs))
  print("Default score : {}".format(default_score))
  learned_parameters = set()
  while(len(parameter.left_graphs) > 0):
    graph = btm_up_graph_chooser_from_middle(parameter.left_graphs, parameter.graphs) 
    abs_graph = constructAbsGraphUndirected(parameter, graph, my_maps)
    
    while(my_connect(abs_graph) == False):
      print("Is not connected Initial!!!")
      print("Not connected Graph : {}".format(graph))
      parameter.left_graphs.remove(graph)
      graph = btm_up_graph_chooser_from_big(parameter.left_graphs, parameter.graphs)
      abs_graph = constructAbsGraphBBBP(parameter, graph)
      print(my_connect(abs_graph))
      
    print("Given graph")
    print("Nodes : {}".format(parameter.graphs[graph][0]))
    print("Nodes Len : {}".format(len(parameter.graphs[graph][0])))
    print("Edges : {}".format(parameter.graphs[graph][1]))
    print("Edges Len : {}".format(len(parameter.graphs[graph][1])))
    print()
    print()
    print("AbsGraph")
    print("AbsNodes : {}".format(abs_graph.absNodes))
    print("AbsNodes : {}".format(len(abs_graph.absNodes)))
    print("AbsEdges : {}".format(abs_graph.absEdges))
    print("AbsEdges : {}".format(len(abs_graph.absEdges)))
    print("Chosen_graph : {}".format(graph))

    learned_abs_graph = generalize(abs_graph, parameter, my_maps)
    
    score = eval_abs_graph_on_graphs_GC(learned_abs_graph, parameter.graphs, parameter.labeled_graphs, parameter.left_graphs, parameter.train_graphs, my_maps)
    chosen_train_graphs = eval_abs_graph_on_graphs_exist(learned_abs_graph, parameter.graphs, my_maps) & parameter.train_graphs
    
    if (score < default_score * parameter.expected) or (len(chosen_train_graphs) == 1):
      print("This learning failed!!")
      parameter.left_graphs.remove(graph)
      continue
    chosen_graphs = eval_abs_graph_on_graphs_exist(learned_abs_graph, parameter.graphs, my_maps)
    print()
    print("Covered graphs : {}".format(chosen_graphs))
    print("Covered graphs len : {}".format(len(chosen_graphs)))
    print()
    parameter.left_graphs = parameter.left_graphs - chosen_graphs
    learned_parameters.add(learned_abs_graph)
    print("Left graphs len : {}".format(len(parameter.left_graphs)))
  return learned_parameters 


def sort_abs_graph_edges(abs_graph):
      current_abs_edges = copy.deepcopy(abs_graph.absEdges)
  candidate_edges = current_abs_edges
  new_absEdges = [current_abs_edges[0]]

  reachable = set()
  reachable.add(current_abs_edges[0][1])
  reachable.add(current_abs_edges[0][2])
  candidates = set()
  for i in range(len(current_abs_edges)):
    candidates.add(i)
  candidates.remove(0)
  #print("First Candidates : {}".format(candidates))

  while (len(candidates) > 0):
    tmp_candidates = copy.deepcopy(candidates)
    for _, val in enumerate(tmp_candidates):
      if (current_abs_edges[val][1] in reachable) or (current_abs_edges[val][2] in reachable):
        reachable.add(current_abs_edges[val][1])
        reachable.add(current_abs_edges[val][2])
        new_absEdges.append(current_abs_edges[val])
        candidates.remove(val)
    

  new_abs_graph = AbstractGraph ()
  new_abs_graph.absNodes = copy.deepcopy(abs_graph.absNodes)
  new_abs_graph.absEdges = new_absEdges 
  
  return new_abs_graph