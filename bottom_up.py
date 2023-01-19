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





def refine(abs_graph, parameter, my_maps, current_score) :
      current_abs_graph = abs_graph
  current_score = current_score

  best_abs_graph = abs_graph
  best_score = current_score

  flag = False

  #'''
  print("Widening node intervals")
  #widening node intervals
  original_node_len = len(abs_graph.absNodes)
  #node_idx = len(abs_graph.absNodes) - 1
  #while(node_idx >= 0):
  for node_idx in range(len(abs_graph.absNodes)):
    itvs = current_abs_graph.absNodes[node_idx]
    if itvs == {}:
      continue
    else:
      for _, feat_idx in enumerate(itvs):
        (a, b) = itvs[feat_idx]
        if a != -99 and b != 99:
          new_abs_graph = copy.deepcopy(current_abs_graph)
          new_itvs = copy.deepcopy(itvs)
          new_itvs[feat_idx] = (a,99)
          new_abs_graph.absNodes[node_idx] = new_itvs

          new_score = eval_abs_graph_on_graphs_GC(new_abs_graph, parameter.graphs, parameter.labeled_graphs, parameter.left_graphs, parameter.train_graphs, my_maps)
          if (new_score >= best_score):
            flag = True
            best_abs_graph = new_abs_graph
            best_score = new_score
          if (new_score > best_score):
            print()
            print("NewAbsGraph")
            print(new_abs_graph.absNodes)
            print(new_abs_graph.absEdges)
            print()
            print("New Score : {}".format(new_score))

          new_abs_graph = copy.deepcopy(current_abs_graph)
          new_itvs = copy.deepcopy(itvs)
          new_itvs[feat_idx] = (-99,b)
          new_abs_graph.absNodes[node_idx] = new_itvs

          new_score = eval_abs_graph_on_graphs_GC(new_abs_graph, parameter.graphs, parameter.labeled_graphs, parameter.left_graphs, parameter.train_graphs, my_maps)
          if (new_score >= best_score):
            flag = True
            best_abs_graph = new_abs_graph
            best_score = new_score
          if (new_score > best_score):
            print()
            print("NewAbsGraph")
            print(new_abs_graph.absNodes)
            print(new_abs_graph.absEdges)
            print()
            print("New Score : {}".format(new_score))

        elif (a != -99 and b == 99) or (a == -99 and b != 99):
          new_abs_graph = copy.deepcopy(current_abs_graph)
          new_itvs = copy.deepcopy(itvs)
          new_itvs[feat_idx] = (-99,99)
          new_abs_graph.absNodes[node_idx] = new_itvs

          new_score = eval_abs_graph_on_graphs_GC(new_abs_graph, parameter.graphs, parameter.labeled_graphs, parameter.left_graphs, parameter.train_graphs, my_maps)
          if (new_score >= best_score):
            flag = True
            best_abs_graph = new_abs_graph
            best_score = new_score
          if (new_score > best_score):
            print()
            print("NewAbsGraph")
            print(new_abs_graph.absNodes)
            print(new_abs_graph.absEdges)
            print()
            print("New Score : {}".format(new_score))
  
        else:
          continue

    #node_idx = node_idx - 1


  print("Widening edge intervals")
  #widening edge intervals
  original_edge_len = len(abs_graph.absEdges)
  #edge_idx = len(abs_graph.absEdges) - 1
  #while(edge_idx >= 0):
  for edge_idx in range(len(abs_graph.absEdges)):
    (itvs, p, q) = current_abs_graph.absEdges[edge_idx]
    if itvs == {}:
      continue
    else:
      for _, feat_idx in enumerate(itvs):
        (a, b) = itvs[feat_idx]
        if a != -99 and b != 99:
          new_abs_graph = copy.deepcopy(current_abs_graph)
          new_itvs = copy.deepcopy(itvs)
          new_itvs[feat_idx] = (a,99)
          new_abs_graph.absEdges[edge_idx] = (new_itvs, p, q)

          new_score = eval_abs_graph_on_graphs_GC(new_abs_graph, parameter.graphs, parameter.labeled_graphs, parameter.left_graphs, parameter.train_graphs, my_maps)
          if (new_score >= best_score):
            flag = True
            best_abs_graph = new_abs_graph
            best_score = new_score
          if (new_score > best_score):
            print()
            print("NewAbsGraph")
            print(new_abs_graph.absNodes)
            print(new_abs_graph.absEdges)
            print()
            print("New Score : {}".format(new_score))


          new_abs_graph = copy.deepcopy(current_abs_graph)
          new_itvs = copy.deepcopy(itvs)
          new_itvs[feat_idx] = (-99,b)
          new_abs_graph.absEdges[edge_idx] = (new_itvs, p, q)

          new_score = eval_abs_graph_on_graphs_GC(new_abs_graph, parameter.graphs, parameter.labeled_graphs, parameter.left_graphs, parameter.train_graphs, my_maps)
          if (new_score >= best_score):
            flag = True
            best_abs_graph = new_abs_graph
            best_score = new_score
          if (new_score > best_score):
            print()
            print("NewAbsGraph")
            print(new_abs_graph.absNodes)
            print(new_abs_graph.absEdges)
            print()
            print("New Score : {}".format(new_score))

        

        elif (a != -99 and b == 99) or (a == -99 and b != 99):

          new_abs_graph = copy.deepcopy(current_abs_graph)
          new_itvs = copy.deepcopy(itvs)
          new_itvs[feat_idx] = (-99,99)
          new_abs_graph.absEdges[edge_idx] = (new_itvs, p, q)

          new_score = eval_abs_graph_on_graphs_GC(new_abs_graph, parameter.graphs, parameter.labeled_graphs, parameter.left_graphs, parameter.train_graphs, my_maps)
          if (new_score >= best_score):
            flag = True
            best_abs_graph = new_abs_graph
            best_score = new_score
          if (new_score > best_score):
            print()
            print("NewAbsGraph")
            print(new_abs_graph.absNodes)
            print(new_abs_graph.absEdges)
            print()
            print("New Score : {}".format(new_score))
     
        else:
          continue
    #edge_idx = edge_idx - 1 
  #'''    
  print("Removing edge")
  #remove edge
  original_edge_len = len(abs_graph.absEdges)
  edge_idx = len(abs_graph.absEdges) - 1
  while(edge_idx >= 0):

    new_abs_graph = copy.deepcopy(current_abs_graph)
    new_abs_graph.absEdges.pop(edge_idx)
     
    if not (my_connect(new_abs_graph)) : 
      edge_idx = edge_idx - 1
      continue

    new_abs_graph =  sort_abs_graph_edges(new_abs_graph)

    new_score = eval_abs_graph_on_graphs_GC(new_abs_graph, parameter.graphs, parameter.labeled_graphs, parameter.left_graphs, parameter.train_graphs, my_maps)
    if (new_score >= best_score):
      flag = True
      print("edge_idx : {}".format(edge_idx))
      best_abs_graph = new_abs_graph
      best_score = new_score
      if (new_score > best_score):
        print()
        print("NewAbsGraph")
        print(new_abs_graph.absNodes)
        print(new_abs_graph.absEdges)
        print()
        print("New Score : {}".format(new_score))

    #else:
    #  print()
    #  print("No!!!!!!! Abort!")
    #  print()
    edge_idx = edge_idx - 1

  if flag == False :
    print()
    print()
    print("No better absgraph")
    print()
    print()
    return (best_abs_graph, best_score)

  else:
    return refine(best_abs_graph, parameter, my_maps, best_score)
 