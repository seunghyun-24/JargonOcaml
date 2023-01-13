type abstract_graph = abs_node list * abs_edge list
and abs_node = itv list 
and abs_edge = triple list
and triple = itv list * from_idx * to_idx
and itv = Itv of float * float 
and from_idx = int
and to_idx = int 

type graph_list = graph list
and graph = node list * edge list
and node = itv list
and edge = triple list
and triple = itv list * from_idx * to_idx
and itv = ITV of float * float
and from_idx = int
and to_idx = int
