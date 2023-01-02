(*def eval_abs_node(abs_node, nodes, X_node):
  filtered_nodes = set()

  if len(abs_node) == 0:
    return nodes

  for _, node in enumerate(nodes):
    flag = True
    for _, feat_idx in enumerate(abs_node):
      (bot, top) = abs_node[feat_idx]
      if X_node[node][feat_idx] < bot or top < X_node[node][feat_idx]:
        flag = False
        break
    if flag == True:
      filtered_nodes.add(node)
  return filtered_nodes*)

let filtered_nodes = ref [||]

let eval_abs_node 
= fun abs_node nodes x_node ->
  if Array.length abs_node = 0 then nodes
  else begin
    let node = ref 0 in
    for i = 0 to Array.length nodes do
      let flag = ref true in
      let feat_idx = ref 0 in
      let quit_loop = ref false in
      while not !quit_loop do
        let (bot, top) = abs_node.(!feat_idx) in
        if (x_node.(!node).(!feat_idx) < bot || top < x_node.(!node).(!feat_idx)) then (flag := false; quit_loop := true)
        else ();
        feat_idx := !feat_idx + 1;
      done;
      if !flag = true then Array.append !filtered_nodes [|!node|] 
      else !filtered_nodes;
      node := !node + 1;
    done;
    !filtered_nodes
  end 