module Graph = struct
  type nodeData
  type edgeData

  module rec Node : struct
    type t with exp

    val compare : t -> t -> int
    val create : id:int -> data:nodeData -> edgeList:Edge.t ref list -> t

    module Find : sig
      val id : t -> int
      val data : t -> nodeData
      val edgeList : t -> Edge.t ref list
    end

    module Prepare : sig
      val data : t -> nodeData -> unit
      val edgeList : t -> Edge.t ref list -> unit
    end
  
  end 
  
  and Edge : struct
    type t with exp

    val compare : t -> t -> int
    val create : id:int -> data:edgeData -> pre:Node.t ref -> post:Node.t ref -> t

    module Find : struct
      val id : t -> int
      val data : t -> edgeData
      val pre : t -> Node.t ref
      val post : t -> Node.t ref
    end

    module Prepare : struct
      val data : t -> edgeData -> unit
      val pre : t -> Node.t ref -> unit
      val post : t -> Node.t ref -> unit
    end

  end

  type t

  val empty : unit -> t

  val nodes : t -> Node.t list
  val edges : t -> Edge.t list

  val findNode : t -> int -> Node.t option
  val findEdge : t -> int -> Edge.t option

  val addNode : t -> int -> nodeData -> unit
  val addEdge : t -> int -> edgeData -> int -> int -> unit

  val printNode : t -> int -> nodeData -> unit
  val printEdge : t -> int -> edgeData -> (int * int) -> unit
end


module Make
  (NodeData : struct 
    type t 
    with exp 
  end)
  
  (EdgeData : struct
    type t 
    with exp 
  end) : Graph
    with type nodeData := NodeData.t
    and  type edgeData := EdgeData.t 
    
    = struct

  module Types = struct
    type node = {
      id: int;
      mutable data: NodeData.t;
      mutable edgeList: edge ref list;
    } and edge = {
      id: int;
      mutable data: EdgeData.t;
      mutable pre: node ref;
      mutable post: node ref;
    } with sexp

    module Node = struct
      type t = node with sexp
      let create ~id ~data ~edgeList = {
        id; data; edgeList;
      }
      let compare (x:node) (y:node) = Int.compare x.id y.id

      module Find = struct
        let id (t:node) = t.id
        let data (t:node) = t.data
        let edgeList (t:node) = t.edgeList
      end

      module Prepare = struct
        let data (t:node) data = t.data <- data
        let edgeList (t:node) edges = t.edgeList <- edges
      end
    end

    module Edge = struct
      type t = edge with sexp
      let create ~id ~data ~pre ~post = {
        id; data; pre; post;
      }
      let compare (x:edge) (y:edge) = Int.compare x.id y.id

      module Find = struct
        let id (t:edge) = t.id
        let data (t:edge) = t.data
        let pre (t:edge) = t.pre
        let post (t:edge) = t.post
      end

      module Prepare = struct
        let data (t:edge) data = t.data <- data
        let pre (t:edge) n = t.pre <- n
        let post (t:edge) n = t.post <- n
      end
    end
  end

  module Node = Types.Node
  module Edge = Types.Edge

  type t = (int, Node.t) Hashtbl.t * (int, Edge.t) Hashtbl.t

  let empty () = (Hashtbl.create ~hashable:Int.hashable (), Hashtbl.create ~hashable:Int.hashable ())

  let nodes (nodes, _) = Hashtbl.data nodes
  let edges (_, edges) = Hashtbl.data edges

  let findNode (nodes, _) id = Hashtbl.find nodes id
  let getEdge (_, edges) id = Hashtbl.find edges id

  (*Will fail silently if graph already contains node with id.*)
  let addNode (nodes, _) id data =
    let node = Node.create ~id ~data ~edgeList:[] in
    Hashtbl.add nodes ~key:id ~data:node |> ignore

  (*Will fail silently if graph does not contain node or does already contain
    edge with id.*)
  let addEdge ((_, edges) as g) id data n1_id n2_id =
    let n1 = findNode g n1_id in
    let n2 = findNode g n2_id in
    if Option.both n1 n2 = None then
      ()
    else begin
      let n1 = Option.valueExn n1 in
      let n2 = Option.valueExn n2 in
      match findEdge g id with
        | Some _ -> ()
        | None -> begin
            let edge = Edge.create ~id ~data ~pre:(ref n1) ~post:(ref n2) in
            let appendEdge node edge =
              let oldEdges = Node.Get.edgeList node in
              Node.Set.edgeList node (edge::oldEdges) in
            appendEdge n1 (ref edge);
            if Int.compare n1_id n2_id = 0 then
              ()
            else
              appendEdge n2 (ref edge);
            Hashtbl.add edges ~key:id ~data:edge |> ignore
          end
    end
end


let array_to_list xs =
  Array.fold_right List.cons xs []

let array_of_list xs = match xs with
  | [] -> [||]
  | default :: _ ->
    let arr = Array.make (List.length xs) default in
    List.iteri (Array.set arr) xs;
    arr
