exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let node_for_label g x = 
  try List.find (fun n -> n.n_label = x) g.g_nodes
  with Not_found -> failwith "No such node"

let add_node g x =
  try 
    let _ = node_for_label g x in ()
  with _ -> let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
            g.g_nodes <- n::g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g =
  let rec dfs s =
    if s.n_mark = InProgress then true else begin
      s.n_mark <- InProgress;
      let b = List.fold_left (fun b node -> (b || (if node.n_mark != Visited 
                                                   then dfs node else false)))
                             false
                             s.n_link_to in
      s.n_mark <- Visited;
      b
    end
  in
  clear_marks g;
  List.fold_left (fun b node -> b || (if node.n_mark = NotVisited then dfs node else false)) 
                 false g.g_nodes

let topological g =
  let rec dfs acc todo =
    match todo with
    | node :: ns -> node.n_mark <- Visited;
                    let todo = List.fold_left 
                                 (fun l n -> if n.n_mark = NotVisited then n :: l else l)
                                 []
                                 node.n_linked_by in
                    dfs (node.n_label :: (dfs acc todo)) ns
    | [] -> acc
  in
  clear_marks g;
  let finish = List.filter (fun n -> n.n_link_to = []) g.g_nodes in
  List.rev (dfs [] finish)
