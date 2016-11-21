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

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle_2 g =
  clear_marks g;
  let rec dfs restants = 
    match restants with 
      |[] -> List.iter (fun x -> if x.n_mark = InProgress then x.n_mark <- Visited) g.g_nodes; false
      |x :: xs -> if x.n_mark = InProgress then true
                  else (if x.n_mark = Visited then dfs xs
                        else (x.n_mark <- InProgress;
                              dfs (x.n_link_to @ xs)))
  in
  List.fold_left (fun b x -> (b || (if x.n_mark = Visited then false else dfs [x]))) false g.g_nodes

let has_cycle g =
  clear_marks g;
  let rec dfs restants = 
    match restants with 
      |[] -> ()
      |x :: xs -> if x.n_mark = InProgress then raise Cycle
                  else (if x.n_mark = NotVisited then (x.n_mark <- InProgress; dfs (x.n_link_to));
                        x.n_mark <- Visited;
                        dfs xs)
  in
  try (List.iter (fun x -> (if x.n_mark = Visited then () else dfs [x])) g.g_nodes;
  false)
  with Cycle -> true

let topological g =
  clear_marks g;
  (*Parcours en profondeur postfixe *)
  let res = ref [] in
  let rec parcours restants =
    match restants with
      |[] -> ()
      |x :: xs -> if x.n_mark = NotVisited then (x.n_mark <- InProgress; parcours x.n_link_to; x.n_mark <- Visited; res := x.n_label :: !res);
                  parcours xs
  in parcours (find_roots g);
  !res


