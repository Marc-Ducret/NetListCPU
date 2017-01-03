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

let has_cycle g =
  let rec cycle = function
    | [] -> false
    | a::l ->
       if a.n_mark = InProgress then true
       else ( a.n_mark <- InProgress;
          let boo = cycle a.n_link_to in (
          a.n_mark <- Visited;
          boo || cycle l ) )
  in cycle g.g_nodes

let topological g =
  clear_marks g;
  if has_cycle g then raise Cycle
  else
      clear_marks g;
      let rec build = function
        | [] -> []
        | a::l -> if a.n_mark = Visited then build l
                  else let l1 = (build  a.n_link_to) in
                       a.n_mark <- Visited;
                       (build l)@(a.n_label::l1)
      in
      clear_marks g;
      build (find_roots g)
            

(*
let gr = mk_graph() ;;
    add_node gr "a" ; add_node gr "b" ; add_node gr "c" ;
    add_node gr "l1" ; add_node gr "l3" ; add_node gr "l4";
    add_node gr "l5" ; add_node gr "r" ; add_node gr "s" ;
    add_edge gr "a" "l1" ; add_edge gr "a" "l3" ; add_edge gr "a" "l5"
    ; add_edge gr "b" "l1" ; add_edge gr "b" "l3" ;
    add_edge gr "b" "l4" ; add_edge gr "c" "l5"; add_edge gr "c" "s" ;    
    add_edge gr "l4" "l5" ; add_edge gr "l3" "r" ; add_edge gr "l5" "r";
    topological gr;; *)
 
