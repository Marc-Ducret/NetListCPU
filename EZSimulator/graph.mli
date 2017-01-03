exception Cycle
type mark = NotVisited | InProgress | Visited
type 'a graph = { mutable g_nodes : 'a node list; }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}
val mk_graph : unit -> 'a graph
val add_node : 'a graph -> 'a -> unit
val node_for_label : 'a graph -> 'a -> 'a node
val add_edge : 'a graph -> 'a -> 'a -> unit
val clear_marks : 'a graph -> unit
val find_roots : 'a graph -> 'a node list
val find_roots_rev : 'a graph -> 'a node list
val has_cycle : 'a graph -> bool
val topological : 'a graph -> 'a list
