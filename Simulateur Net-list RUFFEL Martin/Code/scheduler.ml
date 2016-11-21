open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp eq =
  let (x, expr) = eq in
  let res = ref [] in
  let add  = function
    |Avar(x) -> res := x :: !res
    |_ -> ()
  in
  let cherche = function
    | Earg(x) -> add x
    | Ereg(x) -> ()
    | Enot(x) -> add x
    | Ebinop(_, x, y) -> add x; add y
    | Emux(x, y, z) -> add x; add y; add z
    | Erom(_, _, x) -> add x
    | Eram(_, _, x, y, z, a) -> add x
    | Econcat(x, y) -> add x; add y
    | Eslice(_, _, x) -> add x
    | Eselect(_, x) -> add x
  in
  cherche expr;
  x, !res
  

let schedule p =
  let list_eq = p.p_eqs in
  let inp = p.p_inputs in
  let outp = p.p_outputs in
  let g = mk_graph () in
  let node_list = ref [] in
  let link_list = ref [] in
  let rec separe_reg rest r a =
    (* Separe les registres des autres equations *)
    match rest with
    |[] -> r, a
    |(x, Ereg(y)) :: cs -> separe_reg cs ((x, Ereg(y)) :: r) a
    |c :: cs -> separe_reg cs r (c :: a)
  in
  let r, a = separe_reg list_eq [] []
  in
  let rec add_nodes = function
    |[] -> ()
    |n :: ns -> if not (List.mem n !node_list) then node_list := n :: !node_list;
      add_nodes ns
  in
  let rec add_links x ys =
    match ys with
      |[] -> ()
      |y :: ys -> if not (List.mem (x, y) !link_list) then link_list := (x, y) :: !link_list;
	add_links x ys
  in
  let rec transforme restants =
    match restants with
      |[] -> ()
      |eq :: eqs -> let x, vars = read_exp eq in 
		    add_nodes (x :: vars);
		    add_links x vars;
		    transforme eqs
  in
  transforme list_eq;
  List.iter (add_node g) !node_list;
  List.iter (fun (a, b) -> add_edge g a b) !link_list;
  
  if has_cycle g then raise Combinational_cycle;
  let sorted = (topological g) in
  (* On doit retrouver les equations associees *)
  let rec associe ids =
    match ids with
      [] -> []
      |x :: xs -> try 
                    (List.find (fun (y, eq) -> x=y) a) :: (associe xs)
                  with Not_found -> associe xs
  in
  (* On met tous les registres à la fin, dans l'ordre initial.*)
  
  {p_eqs=List.rev (r @ (associe sorted)); p_inputs=inp; p_outputs=outp; p_vars=p.p_vars}
      
  
		      
