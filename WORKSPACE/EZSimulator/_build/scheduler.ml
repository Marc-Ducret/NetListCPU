open Netlist_ast
open Graph

exception Combinational_cycle

let rec read_exp (id, exp) = 
  let read_arg = function Avar id -> [id] | Aconst _ -> [] in
  match exp with
    Earg a | Enot a | Erom (_,_,a) | Eslice (_,_,a) | Eselect (_,a) -> read_arg a
  | Ebinop (_, a, b) | Econcat (a,b) -> (read_arg a) @ (read_arg b)
  | Emux (a, b, c) -> (read_arg a) @ (read_arg b) @ (read_arg c)
  | Eram (_,_,a,b,c,d) -> (read_arg a)
  | Ereg i -> [i]


let schedule p =
  let g = mk_graph() in
  let vars = List.fold_left (fun u (id,exp) -> u @ (id::(read_exp (id,exp)))) [] p.p_eqs in
  List.iter (fun v ->
		try
		  node_for_label g v; ()
		with Not_found -> add_node g v) vars;
  let regs = List.fold_left (fun u (id, exp) ->
			match exp with
				Ereg _ -> id::u
			|	_ -> u) [] p.p_eqs in
  List.iter (fun (id,exp) 
		-> List.iter (fun v -> 
			if List.mem v regs then add_edge g id v
			else 			add_edge g v id
			) (read_exp (id,exp))) 
		p.p_eqs;
  if has_cycle g then raise Combinational_cycle;
  let id_order = List.rev (topological g) in 
  {p with p_eqs = List.fold_left (fun u id -> 
		try
	  	  (List.find (fun (i,exp) -> i = id) p.p_eqs)::u
		with Not_found -> u) [] id_order}
  

