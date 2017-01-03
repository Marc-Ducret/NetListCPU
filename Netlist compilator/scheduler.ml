open Netlist_ast
open Graph

exception Combinational_cycle

let compute_reglist eqs =
  let rec aux eqs acc =
    match eqs with
    | [] -> acc
    | (r, Ereg _) :: tl -> aux tl (r :: acc)
    | _ :: tl -> aux tl acc
  in
  aux eqs []

let read_exp eq reglist =
  (* Renvoie une liste de (résultat, dépendance)*)
  let (res, exp) = eq in

  let proc_reg (res, x) = if List.mem x reglist then (x, res) else (res, x) in

  let proc_arg arg =
    match arg with
    | Avar x -> [x]
    | Aconst _ -> []
  in
  let proc_args u = List.flatten (List.map proc_arg u) in
  let proc l = List.map (fun x -> proc_reg (res, x)) (proc_args l) in

  match exp with
  | Earg arg | Enot arg | Eslice (_, _, arg) | Eselect (_, arg) | Erom (_, _, arg) -> proc [arg]
  | Ebinop (_, arg, arg') | Econcat (arg, arg') -> proc [arg; arg']
  | Emux (arg, arg', arg'') -> proc [arg; arg'; arg'']
  | Ereg x -> [proc_reg (res, x)]
  | Eram (_, _, ra, we, wa, d) -> proc [ra]

let schedule p =
  let g = mk_graph () in
  let add_nodes = List.iter (add_node g) in
  add_nodes p.p_inputs;
  add_nodes (List.flatten (List.map (fun eq -> (fst eq) :: (List.map snd (read_exp eq []))) p.p_eqs));
  let reglist = compute_reglist p.p_eqs in
  List.iter (fun eq -> List.iter (fun (res, dep) -> add_edge g dep res) (read_exp eq reglist)) p.p_eqs;
  if has_cycle g then raise Combinational_cycle;
  let order = List.rev (topological g) in
  {p with p_eqs = List.fold_left 
                    (fun eqs ident -> if List.mem ident p.p_inputs
                                      then eqs
                                      else (List.find (fun eq -> fst eq = ident) p.p_eqs) :: eqs)
                    []
                    order}

