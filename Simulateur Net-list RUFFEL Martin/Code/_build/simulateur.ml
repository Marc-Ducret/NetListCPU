open Netlist_ast

let print_only = ref false
let number_steps = ref (1)
       
let interprete p n =
  
  (*Initialisation de l'environnement *)
  let env = Hashtbl.create 17 in
  let inputs = p.p_inputs in
  List.iter (fun x -> Hashtbl.add env x (VBit(false))) inputs;
  
  (*Initialisation des registres *)
  let l_regs =
    List.fold_left (fun rs (x, e) -> (match (Env.find x p.p_vars, e) with
                                     |TBit, Ereg(y) ->
                                       Hashtbl.add env x (VBit(false));
                                       (x, y) :: rs
                                     |TBitArray(n), Ereg(y) ->
                                       Hashtbl.add env x (VBitArray(Array.make n false));
                                       (x, y) :: rs
                                     |_ -> rs ))
                   [] p.p_eqs
  in
  
  (* Initialisation de la ram *)
  (* On recupere la taille des objets et adresses *)
  let sa, sw = try (let e =
                  List.find (fun (_, e) -> match e with Erom(_) | Eram (_) -> true
                                                        |_ -> false) p.p_eqs in
                match snd e with
                  Erom(sa, sw, _) -> sa, sw
                 |Eram(sa, sw, _, _, _, _) -> sa, sw
                 |_ -> (-1, -1) (*Impossible*))
               with Not_found -> (0, 0)
  in
  let init sw =
    if sw = 1 then VBit(false)
    else VBitArray(Array.make sw false)
  in
  let ram = Array.make (1 lsl sa) (init sw)
  in
  let ram_actions = ref []
  in
  
  (* Fonctions auxiliaires de la boucle principale *)
  let rec ask_v x =
    (* Fonction demandant la saisie d'une valeur et l'affectant dans l'env *)
    print_string (x ^ " ?  ");
    let s = read_line () in
    try (
      match Env.find x p.p_vars with
        TBit -> (match s with
                 |"0" -> Hashtbl.replace env x (VBit false)
                 |"1" -> Hashtbl.replace env x (VBit true)
                 |_ -> failwith "Wrong input")
       |TBitArray(n) -> if String.length s <> n then failwith "Wrong input"
                        else (let v = Array.make n false in
                              for i=0 to n-1 do(
                                match s.[i] with
                                  '0' -> ()
                                 |'1' -> v.(i) <- true
                                 |_ -> failwith "Wrong input")
                              done;
                              Hashtbl.replace env x (VBitArray(v))
                             )                                       
    )
    with Failure "Wrong input" -> print_endline "Wrong input";
                                  ask_v x
  in
  let ret_v x =
    (* Affiche la valeur actuelle de x *)
    let aux_string v =
      match v with
        VBit(true) -> "1"
       |VBit(false) -> "0"
       |VBitArray(a) -> let s = ref "" in
                        for i=0 to (Array.length a - 1) do
                          s := !s ^ (match a.(i) with
                                       true -> "1"
                                     |false -> "0")
                        done;
                        !s
    in print_string ("=> " ^ x ^ " = " ^ (aux_string (Hashtbl.find env x)) ^ "\n")
  in
  let eval_arg x =
    (* Renvoie la valeur d'un type arg *)
    match x with
      Avar(id) -> Hashtbl.find env id
     |Aconst(v) -> v
  in
  let to_array v =
    (* Convertit la valeur en array *)
    match v with
      VBit(b) -> VBitArray([|b|])
     |_ -> v
  in
  let negation x =
    (* Calcule la negation d'une valeur *)
    match x with
      VBit(b) -> VBit(not b)
    |VBitArray(_) -> failwith "unsupported operand type"
  in
  let conv x =
    (* Convertit x de type arg en valeur entiere associée a son adresse *)
    match eval_arg x with
      VBit(b) -> if b then 1 else 0
     |VBitArray(a) -> let s = ref 0 in
                      for i=0 to (sa - 1) do
                        s := 2 * !s + (if a.(i) then 1 else 0)
                      done;
                      !s
  in
 
  (* Fonction d'execution des equations *)
  let rec execute eqs = 
    match eqs with
    |[] -> ()
    |(x, e) :: eqs -> (match e with
                       | Earg(y) -> Hashtbl.replace env x (eval_arg y)
                       | Ereg(y) -> ()
                       | Enot(y) -> Hashtbl.replace env x (negation (eval_arg y))
                       | Ebinop(op, y, z) ->
                          (try
                             let VBit(vy), VBit(vz) = eval_arg y, eval_arg z in
                             let res = VBit (match op with
                                             |Or -> vy || vz
                                             |And -> vy && vz
                                             |Xor -> (vy && (not vz)) || (vz && (not vy))
                                             |Nand -> not (vy && vz))
                             in
                             Hashtbl.replace env x res
                           with Match_failure (_) -> failwith "unsupported operand type")
                       |Emux(t, y, z) ->
                         (try
                            let VBit(vt) = eval_arg t in
                            let res = (if vt then y else z) in
                            Hashtbl.replace env x (eval_arg res)
                          with Match_failure (_) -> failwith "unsupported operand type")                
                       | Erom (sa, sw, ra) -> Hashtbl.replace env x (ram.(conv ra))
                       | Eram (sa, sw, ra, we, wa, d) ->
                          Hashtbl.replace env x (ram.(conv ra));
                          ram_actions := (we, wa, d) :: !ram_actions
                       | Econcat(a, b) ->
                          (try
                             let VBitArray(va), VBitArray(vb) =
                               to_array (eval_arg a), to_array (eval_arg b)
                             in
                             Hashtbl.replace env x (VBitArray(Array.append va vb))
                           with Match_failure (_) -> failwith "unsupported operand type")                
                       | Eslice(i, j, a) ->
                          (try
                             let VBitArray(va) = to_array (eval_arg a) in
                             Hashtbl.replace env x (VBitArray (Array.sub va i (j - i + 1)))
                           with Match_failure (_) -> failwith "unsupported operand type")                
                       | Eselect(i, a) ->
                          (try
                             let VBitArray(va) = to_array (eval_arg a) in
                             Hashtbl.replace env x (VBit va.(i))
                           with Match_failure (_) -> failwith "unsupported operand type"));
                      execute eqs
                                         

  in
  (* Execution de chaque cycle *)
  for i=1 to n do(
    print_string ("Cycle " ^ (string_of_int i) ^ "\n");
    (* On demande la saisie des entrees *)
    List.iter ask_v p.p_inputs;

    (* On execute un cycle *)
    ram_actions := [];
    execute p.p_eqs;
            
    (* Ecriture de la RAM *)
    List.iter (fun (we, wa, d) ->
        match eval_arg we with
        |VBit(b) -> if b then ram.(conv wa) <- (eval_arg d)
        |VBitArray(_) -> failwith "wrong type" )
              !ram_actions;
    
    (* Affichage des valeurs de sortie *)
    List.iter ret_v p.p_outputs;
    
    (* Mise a jour des registres *)
    List.iter (fun (x, v) -> Hashtbl.replace env x v)
              (List.fold_left (fun changes (x, y) ->
                   (x, Hashtbl.find env y) :: changes)
                              [] l_regs )
  )
  done;
  ()
  

let compile filename =
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        interprete p !number_steps
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
            exit 2
    end;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()
                          
