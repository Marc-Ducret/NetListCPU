open Netlist_ast


(** Global parameters *)
let number_steps = ref max_int

(** Tasks are performed at the end of each cycle *)
type task =
  | NoTask
  | TReg of ident * ident (* (x, z) corresponds to x = EREG z *)
  | TRam of arg (*write_enable*) * arg (*write_addr*) * arg (*data*)


(** Take a program and simulate it *)
let simulate p =
  (* Environments : contains variables *)
  let env  = Hashtbl.create 42 in (* (ident * value) Hashtbl.t *)
  let env' = Hashtbl.create 42 in (* (ident * value) Hashtbl.t, used for tasks *)

  (* RAM initialization *)
  let sizes =
    List.fold_left
      (fun l eq -> match snd eq with
         | Erom (addr_size, word_size, _) | Eram (addr_size, word_size, _, _, _, _)
           -> (addr_size, word_size) :: l
         | _ -> l)
      []
      p.p_eqs
  in
  let (addr_size, word_size) = match sizes with
      [] -> (0, 0)
    | (a, w) :: l -> if List.exists ((<>) (a, w)) l
      then failwith "RAM or ROM has inconsistent address or word size"
      else (a, w)
  in
  let ram = RAM.init addr_size word_size in


  let print_bool b =
    if b then "1" else "0"
  in

  let proc_arg = function
    (** Returns a C code corresponding to the value of the arg *)
    | Aconst (VBit b) -> print_bool b
    | Aconst (VBitArray a) -> let s = Array.fold_left (fun s b -> Format.sprintf "%s,%s" s (print_bool b)) "" a in
      let s' = String.sub s 0 (String.length s - 1) in
      Format.sprintf "{%s}" s'
    | Avar x -> x
  in


  (** Helper functions *)
  let xor a b = 
    Format.sprintf "%s ^ %s" a b
  in
  
  let arrayify arg =
    match Env.find arg p.p_vars with
      TBit -> Format.sprintf "{%s}" (proc_arg arg)
    | TBitArray _ -> proc_arg arg
  in
  
  let not_vbit a =
    
                       
                                   let binop op a b =
                                     match (op, a, b) with
                                     | (_, VBitArray _, _) | (_, _, VBitArray _) ->
                                       failwith "Binops can't be applied to an array"
                                     | (Or, VBit a, VBit b) -> VBit (a || b)
                                     | (Xor, VBit a, VBit b) -> VBit (xor a b)
                                     | (And, VBit a, VBit b) -> VBit (a && b)
                                     | (Nand, VBit a, VBit b) -> VBit (not (a && b))

                                   let mux a b c =
                                     match (a, b, c) with
                                     | (VBit a, VBit b, VBit c) -> VBit (if a then b else c)
                                     | _ -> failwith "Mux can't be applied to an array"

                                   let concat a b =
                                     VBitArray (Array.concat [arrayify a; arrayify b])

                                   let slice a i j =
                                     (* Returns [|a.(i); ...; a.(j)|] *)
                                     let a = arrayify a in
                                     if not (0 <= i && i <= j && j < Array.length a)
                                     then failwith "Uncorrect parameters for slice";
                                     let t = Array.make (j - i + 1) false in
                                     for k = i to j do
                                       t.(k - i) <- a.(k)
                                     done;
                                     VBitArray t

                                   let select a i =
                                     let a = arrayify a in
                                     if i < 0 || i >= Array.length a
                                     then failwith "Uncorrect parameter for select";
                                     VBit a.(i)

                                   let get_addr = function
                                     | VBit _ -> failwith "Address must be an array"
                                     | VBitArray a -> a

                                   let to_bit = function
                                     | VBit b -> b
                                     | VBitArray _ -> failwith "Write_enable expects a bit"


                                   let rec process_tasks = function
                                     (** Apply the given tasks and put the new env in env' *)
                                     | [] -> ()
                                     | NoTask :: t -> process_tasks t
                                     | (TReg (x, z)) :: t -> Hashtbl.replace env' x (proc_arg (Avar z));
                                       process_tasks t
                                     | (TRam (write_enable, write_addr, data)) :: t ->
                                       if to_bit (proc_arg write_enable)
                                       then RAM.write ram (get_addr (proc_arg write_addr))
                                           (arrayify (proc_arg data));
                                       process_tasks t
                                   in


                                   let rec process_eq eqs todo =
                                     (** Process all eqs in order, then do "at the same time" the todo accumulator, 
                                         which is a task list which must be done at the end of the cycle *)
                                     match eqs with
                                     | (res, exp) :: eqs -> (
                                         let (value, todo') = match exp with
                                           | Earg arg -> (proc_arg arg, NoTask)
                                           | Ereg x -> (proc_arg (Avar res), TReg (res, x))
                                           (* REG returns their content at the end of the cycle *)
                                           | Enot arg -> (not_vbit (proc_arg arg), NoTask)
                                           | Ebinop (op, a, b) -> (binop op (proc_arg a) (proc_arg b), NoTask)
                                           | Emux (a, b, c) -> (mux (proc_arg a) (proc_arg b) (proc_arg c), NoTask)
                                           | Eram (_, _, read_addr, write_enable, write_addr, data) ->
                                             let read_addr = get_addr (proc_arg read_addr) in
                                             (VBitArray (RAM.read ram read_addr),
                                              TRam (write_enable, write_addr, data))
                                           | Erom (_, _, read_addr) ->
                                             let read_addr = get_addr (proc_arg read_addr) in
                                             (VBitArray (RAM.read ram read_addr), NoTask)
                                           | Econcat (a, b) -> (concat (proc_arg a) (proc_arg b), NoTask)
                                           | Eslice (i, j, a) -> (slice (proc_arg a) i j, NoTask)
                                           | Eselect (i, a) -> (select (proc_arg a) i, NoTask)
                                         in
                                         Hashtbl.replace env res value;
                                         process_eq eqs (todo' :: todo))
                                     | [] -> (* Use temp hash table to do all affectations "at the same time", then
                                                the changes will be applied to the current env after printing *)
                                       Hashtbl.reset env';
                                       process_tasks todo
                                   in

                                   if p.p_inputs <> [] then failwith "inputs not supported";
                                   let header = "#inlude <stdio>\n\nint main(){\n" in
                                   let vars = Env.fold (fun v t s -> s ^ (match t with
                                       | TBit -> Format.sprintf "char %s;\n" v
                                       | TBitArray n -> Format.sprintf "char %s[n];\n")) p.p_vars "" in
                                   let eqs = process_eq p.p_eqs [] in
                                   ()


                                   let compile filename =
                                     try
                                       let p = Netlist.read_file filename in
                                       let out_name = (Filename.chop_suffix filename ".net") ^ "_sch.net" in
                                       let out = open_out out_name in
                                       let close_all () =
                                         close_out out
                                       in
                                       begin try
                                           let p = Scheduler.schedule p in
                                           simulate p
                                         with
                                         | Scheduler.Combinational_cycle ->
                                           Format.eprintf "The netlist has a combinatory cycle.@.";
                                           close_all (); exit 2
                                       end;
                                       close_all ();
                                     with
                                     | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2


                                   let () =
                                     Arg.parse
                                       ["-n", Arg.Set_int number_steps, "Number of steps to simulate"]
                                       compile
                                       ""
