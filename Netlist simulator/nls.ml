open Netlist_ast


(** Global parameters *)
let number_steps = ref max_int


(** Module used to represent the RAM and read/write it *)
module RAM :
sig
  type t (** The type of the RAM *)

  val init : addr_size : int -> word_size : int -> t

  val read  : t -> bool array -> bool array
  val write : t -> bool array -> bool array -> unit
end
=
struct
  type t = { contents : bool array array;
             addr_size : int;
             word_size : int }

  let address_to_index addr =
    (* Least significant bit is the rightmost *)
    Array.fold_left (fun i b -> 2*i + (if b then 1 else 0)) 0 addr

  let init ~addr_size ~word_size =
    { contents = Array.init (1 lsl addr_size) (fun _ -> Array.create word_size false);
      addr_size;
      word_size }

  let read ram read_addr =
    if Array.length read_addr <> ram.addr_size
    then failwith "Address has not the correct size";
    let off = address_to_index read_addr in
    ram.contents.(off)

  let write ram write_addr data =
    if ram.word_size <> Array.length data
    then failwith "Data to be written in RAM must be a word";
    let off = address_to_index write_addr in
    ram.contents.(off) <- data
end


(** Tasks are performed at the end of each cycle *)
type task =
  | NoTask
  | TReg of ident * ident (* (x, z) corresponds to x = EREG z *)
  | TRam of arg (*write_enable*) * arg (*write_addr*) * arg (*data*)


(** Helper functions *)
let xor a b = 
  (a || b) && (not (a && b))

let arrayify = function
  | VBit b -> [|b|]
  | VBitArray a -> a

let not_vbit = function
  | VBit b -> VBit (not b)
  | VBitArray _ -> failwith "Not can't be applied to an array"

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


  let proc_arg = function
  (** Returns the value of the given argument, with 0 as an uninitialized var *)
  | Aconst v -> v
  | Avar x -> try Hashtbl.find env x 
    with Not_found -> let t = Env.find x p.p_vars in
      match t with
      | TBit -> VBit false
      | TBitArray n -> VBitArray (Array.create n false)
  in


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

  let scan_inputs () =
    let char_to_bool = function
      | '0' -> false
      | '1' -> true
      | _ -> failwith "Wrong input, '0' and '1' are the only characters allowed"
    in
    let type_to_string = function
      | TBit -> ""
      | TBitArray n -> " : " ^ (string_of_int n) ^ " "
    in
    List.iter (fun x -> 
                let v = ref None in
                let t = Env.find x p.p_vars in
                while !v = None do
                  print_string (x ^ (type_to_string t) ^ " = ");
                  let s = read_line () in
                  let n = String.length s in
                  try match t with
                    | TBit -> if n = 1 then v := Some (VBit (char_to_bool s.[0]))
                      else failwith "Wrong input, only one bit is expected"
                    | TBitArray n' -> if n = n'
                      then v :=  Some (VBitArray (Array.init n
                                                    (fun i -> char_to_bool s.[i])))
                      else failwith ("Wrong input, an array of length " ^
                                     (string_of_int n') ^ " is expected")
                  with Failure s -> print_endline s
                done;
                let v = match !v with
                  | Some v -> v
                  | None -> assert false
                in
                Hashtbl.replace env x v) p.p_inputs
  in

  let print_outputs () =
    let rec val_to_string = function
      | VBit true -> "1"
      | VBit false -> "0"
      | VBitArray a -> Array.fold_left (fun s b -> s ^ (val_to_string (VBit b))) "" a
    in
    List.iter (fun x -> print_endline
                          (x ^ " = " ^ (val_to_string (proc_arg (Avar x)))))
      p.p_outputs
  in      


  let step = ref 1 in
  while !step <= !number_steps do
    print_endline ("Step " ^ (string_of_int !step));
    scan_inputs ();
    process_eq p.p_eqs [];
    print_outputs ();
    Hashtbl.iter (fun x v -> Hashtbl.replace env x v) env';
    incr step
  done

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
