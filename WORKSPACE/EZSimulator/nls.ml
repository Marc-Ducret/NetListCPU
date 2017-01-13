open Netlist_ast
open Netlist
open Scheduler

exception Incompatible_Sizes

let print_only = ref false
let number_steps = ref (-1)

let sim p =
	let env = Hashtbl.create 42 in
	let ram = ref None in
	let slice s e a =
		let ret = Array.make (e-s+1) false in
		for i = 0 to e-s do
			ret.(i) <- a.(i+s)
		done;
		ret
	in
	let bit = function
		| VBit b -> b
		| VBitArray a -> 
				if Array.length a = 1 then a.(0)
				else raise Incompatible_Sizes
	in
	let bit_array = function
		| VBitArray a -> a
		| VBit b -> Array.make 1 b
	in
	let array_value a =
		let v = ref 0 in
		for i = (Array.length a - 1) downto 0 do
			if a.(i) then v := 2 * !v + 1
			else v := 2 * !v
		done;
		!v
	in
	let rec print_value = function
		|	VBit(true) -> print_int 1
		|	VBit(false) -> print_int 0
		| VBitArray a -> print_string "[ "; Array.iter (fun b -> print_value (VBit b); print_string " ") a; print_string "]";
	in
	let eval id =
		if Hashtbl.mem env id then Hashtbl.find env id
		else VBit(false) in
	let eval_arg = function
		| Avar id -> eval id
		|	Aconst c -> c in
	let eval_expr = function
		| Earg a -> eval_arg a
		|	Ereg i -> eval i
		|	Enot a -> let b = bit (eval_arg a) in VBit(not b)
		| Ebinop (op, a, b) ->
				let va = bit (eval_arg a) in
				let vb = bit (eval_arg b) in
				(match op with
					|	Or -> 
							print_string "@ "; print_value (eval_arg a); print_string " || "; print_value (eval_arg b); print_endline "";
							VBit(va || vb)
					|	Xor -> VBit( (va && (not vb)) || (vb && (not va)) )
					| And -> VBit(va && vb)
					|	Nand -> VBit( not ( va && vb) )
				)
		| 	Emux (a, b, sel) -> let vsel = bit (eval_arg sel) in
				if vsel then eval_arg b else eval_arg a (* args?? *)
		| 	Econcat (a, b) ->
				print_string "# "; print_value (eval_arg a); print_string "."; print_value (eval_arg b); print_endline "";
				let va = bit_array (eval_arg a) in
				let vb = bit_array (eval_arg b) in
				VBitArray (Array.concat [va; vb])
		|		Eslice (s, e, a) -> 
				let va = bit_array (eval_arg a) in
				VBitArray (slice s e va)
		| 	Eselect (i, a) ->
				let va = bit_array (eval_arg a) in
				VBit va.(i)
		|		Eram (addr, word, ra, we, wa, data) -> (* we assume consistent word and address sizes across the net list *)
				(match !ram with
				| None ->
							ram := Some (Array.make (2 lsl (addr - 1)) [||]);
							VBitArray (Array.make word false) 
				| Some r ->
    					let content = r.(array_value (bit_array (eval_arg ra))) in
    					if content = [||] then VBitArray (Array.make word false)
							else VBitArray content)
		|	_ -> failwith "not learned yet"
	in
	let rec ram_write = function
		| Eram (addr, word, ra, we, wa, data) -> (* we assume consistent word and address sizes across the net list *)
				(match !ram with
				| None ->
							ram := Some (Array.make (2 lsl (addr - 1)) [||]);
							ram_write (Eram (addr, word, ra, we, wa, data))
				| Some r ->
							if bit (eval_arg we) then (
    						r.(array_value (bit_array (eval_arg wa))) <- bit_array (eval_arg data);
								print_value (eval_arg data); print_endline "";
								)
				)
		| _ -> ()
	in
	while true do
		print_endline "CLK";
		let split c s =
			let tmp = ref "" in
			let res = ref [] in
			for i = 0 to (String.length s) - 1 do
				if s.[i] = c then (
					res := !tmp :: !res;
					tmp := "" )
				else
					tmp := (String.make 1 s.[i]) ^ !tmp
			done;
			res := !tmp :: !res;
			tmp := "";
			List.rev !res
		in
		List.iter (fun id -> 
			print_string (id^"? ");
			Hashtbl.replace env id 
						(VBitArray(Array.map (fun s -> (int_of_string (String.trim s)) <> 0)
																	(Array.of_list (split ',' (read_line()))))))
						p.p_inputs;
		List.iter (fun (id, expr) -> Hashtbl.replace env id (eval_expr expr)) p.p_eqs;
		List.iter (fun (id, expr) -> ram_write expr) p.p_eqs;
		List.iter (fun id -> print_string (id^" "); print_value (eval id); print_string "\n";) p.p_outputs
	done
	
let compile filename =
  try
    let p = Netlist.read_file filename in
    let p = Scheduler.schedule p in
		print_endline "ORDER:";
		List.iter (fun (id, expr) -> print_endline id) p.p_eqs;
    sim p
        
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
	Printexc.record_backtrace true;
  Arg.parse
    ["-print", Arg.Set print_only, "Only print the result of scheduling";
     "-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()
