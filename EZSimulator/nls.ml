open Netlist_ast

let print_only = ref false
let number_steps = ref (-1)

let sim p =
	let env = Hashtbl.create 42 in
	let slice s e a =
		let ret = Array.make (e-s) false in
		for i = 0 to e-s-1 do
			ret.(i) <- a.(s+i)
		done;
		ret
	in
	let eval id =
		if Hashtbl.mem env id then Hashtbl.find env id
		else VBit(false) in
	let eval_arg = function
		Avar id -> eval id
	|	Aconst c -> c in
	let eval_expr = function
		Earg a -> eval_arg a
	|	Ereg i -> eval i
	|	Enot a -> let VBit(b) = eval_arg a in VBit(not b)
	| 	Ebinop (op, a, b) ->
				let VBit(va) = eval_arg a in
				let VBit(vb) = eval_arg b in
				(match op with
				|	Or -> VBit(va || vb)
				|	Xor -> VBit( (va && (not vb)) || (vb && (not va)) )
				|  	And -> VBit(va && vb)
				|	Nand -> VBit( not ( va && vb) )
				)
	| 	Emux (a, b, sel) -> let VBit(vsel) = eval_arg sel in
				if vsel then eval_arg b else eval_arg a (* args?? *)
	| 	Econcat (VBitArray a, VBitArray b) -> VBitArray (Array.concat [a; b]) (* LOL *)
	|	Eslice (s, e, VBitArray a) -> VBitArray (slice s e a)
	| 	Eselect (i, VBitArray a) -> VBit a.(i)
	|	_ -> failwith "not learned yet"
	in
	let print_value = function
	|	VBit(true) -> print_int 1
	|	VBit(false) -> print_int 0
	| 	_ -> () in
	while true do
		print_string "CLK\n";
		List.iter (fun (id, expr) -> Hashtbl.add env id (eval_expr expr)) p.p_eqs;
		List.iter (fun id -> print_string (id^" "); print_value (eval id); print_string "\n";) p.p_outputs
	done
	

let compile filename =
  try
    let p = Netlist.read_file filename in
    let p = Scheduler.schedule p in
    sim p
        
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-print", Arg.Set print_only, "Only print the result of scheduling";
     "-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()
