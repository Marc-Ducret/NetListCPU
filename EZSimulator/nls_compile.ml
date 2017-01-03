open Netlist_ast
open Netlist
open Scheduler
open Printf

exception Incompatible_Sizes

let print_only = ref false
let number_steps = ref (-1)

let sim p =
	let eval id = id in
	let eval_arg = function
		| Avar id -> eval id
		|	Aconst VBit true -> "1"
		|	Aconst VBit false -> "0"
		|	Aconst VBitArray _ -> failwith "array constants unsupported" in
	let eval_expr = function
		| Earg a -> eval_arg a
		|	Ereg i -> eval i
		|	Enot a -> "!" ^ (eval_arg a)
		| Ebinop (op, a, b) ->
				let va = eval_arg a in
				let vb = eval_arg b in
				(match op with
					|	Or 		-> va^"|"^vb
					|	Xor 	-> va^"^"^vb
					| And 	-> va^"&"^vb
					|	Nand 	-> "!("^va ^"&"^vb^")"
				)
		| 	Emux (a, b, sel) -> 
				let vsel = eval_arg sel in
				let va = eval_arg a in
				let vb = eval_arg b in
				vsel^" ? "^vb^" : "^va
		|	_ -> failwith "not learned yet"
	in
	let head = "#include \"stdio.h\"\n\nint main() {\n" in
	let vars = Env.fold (fun var t str -> match t with
				|	TBit -> str ^ "\tchar " ^ var ^ ";\n"
				| TBitArray n -> str ^ "\tchar " ^ var ^ "[" ^ (string_of_int n) ^ "];\n"
				) p.p_vars "" in
	let exprs = "\n\twhile(1) {\n" ^ (List.fold_left 
																								(fun str (var, expr) -> str ^ "\t\t"^var^" = "^(eval_expr expr)^";\n")
																								"" p.p_eqs)
																 ^ "\t}\n}" in
	head ^ vars ^ exprs
	
let compile filename =
  try
    let p = Netlist.read_file filename in
    let p = Scheduler.schedule p in
    let code = sim p in
		let oc = open_out (filename^".c") in
		fprintf oc "%s" code;
		close_out oc;
		
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
	Printexc.record_backtrace true;
  Arg.parse
    []
    compile
    ""
;;

main ()
