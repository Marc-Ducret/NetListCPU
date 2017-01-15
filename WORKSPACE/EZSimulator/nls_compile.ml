open Netlist_ast
open Netlist
open Scheduler
open Printf

exception Incompatible_Sizes

let print_only = ref false
let number_steps = ref (-1)

let sim p =
	let size var = 
		match Env.find var p.p_vars with
		| TBit -> 1
		| TBitArray n -> n
	in
	let is_array var =
		match Env.find var p.p_vars with
		| TBit -> false
		| TBitArray _ -> true
	in
	let is_only_array var =
		match Env.find var p.p_vars with
  		| TBit -> false
  		| TBitArray n -> n > 1;
	in
	let size_arg = function
		| Avar var -> size var
		| Aconst _ -> 1
	in
	let arg_var_name = function
		Avar var -> var
		| _ -> failwith "Arg is not a var"
	in
	let eval_var_array var =
		var
	in
	let eval_var_bit var =
		if is_array var then var^"[0]"
		else var
	in
	let eval_var array var =
		if array then eval_var_array var
		else eval_var_bit var
	in
	let eval_arg_array = function
		| Avar id -> id
		| _ -> failwith "array constants unsupported" in
	let eval_arg_bit = function
		| Avar id -> eval_var_bit id
		|	Aconst VBit true -> "1"
		|	Aconst VBit false -> "0"
		|	Aconst VBitArray _ -> failwith "array constants unsupported" in
	let eval_arg array arg =
		if array then eval_arg_array arg
		else eval_arg_bit arg in
	let sel i = function
		| Avar var when is_array var -> var^"["^i^"]"
		| x -> eval_arg_bit x
	in
	let assign var str =
		"\t\t"^(eval_var (is_only_array var) var)^" = "^str^";\n"
	in
	let eval_expr var = function
		| 	Earg a -> assign var (eval_arg (is_only_array var) a)
		|	Ereg i -> assign var (eval_var (is_only_array var) i)
		|	Enot a -> assign var ("!" ^ (eval_arg_bit a))
		| 	Ebinop (op, a, b) ->
				let va = eval_arg_bit a in
				let vb = eval_arg_bit b in
				assign var
				(match op with
					|	Or 		-> va^" | "^vb
					|	Xor 	-> va^" ^ "^vb
					| And 	-> va^" & "^vb
					|	Nand 	-> "!("^va ^" & "^vb^")"
				)
		| 	Emux (sel, a, b) -> 
				let vsel = eval_arg_bit sel in
				let va = eval_arg_bit a in
				let vb = eval_arg_bit b in
				assign var (vsel^" ? "^va^" : "^vb)
		|	Econcat (a, b) ->
  			let sa = size_arg a in
  			let sb = size_arg b in
  				"\t\tfor(int i = 0; i < "^(string_of_int sa)^"; i++)\n\t\t\t"^(sel "i" (Avar var))^" = "^(sel "i" a)^";\n"
  			^ "\t\tfor(int i = 0; i < "^(string_of_int sb)^"; i++)\n\t\t\t"^(sel ("i+"^(string_of_int sa)) (Avar var))^" = "^(sel "i" b)^";\n"
		| Eslice (i, j, a) -> 
				"\t\tfor(int i = "^(string_of_int i)^"; i <= "^(string_of_int j)^"; i ++) "^(sel ("i-"^(string_of_int i)) (Avar var))
				^" = "^(sel "i" a)^";\n";
		| Eselect (i, a) -> assign var (sel (string_of_int i) a)
		| Eram (sa, sw, ra, we, wa, data) -> "\t\t_addr = toInt("^(eval_arg_array ra)^", "^(string_of_int sa)^");\n"
																				^"\t\tfor(int i = 0; i < "^(string_of_int sw)^"; i++) "^var^"[i] = "
																				^"_ram[_addr*"^(string_of_int sw)^" + i];\n"
		| Erom (sa, sw, ra) -> 							 "\t\t_addr = toInt("^(eval_arg_array ra)^", "^(string_of_int sa)^");\n"
																				^"\t\trom("^var^", _rom, "^(string_of_int sw)^", _addr);\n"
	in
	let pre_expr = function
		| Eram (sa, sw, ra, we, wa, data) -> "\tchar * _ram = (char *) malloc("^(string_of_int ((1 lsl sa)*sw))^");\n"
		|	_ -> ""
	in
	let post_expr = function
		| Eram (sa, sw, ra, we, wa, data) -> "\t\tif("^(eval_arg_bit we)^") {\n"
																				^"\t\t\t_addr = toInt("^(eval_arg_array wa)^", "^(string_of_int sa)^");\n"
																				^"\t\t\tfor(int i = 0; i < "^(string_of_int sw)^"; i++) _ram[_addr*"
																				^(string_of_int sw)^" + i] = "^(sel "i" data)^";\n"
																				^"\t\t\tif(_addr >= 0 && _addr < _screenW*_screenH)"
																				^" writeChar(_addr%_screenW, _addr/_screenW,"
																				^" toInt("^(arg_var_name data)^", "^(string_of_int sw)^"));\n"
																				^"\t\t\tif(_addr == 0x10000) writeRedraw();\n"
																				^"\t\t\tif(_addr == 0x10001) { writeExit(); break; }\n"
																				^"\t\t}\n"
																				^"\t\ttick();\n"
		|	_ -> ""
	in
	let head = "#include <stdio.h>\n#include \"utils.c\"\n\nint main() {\n"	in
	let pre_exprs = (List.fold_left 
									(fun str (var, expr) -> str ^(pre_expr expr))
									"" p.p_eqs) in
	let init = "\tinit(_ram);\n"
				^"\tchar _screenW = readByte();\n"
				^"\tchar _screenH = readByte();\n"
				^"\tchar* _rom = readRom();\n"
				^"\trunKeyThread();\n" in
	let vars = "\tint _addr;\n" ^ Env.fold (fun var t str -> match t with
				|	TBit -> str ^ "\tchar " ^ var ^ " = 0;\n"
				| TBitArray n -> str ^ "\tchar * " ^ var ^ " = (char *) malloc("^(string_of_int n)^");\n"
				) p.p_vars "" in
	let exprs = "\n\twhile(1) {\n" ^ (List.fold_left
																								(fun str (var, expr) -> str ^ (eval_expr var expr))
																								"" p.p_eqs)
																 ^ (List.fold_left 
																								(fun str (var, expr) -> str ^ (post_expr expr))
																								"" p.p_eqs)
																 ^ "\t}\n}" in
	head ^ pre_exprs ^ init ^ vars ^ exprs
	
let compile filename =
  try
    let p = Netlist.read_file filename in
		print_endline "Scheduling the netlist...";
    let p = Scheduler.schedule p in
		print_endline "Writing C code...";
    let code = sim p in
		let oc = open_out (filename^".c") in
		fprintf oc "%s" code;
		close_out oc;
		print_endline "Code writen";
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