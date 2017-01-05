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
	let size_arg = function
		| Avar var -> size var
		| Aconst _ -> 1
	in
	let eval_var var =
		if is_array var then var^"[0]"
		else var
	in
	let eval_arg = function
		| Avar id -> eval_var id
		|	Aconst VBit true -> "1"
		|	Aconst VBit false -> "0"
		|	Aconst VBitArray _ -> failwith "array constants unsupported" in
	let sel i = function
		| Avar var when is_array var -> var^"["^i^"]"
		| x -> eval_arg x
	in
	let assign var str =
		"\t\t"^(eval_var var)^" = "^str^";\n"
	in
	let eval_expr var = function
		| Earg a -> assign var (eval_arg a)
		|	Ereg i -> assign var (eval_var i)
		|	Enot a -> assign var ("!" ^ (eval_arg a))
		| Ebinop (op, a, b) ->
				let va = eval_arg a in
				let vb = eval_arg b in
				assign var
				(match op with
					|	Or 		-> va^" | "^vb
					|	Xor 	-> va^" ^ "^vb
					| And 	-> va^" & "^vb
					|	Nand 	-> "!("^va ^" & "^vb^")"
				)
		| Emux (a, b, sel) -> 
				let vsel = eval_arg sel in
				let va = eval_arg a in
				let vb = eval_arg b in
				assign var (vsel^" ? "^vb^" : "^va)
		|	Econcat (a, b) ->
  			let sa = size_arg a in
  			let sb = size_arg b in
  				"\t\tfor(int i = 0; i < "^(string_of_int sa)^"; i++)\n\t\t\t"^(sel "i" (Avar var))^" = "^(sel "i" a)^";\n"
  			^ "\t\tfor(int i = 0; i < "^(string_of_int sb)^"; i++)\n\t\t\t"^(sel ("i+"^(string_of_int sa)) (Avar var))^" = "^(sel "i" b)^";\n"
		| Eslice (i, j, a) -> 
				"\t\tfor(int i = "^(string_of_int i)^"; i <= "^(string_of_int j)^"; i ++) "^(sel ("i-"^(string_of_int i)) (Avar var))
				^" = "^(sel "i" a)^";\n";
		| Eselect (i, a) -> assign var (sel (string_of_int i) a)
		| Eram (sa, sw, ra, we, wa, data) -> "\t\tint _addr = 0, _pow = 1;\n\t\tfor(int i = 0; i < "
																				^(string_of_int sa)^"; i++) {\n\t\t\t_addr += _pow * "^(sel "i" ra)^";\n"
																				^"\t\t\t_pow *= 2;\n\t\t}\n"
																				^"\t\tfor(int i = 0; i < "^(string_of_int sw)^"; i++) "^var^"[i] = "
																				^"_ram[_addr*"^(string_of_int sw)^" + i];\n"
		| Erom (sa, sw, ra) -> 							"\t\t_addr = 0; _pow = 1;\n\t\tfor(int i = 0; i < "
																				^(string_of_int sa)^"; i++) {\n\t\t\t_addr += _pow * "^(sel "i" ra)^";\n"
																				^"\t\t\t_pow *= 2;\n\t\t}\n"
																				^"\t\tfor(int i = 0; i < "^(string_of_int sw)^"; i++) "^var^"[i] = "
																				^"_rom[_addr*"^(string_of_int sw)^" + i];\n"
		|	_ -> failwith "not learned yet"
	in
	let pre_expr = function
		| Eram (sa, sw, ra, we, wa, data) -> "\tchar _ram["^(string_of_int ((1 lsl sa)*sw))^"] = {0};\n"
		|	_ -> ""
	in
	let post_expr = function
		| Eram (sa, sw, ra, we, wa, data) -> "\t\t_addr = 0; _pow = 1;\n\t\tfor(int i = 0; i < "
																				^(string_of_int sa)^"; i++) {\n\t\t\t_addr += _pow * "^(sel "i" wa)^";\n"
																				^"\t\t\t_pow *= 2;\n\t\t}\n"
																				^"\t\tfor(int i = 0; i < "^(string_of_int sw)^"; i++) _ram[_addr*"
																				^(string_of_int sw)^" + i] = "^(sel "i" data)^";\n"
		|	_ -> ""
	in
	let head = "#include <stdio.h>\n#include <utils.c>\n\nint main() {\n"
						^"\tchar _screenW = readByte();\n"
						^"\tchar _screenH = readByte();\n"
						^"\tchar* _rom = readRom();\n" in
	let vars = Env.fold (fun var t str -> match t with
				|	TBit -> str ^ "\tchar " ^ var ^ " = 0;\n"
				| TBitArray n -> str ^ "\tchar " ^ var ^ "[" ^(string_of_int n)^ "] = {0};\n"
				) p.p_vars "" in
	let pre_exprs = (List.fold_left 
																(fun str (var, expr) -> str ^(pre_expr expr))
																"" p.p_eqs) in
	let exprs = "\n\twhile(1) {\n" ^ (List.fold_left 
																								(fun str (var, expr) -> str ^ (eval_expr var expr))
																								"" p.p_eqs)
																 ^ (List.fold_left 
																								(fun str (var, expr) -> str ^ (post_expr expr))
																								"" p.p_eqs)
																 ^ "\t}\n}" in
	head ^ vars ^ pre_exprs ^ exprs
	
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