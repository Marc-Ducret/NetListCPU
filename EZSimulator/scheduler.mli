exception Combinational_cycle
val read_exp : 'a * Netlist_ast.exp -> Netlist_ast.ident list
val schedule : Netlist_ast.program -> Netlist_ast.program
