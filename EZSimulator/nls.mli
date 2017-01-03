exception Incompatible_Sizes
val print_only : bool ref
val number_steps : int ref
val sim : Netlist_ast.program -> unit
val compile : string -> unit
val main : unit -> unit
