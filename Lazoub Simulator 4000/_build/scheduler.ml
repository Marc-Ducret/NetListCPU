open Netlist_ast
open Graph

exception Combinational_cycle
exception Problem of string

let read_exp (eq : Netlist_ast.equation) =
  let id,ex = eq in
  let rec test  = function
    | [] -> []
    |(Netlist_ast.Avar i)::l -> i::(test l)
    |_::l -> test l in
  let testEx = function
    | Earg arg -> test [arg]
    | Ereg i -> [i]
    | Enot arg -> test [arg]
    | Ebinop (_, a1,a2) -> test [a1;a2]
    | Emux (a,b,c) -> test [a;b;c]
    | Erom (_,_,a) -> test [a]
    | Eram (_,_,a1,a2,a3,a4) -> test [a1;a2;a3;a4]
    | Econcat (a,b) -> test [a;b]
    | Eslice (_,_,a) -> test [a]
    | Eselect (_,a) -> test [a]
  in
  testEx ex

let reg = function
  (*Teste si l'expression se rapporte à un registre*)
  | Ereg _ -> true
  | _ -> false

let ram = function
  (*Teste si l'expression se rapporte à une mémoire ram ou rom*)
  | Eram _ | Erom _ -> true
  | _ -> false

let identif id lis =
  (*Renvoie l'équation dont la variable modifiée est id*)
  try List.find (fun (i,n)-> i=id) lis
  with Not_found ->
    raise (Problem ("Impossible d'identifier la variable "^id
           ^" parmi la liste : "
           ^(List.fold_right (fun (a,b) x -> x^a^", ") lis "")))
                
let memnec id eqL =
  try
      begin match (identif id eqL) with (id,ini) -> (ram ini || reg ini) end
  with _ -> false
                   
let notExistsG gr id =
  List.for_all (fun x-> (x.n_label <> id) ) gr.g_nodes
           
let schedule p =
  let g = Graph.mk_graph() in
  let regL = ref [] in
  let ramL = ref [] in

  let rec dealNode n = function
    (*Pour un noeud n donné, et une liste de variable, pose chaque
    variable comme parent de n. Ne rajoute pas les variables
    demandant de la mémoire (reg, rom, ram) au graphe*)
    |[] -> ()
    |a::l -> begin let add = not (memnec a p.p_eqs) in if add then begin
                        begin if (notExistsG g a)
                              then Graph.add_node g a end;
                    (Graph.add_edge g a n) end; dealNode n l
         end
  in

  let rec sortEqs = function
    (*Prend en entrée la liste des équations à traiter.
     Toutes celles ne nécessitant pas la mémoire sont introduites
    dans un graphe pour un tri topologique ultérieur.
     Celles nécessitant les registres sont placées dans une liste à
    part
     Celles nécessitant la mémoire RAM ou ROM sont placées dans une
    autre liste à part, puis, lorsque toutes les équations ont été
    traitées, elles sont introduites soit dans la liste des registres
    si l'adresse de lecture est en entrée (pas besoin de la
    calculer), soit comme fille de leur addresse de lecture dans le
    graphe si ce l'addresse de lecture doit être calculée. *)
    | [] -> begin List.iter (fun (id,addr) ->
                          if notExistsG g addr
                          then regL:= (identif id p.p_eqs)::!regL
                          else (Graph.add_node g id ; Graph.add_edge g addr id) 
                      ) !ramL end
                          
    | x::ll -> begin match x with (id,ini) ->
         begin
            if reg ini then regL := x::!regL else
            let vars = (read_exp x) in
                begin 
                    if ram ini then ramL := ((id,(List.hd vars))::!ramL) else
                        begin
                            begin if notExistsG g id then (Graph.add_node g id) end ;
                            dealNode id vars
                        end                         
                end
         end ; sortEqs ll
               end
  in

  sortEqs p.p_eqs;
          
  try let sortedG = Graph.topological g in  

      let rec result = function
        (*Prend une liste d'identifiants et renvoie la liste
        d'équation correspondant, dans le même ordre*)
        | [] -> []
        | id::l -> try begin
              let x = identif id p.p_eqs in
              x::(result l) end
                   with _ -> result l
                                            
      in {p_eqs = (!regL)@(result sortedG) ; p_inputs = p.p_inputs ;
              p_outputs = p.p_outputs ; p_vars = p.p_vars}
 
  with Graph.Cycle->raise Combinational_cycle
     | (Problem a) -> raise (Problem ("Final : "^a))
                   
