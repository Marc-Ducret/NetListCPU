

open Netlist_ast
open Netlist
open Scheduler

type queueMem = (*Type of the queue in the main function*)
  | Qram of Netlist_ast.arg*Netlist_ast.arg*Netlist_ast.arg
  | Qreg of string

let bitString_of_bool = function
  | true -> '1'
  | false -> '0'

exception NotABit
exception WrongLength of int
exception WrongType
exception NoType of string


      
let typeArg typMap x =
  (*Returns the type of x*)
  try Netlist_ast.Env.find x typMap
  with Not_found -> raise (NoType x)

                                            
let inputVal var x typMap =
  (*Inputs the value of x*)
  
  let rec inputBit () =
    (*Inputs a single bit*)
    print_string (x^" ? ");
    try let y = read_int() in
        if y = 1 then true else if y = 0 then false else raise NotABit
    with _ -> print_string "Wrong Input\n"; inputBit() in
  
  let rec inputBitString n =
    (*Inputs a bool array from a string*)

    let buildBitArray y n =
      let a = Array.make n false in
      let rec intToArray k = function
        | 0 -> ()
        | m -> begin if (m mod 10) = 0
                     then (a.(k) <- false; intToArray (k-1) (m/10))
                     else begin if ((m-1) mod 10) = 0
                                then (a.(k) <- true; intToArray (k-1) ((m-1)/10))
                     else raise NotABit end end
      in (intToArray (n-1) y); a
    in
    
    print_string (x^" ? ");
    try let y = read_int() in
        buildBitArray y n
    with _ -> print_string "Wrong Input\n"; inputBitString n

  in


  match typeArg typMap x with
  | TBit -> Hashtbl.add var x (VBit (inputBit()))
  | TBitArray n -> Hashtbl.add var x (VBitArray (inputBitString n))
                               
                                   
let retrieveVal var = function
  (*Computes arg *)
     | Aconst v -> v
     | Avar id -> Hashtbl.find var id

                               

let binop a b = function
  (*Applies the binary operator*)
  | Or -> (VBit (a||b))
  | And -> (VBit (a&&b))
  | Xor -> (VBit ((a||b)&&((not a)||not b)))
  | Nand -> VBit (not (a&&b))

                 
let int_of_value = function
  (*Gives the key to the ram hash table from a value*)
  | VBitArray arr -> Array.fold_left
                         (fun x a -> if a then 2*(x+1) else 2*x) 0 arr
  | VBit a -> if a then 1 else 0

let arg1 var x =
  (*Allows to use an array of size 1 instead of a single bit*)
  match (retrieveVal var x) with
  | VBit a | VBitArray [|a|] -> a
  | _ -> raise WrongType
               

let checkRAM ram wSize addr =
  (*If the key doesn't yet have a value, creates a blank
  initialisation value*)
  try Hashtbl.find ram addr
  with Not_found -> let blank =
         if wSize > 1
         then (VBitArray (Array.make wSize false))
         else (VBit false)
                    in Hashtbl.add ram addr blank;
                       blank

                    
let modifRAM var ram addrA (we,dataA) =
  (*If write is enabled, modifies the ram hash table*)
  match retrieveVal var we with
  | VBit true -> begin
          let addr = (int_of_value (retrieveVal var addrA)) in
          let data = (retrieveVal var dataA) in
          Hashtbl.replace ram addr data
      end
  | VBitArray _ -> raise WrongType
  | _ -> ()


let rec emptyQueue (var,reg,ram) queue =
  (*Operates on the memory hash tables until the queue is empty*)
  if (Queue.is_empty queue) then ()
  else begin begin match (Queue.take queue) with
       | Qram (we, w, data) -> if (arg1 var we) then
                     let addr = (int_of_value (retrieveVal var w)) in
                     let dataVal = (retrieveVal var data) in
                     Hashtbl.replace ram addr dataVal
       | Qreg a -> Hashtbl.replace reg a
               begin match (Hashtbl.find var a) with
               | VBit a | VBitArray [|a|] -> a
               | _ -> raise WrongType end
             end;
  emptyQueue (var,reg,ram) queue end
             
                    
let rec instrExe env queue = function
  (*Main function
Matches equations following the nature of the expression
At the end, calls emptyQueue*)
      
  | [] -> (emptyQueue env queue)
  | (id,ex)::l -> let (var,_,_) = env in let valu = begin match ex with
                                               
     | Earg a -> retrieveVal var a

     | Ereg a -> Queue.add (Qreg a) queue;
                 let (_,reg,_) = env in
                       VBit (Hashtbl.find reg a)
                              
     | Enot a -> (VBit (not (arg1 var a)))
                     
     | Ebinop (bin, a, b) -> ( binop (arg1 var a) (arg1 var b) bin )
                                      
     | Emux (s, a, b) -> ( if (arg1 var s) then (retrieveVal var a)
                                else (retrieveVal var b) )
                             
     | Erom (aSize, wSize, a) ->
        let (_,_,ram) = env in
        checkRAM ram wSize (int_of_value (retrieveVal var a))
                                          
     | Eram (aSize, wSize, a, we, w, data) ->
        Queue.add (Qram (we,w,data)) queue; 
        let (_,_,ram) = env in
        checkRAM ram wSize (int_of_value (retrieveVal var a))
                                 
     | Econcat (a,b) -> begin
           let conc =
             match ((retrieveVal var a), (retrieveVal var b)) with
             | VBit x, VBit y -> [|x;y|]
             | VBit x, VBitArray y -> (Array.append [|x|] y)
             | VBitArray x, VBit y -> (Array.append x [|y|])
             | VBitArray x, VBitArray y -> (Array.append x y)
           in (VBitArray conc) end
                            
     | Eslice (i1, i2, a) -> begin
             match (retrieveVal var a) with
             | VBitArray x -> ( try (VBitArray (Array.sub x i1 (i2-i1+1)))
                                with _ -> raise (WrongLength (i2-i1+1)) )
             | a -> if (i1=0 && i2=0) then a else raise WrongType
         end
                                 
     | Eselect (i,a) -> begin
             match (retrieveVal var a) with
             | VBitArray x -> ( try (VBit x.(i))
                                with _ -> raise (WrongLength i ))
             | a -> if i=0 then a else raise WrongType
         end
              end in
              Hashtbl.replace var id valu;
              instrExe env queue l

                   
let execute prog env =
  (*Execute one clock turn*)
  let (var,reg,ram) = env in 
  List.iter (fun x-> inputVal var x prog.p_vars) prog.p_inputs;
  instrExe env (Queue.create()) prog.p_eqs;
  List.iter (fun x-> print_string ("--> "^x^" = ");
        begin match Hashtbl.find var x with
        | VBit b -> print_char (bitString_of_bool b)
        | VBitArray b -> Array.iter (fun x-> print_char (bitString_of_bool x)) b
        end ; print_char '\n') prog.p_outputs



let rec repeat f n = function
        | 0 -> ()
        | k -> print_string ("\nStep "^(string_of_int n)^" :\n");
               f(); repeat f (n+1) (k-1)
let rec infLoop f n =
  print_string ("Step "^(string_of_int n)^" :\n");
  f() ; infLoop f (n+1)

                
let launchProg (prog, n, printOnly, progAddr) =
  (*Launches the program with its options*)
  if printOnly then
      let out = open_out ((String.sub progAddr 0
                                      ((String.length progAddr) - 4))
                         ^"_built.net") in
      Netlist.print_program out prog
  else

      let card = (Netlist_ast.Env.cardinal prog.p_vars)/5 + 1
   in let var = (Hashtbl.create (4*card) : (string, Netlist_ast.value) Hashtbl.t)
   in let reg = (Hashtbl.create card : (string, bool) Hashtbl.t)
   in let ram = (Hashtbl.create card : (int, Netlist_ast.value) Hashtbl.t)
   in
         
   let rec initVars = function
     | [] -> ()
     | (x, Ereg a)::l -> Hashtbl.add reg a false ;
                         initVars l
     | (_, Eram (s,n,a,_,_,_))::l | (_, Erom (s,n,a))::l -> initVars l
     | _ -> () (*Dans l'ordonnanceur, on s'arrête dès qu'on a vu
      les reg, qui ne peuvent être précédés que de rom ou de ram*)
   in
   initVars prog.p_eqs;
   
   let f () = execute prog (var,reg,ram) in
   
   if n=(-1) then infLoop f 1
   else if n>=0 then repeat f 1 n
   else failwith "Nombre d'itérations invalide"





let getProgOpt () =
  (*Gets the options from the command line*)
  let nCycle = ref (-1) in
  let printOnly = ref false in
  let spe = [
          ("-n", Arg.Set_int nCycle, "Number of cycles");
          ("-print", Arg.Set printOnly,
           "Doesn't execute the netlist, only produces the ordered netlist")]
  in
  let progAddr = ref "" in                     
  Arg.parse spe (fun x -> progAddr :=x) "Options available : ";
  let prog = Scheduler.schedule (Netlist.read_file !progAddr) in
  (prog, !nCycle, !printOnly, !progAddr)
                          
  
let () = launchProg (getProgOpt());;
