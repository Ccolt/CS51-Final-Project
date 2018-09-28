(* 
                         CS 51 Final Project
                        MiniML -- Expressions
                             Spring 2018
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;
      
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;
  
(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* Function to print the variables in a varidset (for testing)  *)
let print_vars (varset : varidset) : unit =
  let vars = SS.elements varset in
  let rec print_vars' vlst =
    match vlst with
    | [] -> ()
    | h :: t -> print_string h; print_vars' t in
    print_vars' vars ;; 

(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let free_vars (exp : expr) : varidset =
  let varset = ref SS.empty in
  let rec free_vars' (expr : expr) : unit =
    match expr with
    | Var id -> varset := (SS.add id !varset)
    | Num _ -> ()
    | Bool _ -> ()
    | Unop (_, expr) -> free_vars' expr
    | Binop (_, exp1, exp2) -> free_vars' exp1; free_vars' exp2
    | Conditional (exp1, exp2, exp3) -> free_vars' exp1; 
                                        free_vars' exp2;
                                        free_vars' exp3;
    | Fun (varid, expr) -> free_vars' expr; varset := SS.remove varid !varset
    | Let (varid, exp1, exp2) -> free_vars' exp2; 
                                 varset := SS.remove varid !varset;
                                 free_vars' exp1
    | Letrec (varid, exp1, exp2) -> free_vars' exp2;
                                 varset := SS.remove varid !varset;
                                 free_vars' exp1
    | Raise -> ()
    | Unassigned -> ()
    | App (exp1, exp2) -> free_vars' exp1; free_vars' exp2
    in
  free_vars' exp; !varset ;;

(* Reference to hold incrementing varid integer  *)
let intref = ref ~-1 ;;

(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname () : varid =
  intref := !intref + 1; "var" ^ string_of_int !intref ;;
(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let subst' = subst var_name repl in
  let freevars = free_vars exp in
  let is_free = SS.mem var_name freevars in
    match exp with
    | Var id -> if var_name = id then repl else Var(id)
    | Num i -> Num(i) 
    | Bool b -> Bool(b)
    | Unop (op, exp1) -> Unop (op, subst' exp1)
    | Binop (op, exp1, exp2) -> Binop (op, subst' exp1, 
                                           subst' exp2)
    | Conditional (exp1, exp2, exp3) -> Conditional (subst' exp1, 
                                                     subst' exp2,
                                                     subst' exp3)
    | Fun (varid, exp1) -> 
        if varid = var_name then exp 
        else if not is_free then
          Fun (varid, subst' exp1)
        else
          let new_var = new_varname () in
          Fun (new_var, subst' 
               (subst varid (Var(new_var)) exp1))
    | Let (varid, exp1, exp2) -> 
        if varid = var_name then
          Let (varid, subst' exp1, exp2)
        else if not is_free then
          Let (varid, subst' exp1, subst' exp2)
        else
          let new_var = new_varname () in
          Let (new_var, subst' exp1, subst'
               (subst varid (Var(new_var)) exp2))
    | Letrec (varid, exp1, exp2) ->
        if varid = var_name then exp
        else if not is_free then
          Letrec (varid, subst' exp1, subst' exp2)
        else
          let new_var = new_varname () in
          Letrec (new_var, subst' exp1, subst'
               (subst varid (Var(new_var)) exp2))
    | Raise -> Raise
    | Unassigned -> Unassigned
    | App (exp1, exp2) -> App (subst' exp1,
                               subst' exp2) ;;
(*......................................................................
  String representations of expressions
 *)

(* Returns a concrete string representation of a unop  *)
let unop_to_conc_str (op : unop) =
  match op with
  | Negate -> "-"

(* Returns a concrete string representation of a binop  *)
let binop_to_conc_str (op : binop) =
  match op with
  | Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Equals -> " = "
  | LessThan -> " < "
  
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var id -> id
  | Num i -> string_of_int i                 
  | Bool b -> if b then "True" else "False"        
  | Unop (op, expr) -> (unop_to_conc_str op) ^ "(" ^ 
                       (exp_to_concrete_string expr) ^ ")"
  | Binop (op, exp1, exp2) -> "((" ^ (exp_to_concrete_string exp1) ^ ")" ^
                              (binop_to_conc_str op) ^
                              "(" ^ (exp_to_concrete_string exp2) ^ "))"
  | Conditional (exp1, exp2, exp3) -> "(if " ^ (exp_to_concrete_string exp1) ^
                                      " then " ^ (exp_to_concrete_string exp2) ^
                                      " else " ^ (exp_to_concrete_string exp3) ^
                                      ")"
  | Fun (varid, expr) -> "(fun " ^ varid ^ " -> " ^ 
                         (exp_to_concrete_string expr) ^ ")"
  | Let (varid, exp1, exp2) -> "let " ^ varid ^ " = " ^ 
                               (exp_to_concrete_string exp1) ^ " in " ^
                               (exp_to_concrete_string exp2)
  | Letrec (varid, exp1, exp2) -> "let rec " ^ varid ^ " =  " ^
                               (exp_to_concrete_string exp1) ^ " in " ^
                               (exp_to_concrete_string exp2)
  | Raise -> "Raise" 
  | Unassigned -> "Unassigned"
  | App (exp1, exp2) -> "(" ^ (exp_to_concrete_string exp1) ^ " " ^
                         (exp_to_concrete_string exp2) ^ ")" ;;

(* Returns an abstract string representation of a unop  *)
let unop_to_abstr_str (op : unop) : string =
  match op with
  | Negate -> "Negate" ;;

(* Returns an abstract string representation of a binop  *)
let binop_to_abstr_str (op : binop) : string = 
  match op with
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Equals -> "Equals"
  | LessThan -> "LessThan"

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var id -> "Var(" ^ id ^ ")"
  | Num i -> "Num(" ^ string_of_int i ^ ")"               
  | Bool b -> "Bool(" ^ (if b then "true" else "talse") ^ ")"        
  | Unop (op, expr) -> "Unop(" ^ (unop_to_abstr_str op) ^ ", " ^
                       (exp_to_abstract_string expr) ^ ")"
  | Binop (op, exp1, exp2) -> "Binop(" ^ (binop_to_abstr_str op) ^ ", " ^
                              (exp_to_abstract_string exp1) ^ ", " ^
                              (exp_to_abstract_string exp2) ^ ")"
  | Conditional (exp1, exp2, exp3) -> "Conditional(" ^
                                      (exp_to_abstract_string exp1) ^ ", " ^
                                      (exp_to_abstract_string exp2) ^ ", " ^
                                      (exp_to_abstract_string exp3) ^ ")"
  | Fun (varid, expr) -> "Fun(" ^ varid ^ ", " ^
                         (exp_to_abstract_string expr) ^ ")"
  | Let (varid, exp1, exp2) -> "Let(" ^ varid ^ ", " ^ 
                               (exp_to_abstract_string exp1) ^ ", " ^
                               (exp_to_abstract_string exp2) ^ ")"
  | Letrec (varid, exp1, exp2) -> "Letrec(" ^ varid ^ ", " ^
                               (exp_to_abstract_string exp1) ^ ", " ^
                               (exp_to_abstract_string exp2) ^ ")"
  | Raise -> "Raise" 
  | Unassigned -> "Unassigned"
  | App (exp1, exp2) -> "App(" ^ (exp_to_abstract_string exp1) ^ ", " ^
                         (exp_to_abstract_string exp2) ^ ")" ;;
