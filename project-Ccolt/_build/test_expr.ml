(* 
                         CS 51 Final Project
                         MiniML -- Expr Tests
                             Spring 2018
*)

(* This is a testing module for functions defined in expr.ml *)

open Expr ;;
open Miniml ;;

(* let exp1 = str_to_exp "" ;; *)
let exp1 = Var("x") ;;
let exp2 = Num(1) ;;
let exp3 = Bool(true) ;;
let exp4 = Unop(Negate, Var("x")) ;;
let exp5 = Binop(Plus, Var("x"), Var("y")) ;;
let exp6 = Conditional(Var("x"), Var("y"), Num(2)) ;;
let exp7 = Fun("x", Num(1)) ;;
let exp8 = Fun("x", Binop(Equals, Var("y"), Var("x"))) ;;
let exp9 = Let("x", Num(1), Var("x")) ;;
let exp10 = Let("x", Var("x"), Binop(Plus, Var("x"), Var("y"))) ;;
let exp11 = Letrec("x", Var("x"), Num(0)) ;;
let exp12 = Letrec("x", Num(0), Var("x")) ;;
let exp13 = Raise ;;
let exp14 = Unassigned ;;
let exp15 = App(exp8, Num(3)) ;;
let exp16 = App(exp8, Var("z")) ;;
let repl = Var("y") ;;

let test_free_vars () : unit =
  assert(same_vars (free_vars exp1) (vars_of_list ["x"]));
  assert(same_vars (free_vars exp2) (vars_of_list []));
  assert(same_vars (free_vars exp3) (vars_of_list []));
  assert(same_vars (free_vars exp4) (vars_of_list ["x"]));
  assert(same_vars (free_vars exp5) (vars_of_list ["x";"y"]));
  assert(same_vars (free_vars exp6) (vars_of_list ["x";"y"]));
  assert(same_vars (free_vars exp7) (vars_of_list []));
  assert(same_vars (free_vars exp8) (vars_of_list ["y"]));
  assert(same_vars (free_vars exp9) (vars_of_list []));
  assert(same_vars (free_vars exp10) (vars_of_list ["x";"y"]));
  assert(same_vars (free_vars exp11) (vars_of_list ["x"]));
  assert(same_vars (free_vars exp12) (vars_of_list []));
  assert(same_vars (free_vars exp13) (vars_of_list []));
  assert(same_vars (free_vars exp14) (vars_of_list []));
  assert(same_vars (free_vars exp15) (vars_of_list ["y"]));
  assert(same_vars (free_vars exp16) (vars_of_list ["y";"z"]));;

let test_subst () : unit =
  assert(subst "x" repl exp1 = Var("y")) ;
  assert(subst "x" repl exp2 = Num(1)) ;
  assert(subst "x" repl exp3 = Bool(true)) ;
  assert(subst "x" repl exp4 = Unop(Negate, Var("y"))) ;
  assert(subst "x" repl exp5 = Binop(Plus, Var("y"), Var("y"))) ;
  assert(subst "x" repl exp6 = Conditional(Var("y"), Var("y"), Num(2))) ;
  assert(subst "x" repl exp7 = Fun("x", Num(1))) ;
  assert(subst "x" repl exp8 = Fun("x", Binop(Equals, Var("y"), Var("x")))) ;
  assert(subst "x" repl exp9 = Let("x", Num(1), Var("x"))) ;
  assert(subst "x" repl exp10 = Let("x", Var("y"), Binop(Plus, Var("x"), Var("y")))) ;
  assert(subst "x" repl exp11 = Letrec("x", Var("x"), Num(0))) ;
  assert(subst "x" repl exp12 = Letrec("x", Num(0), Var("x"))) ;
  assert(subst "x" repl exp13 = Raise) ;
  assert(subst "x" repl exp14 = Unassigned) ;
  assert(subst "x" repl exp15 = App(exp8, Num(3))) ;
  assert(subst "x" repl exp16 = App(exp8, Var("z"))) ;;

let test_exp_to_abstract_string () : unit =
  assert(exp_to_abstract_string exp1 = "Var(x)") ;
  assert(exp_to_abstract_string exp2 = "Num(1)") ;
  assert(exp_to_abstract_string exp3 = "Bool(true)") ;
  assert(exp_to_abstract_string exp4 = "Unop(Negate, Var(x))") ;
  assert(exp_to_abstract_string exp5 = "Binop(Plus, Var(x), Var(y))") ;
  assert(exp_to_abstract_string exp6 = "Conditional(Var(x), Var(y), Num(2))") ;
  assert(exp_to_abstract_string exp7 = "Fun(x, Num(1))") ;
  assert(exp_to_abstract_string exp8 = "Fun(x, Binop(Equals, Var(y), Var(x)))") ;
  assert(exp_to_abstract_string exp9 = "Let(x, Num(1), Var(x))") ;
  assert(exp_to_abstract_string exp10 = "Let(x, Var(x), Binop(Plus, Var(x), Var(y)))") ;
  assert(exp_to_abstract_string exp11 = "Letrec(x, Var(x), Num(0))") ;
  assert(exp_to_abstract_string exp12 = "Letrec(x, Num(0), Var(x))") ;
  assert(exp_to_abstract_string exp13 = "Raise") ;
  assert(exp_to_abstract_string exp14 = "Unassigned") ;
  assert(exp_to_abstract_string exp15 = "App(Fun(x, Binop(Equals, Var(y), Var(x))), Num(3))") ;
  assert(exp_to_abstract_string exp16 = "App(Fun(x, Binop(Equals, Var(y), Var(x))), Var(z))") ;;

let _ = test_free_vars() ;;
let _ = test_subst() ;;
let _ = test_exp_to_abstract_string();;

