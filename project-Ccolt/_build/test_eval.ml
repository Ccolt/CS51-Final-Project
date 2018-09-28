(* 
                         CS 51 Final Project
                         MiniML -- Evaluation Tests
                             Spring 2018
*)

(* This is a testing module for functions defined in evaluation.ml *)

open Expr ;;
open Evaluation ;;
open Miniml ;;

let exp1 = Var("x") ;;
let exp2 = Num(1) ;;
let exp3 = Bool(true) ;;
let exp4 = Let("x", Num(2), Unop(Negate, Var("x"))) ;;
let exp5 = Let("x", Num(2), Let("y", Num(1), Binop(Plus, Var("x"), Var("y")))) ;;
let exp6 = Conditional(Bool(true), Num(1), Num(2)) ;;
let exp7 = App(Fun("x", Num(1)), Num(3)) ;;
let exp8 = Let("y", Num(2), (App (Fun("x", Binop(Equals, Var("y"), Var("x"))), Num(1)))) ;;
let exp9 = Let("f", Fun("x", Binop(Plus, Var("x"), Num(1))), App(Var("f"), App(Var("f"), Num(3))))  ;;
let exp10 = Letrec("f", 
                  Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), 
                           Num(0), 
                           App(Var("f"), Binop(Minus, Var("x"), Num(1))))),
                  App(Var("f"),Num(2))) ;;
let exp11 = Raise ;;
let exp12 = Unassigned ;;
let exp13 = Let("x", Num(1), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))), Let("x", Num(2), App(Var("f"), Num(3))))) ;;
let repl = Var("y") ;;

let test_eval func : unit =
  let tryoption1 = try Some (func exp1 (Env.create()))
    with EvalError _ -> None in
  let tryoption2 = try Some (func exp11 (Env.create()))
    with (EvalException) -> None in
  let tryoption3 = try Some (func exp12 (Env.create()))
    with EvalError _ -> None in
  assert(tryoption1 = None);
  assert(tryoption2 = None);
  assert(tryoption3 = None);
  assert(func exp2 (Env.create()) = Env.Val(Num(1)));
  assert(func exp3 (Env.create()) = Env.Val(Bool(true)));
  assert(func exp4 (Env.create()) = Env.Val(Num(-2)));
  assert(func exp5 (Env.create()) = Env.Val(Num(3)));
  assert(func exp6 (Env.create()) = Env.Val(Num(1))); 
  assert(func exp7 (Env.create()) = Env.Val(Num(1))); 
  assert(func exp8 (Env.create()) = Env.Val(Bool(false))); 
  assert(func exp9 (Env.create()) = Env.Val(Num(5))); 
  assert(func exp10 (Env.create()) = Env.Val(Num(0)));;

let _ = test_eval eval_s  ;;
let _ = test_eval eval_d ;;
(*let _ = test_eval eval_l ;;
*)
let _ = assert(eval_s exp13 (Env.create()) = Env.Val(Num(4)));;
let _ = assert(eval_d exp13 (Env.create()) = Env.Val(Num(5)));;
let _ = assert(eval_l exp13 (Env.create()) = Env.Val(Num(4)));;
