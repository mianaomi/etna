open Impl
open List
open Util.Runner

(* Additional Functions *)

let rec ( === ) fsm_a fsm_b = failwith("not added")

let is_functional_FSM = failwith("not added")

(* -- Validity Properties. *)

let prop_moveValid = failwith("not added")

(* ---------- *)

(* -- Postcondition Properties. *)

let prop_movePost = failwith("not added")
(*Assuming that we have a functional FSM, 
move(FSM, state)’s output should contain no states 
that are not in the machine’s state list.*)
let prop_acceptPost = failwith("not added")
(*Assuming that we have functional FSMs, 
if FSM1 = FSM2 then accept(FSM1, a string) 
  should be the same as accept(FSM2, the same string)*)
let prop_conversionPost = failwith("not added")
(*Assuming that we have a functional FSM, 
accept(FSM, a string) should be the same 
as accept(nfa_to_dfa(FSM,  the same string))*)






