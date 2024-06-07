open FSM.Impl
open List
open Util.Runner

(* Additional Functions *)

let ( === ) fsm_a fsm_b = 
  (Impl.eq fsm_a.sigma fsm_b.sigma) && (Impl.eq fsm_a.qs fsm_b.qs) 
  && (fsm_a.q0 == fsm_b.q0) && (Impl.eq fsm_a.fs fsm_b.fs) 
  && (Impl.eq fsm_a.delta fsm_b.delta)

let is_functional_FSM (fsm : ('q) fsm_t) : bool = failwith("not added")

(* -- Postcondition Properties. *)

let prop_movePost: ('q) fsm_t * 'q list  * char -> test = 
  fun (fsm, sl, c) -> is_functional_FSM fsm ->> 
  let result_states = Impl.move fsm sl (Some c) in
    List.for_all (fun state -> List.mem state fsm.qs) result_states
    
(*Assuming that we have a functional FSM, 
move(FSM, state)’s output should contain no states 
that are not in the machine’s state list.*)

let prop_acceptPost: ('q) fsm_t * ('q) fsm_t * string = 
  fun (fsm, fsm', s) -> is_functional_FSM fsm =>> is_functional_FSM fsm' 
  ->> (fsm === fsm') = ((Impl.accept fsm s) = (Impl.accept fsm' s))
(*Assuming that we have functional FSMs, 
if FSM1 = FSM2 then accept(FSM1, a string) 
should be the same as accept(FSM2, the same string)*)

let prop_conversionPost: ('q) fsm_t * string -> test = 
  fun (fsm, s) -> is_functional_FSM fsm 
  ->> ((Impl.accept fsm s) = (Impl.accept (Impl.nfa_to_dfa(fsm)) s))
(*Assuming that we have a functional FSM, 
accept(FSM, a string) should be the same 
as accept(nfa_to_dfa(FSM,  the same string))*)






