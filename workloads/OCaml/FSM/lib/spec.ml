open Impl
open List
open Util.Runner

module Spec = struct 
(* Additional Functions *)

let ( === ) fsm_a fsm_b = 
  (Impl.eq fsm_a.sigma fsm_b.sigma) && (Impl.eq fsm_a.qs fsm_b.qs) 
  && (fsm_a.q0 == fsm_b.q0) && (Impl.eq fsm_a.fs fsm_b.fs) 
  && (Impl.eq fsm_a.delta fsm_b.delta)

  let all_unique lst =
    let rec aux seen = function
      | [] -> true
      | x :: xs ->
        if List.mem x seen then false
        else aux (x :: seen) xs
    in
    aux [] lst
  
let is_functional_FSM (fsm : ('q) fsm_t) : bool = 
  let translegal = 
    List.for_all (fun (start, symbol, finish) ->
      (List.mem start fsm.qs) && (List.mem finish fsm.qs) &&
      (match symbol with
       | None -> true
       | Some c -> List.mem c fsm.sigma)
    ) fsm.delta
  in
  let reaches q0 qs delta fs =
    let rec dfs visited state =
      if List.mem state visited then visited
      else
        let next_states = List.fold_left (fun acc (start, _, finish) ->
          if start = state then finish :: acc else acc
        ) [] delta 
        in List.fold_left dfs (state :: visited) next_states
    in
    let reachable_states = dfs [] q0 
    in ()
  in
  List.for_all (fun state -> List.mem state reachable_states fs) &&
  (List.mem fsm.q0 fsm.qs) && 
  (Impl.subset fsm.fs fsm.qs) && 
  (translegal) && 
  (all_unique fsm.sigma) && 
  (all_unique fsm.delta) && 
  (all_unique fsm.fs) && 
  (all_unique fsm.qs)
(*
   1. can reach every fs from q0
   2. q0 in qs - list.mem
   3. fs in qs - subset 
   4. Transitions legal: 
    a. uses none or a char in sigma 
    b. states on both ends are in qs 
   5. sigma, delta, fs, qs no repeats - all_unique
*)

(* -- Postcondition Properties. *)

let prop_conversionPost: ('q) fsm_t * string -> test = 
  fun (fsm, s) -> is_functional_FSM fsm 
  ->> ((Impl.accept fsm s) = (Impl.accept (Impl.nfa_to_dfa(fsm)) s))
(*Assuming that we have a functional FSM, 
accept(FSM, a string) should be the same 
as accept(nfa_to_dfa(FSM,  the same string))*)

(*
Postponed
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

*)






end