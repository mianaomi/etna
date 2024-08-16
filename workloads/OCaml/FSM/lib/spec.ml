open Impl
open List
open Util.Runner

module Spec = struct 
(* Additional Functions *)
  let all_unique lst =
    let rec aux seen = function
      | [] -> true
      | x :: xs ->
        if List.mem x seen then false
        else aux (x :: seen) xs
    in
    aux [] lst
  
    let is_functional_FSM (fsm : ('q) fsm_t) : bool = 
      (*check if all elements in list are unique*)
      let all_unique lst =
        let rec aux seen = function
          | [] -> true
          | x :: xs -> if List.mem x seen then false else aux (x :: seen) xs
        in
        aux [] lst
      in
    
      (*transition Elements in Sigma + qs*)
      let translegal = 
        List.for_all (fun (start, symbol, finish) ->
          (List.mem start fsm.qs) && (List.mem finish fsm.qs) &&
          (match symbol with
           | None -> true
           | Some c -> List.mem c fsm.sigma)
        ) fsm.delta
      in
    
      (*DFS for reachability*)
      let reaches q0 qs delta =
        let rec dfs visited state =
          if List.mem state visited then visited
          else
            let next_states = List.fold_left (fun acc (start, _, finish) ->
              if start = state then finish :: acc else acc
            ) [] delta 
            in List.fold_left dfs (state :: visited) next_states
        in
        dfs [] q0
      in
    
      (*calls DFS Helper*)
      let reachable_states = reaches fsm.q0 fsm.qs fsm.delta in
    
      (*call checks*)
      ((List.length fsm.qs) > 0) && (*qs > 0*)
      (List.mem fsm.q0 fsm.qs) &&  (*q0 in qs*)
      ((List.length fsm.fs) > 0) && (*fs > 0*)
      (List.for_all (fun state -> List.mem state fsm.qs) fsm.fs) && (*fs subset qs*)
      (translegal) && (*all transitions legal*)
      List.for_all (fun state -> List.mem state reachable_states) fsm.qs && (*all states are reachable from q0*)
      all_unique fsm.sigma && (*sigma all unique*)
      all_unique fsm.qs && (*qs all unique*)
      all_unique fsm.fs && (*fs all unique*)
      all_unique fsm.delta (*delta all unique*)

(* -- Postcondition Properties. *)

let prop_conversionPost: ('q) fsm_t * string -> test = 
  fun (fsm, s) -> is_functional_FSM fsm 
  ->> ((Impl.accept fsm s) = (Impl.accept (Impl.nfa_to_dfa(fsm)) s))
(*Assuming that we have a functional FSM, 
accept(FSM, a string) should be the same 
as accept(nfa_to_dfa(FSM),  the same string))*)

(*
Potential Properties
- isValidFSM(fsm) => nfa_to_dfa: For each state and input symbol, there is exactly one transition to a next state.
- isValidFSM(fsm) => isValidFSM(nfa_to_dfa(fsm))
- isValidFSM(fsm) => length(e_closure(nfa_to_dfa(fsm) state_list)) = length(state_list) 
- isValidFSM(fsm) => (move(fsm)) can reach every state in qs that's not q0
*)






end