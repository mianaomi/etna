open Core

module Impl = struct 


type ('q) transition = 'q * Core.Char.t option * 'q [@@deriving sexp_of, quickcheck]

type ('q) fsm_t = {
  sigma: Core.Char.t Core.List.t;
  qs: 'q Core.List.t;
  q0: 'q;
  fs: 'q Core.List.t;
  delta: ('q) transition Core.List.t;
} [@@deriving sexp_of, quickcheck]


let rec elem x a =
  match a with
  | h::t -> (h = x) || (elem x t)
  | [] -> false

let rec insert x a =
  if not (elem x a) then x::a else a

let rec union a b =
  match a with
  | h::t -> insert h (union t b)
  | [] ->
    (match b with
     | h::t -> insert h (union [] t)
     | [] -> [])


let rec subset a b =
  match a with
    | h::t -> (elem h b) && (subset t b)
    | [] -> true
    
let rec eq a b = (subset a b) && (subset b a)

let rec intersection a b =
  match a with
    | h::t -> if elem h b then insert h (intersection t b) else (intersection t b)
    | [] -> []

let rec remove x a =
  match a with
  | h::t -> if h = x then t else h::(remove x t)
  | [] -> []
    
let rec diff a b =  
  match b with
  | [] -> a
  | h::t -> diff (remove h a) t
let rec minus a b = diff a b
    let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

let move (fsm:('q) fsm_t) (qs: 'q list) (s: char option) : 'q list =
  List.fold_left (fun acc x -> 
                     union (List.fold_left (fun a (tup1,tup2,tup3) ->  
                                          if (tup1=x && tup2=s)
                                            then tup3::a 
                                            else a) 
                        [] fsm.delta) acc 
                 ) 
                [] qs


let rec e_closure (fsm: ('q) fsm_t) (qs: 'q list) : 'q list = 
  let next_qs = union qs (move fsm qs None) 
     in
      if eq qs next_qs 
      then qs 
      else e_closure fsm next_qs
                
let accept (fsm: ('q) fsm_t) (s: string) : bool =
  let rec aux states input =
    match input with
    | [] -> (match (intersection fsm.fs (e_closure fsm states)) with
            |[] -> false
            |h::t-> true)
    | c :: cs -> aux (e_closure fsm (move fsm (e_closure fsm states) (Some c))) cs
  in
  aux [fsm.q0] (explode s)


let new_states (nfa: ('q) fsm_t) (qs: 'q list) : 'q list list =
  List.fold_right (fun sym acc ->
      let dest = e_closure nfa (move nfa qs (Some sym)) in
      if dest = [] then []::acc else dest::acc
  ) nfa.sigma []

            
let new_trans (nfa: ('q) fsm_t) (qs: 'q list): ('q list) transition list =
 List.fold_right (fun sym acc -> 
                      (qs, Some sym, (e_closure 
                                          nfa 
                                          (move 
                                              nfa 
                                              qs 
                                              (Some sym)
                                          )
                                  )
                        )::acc
                ) 
                nfa.sigma
                [] 
                
                   
                   
let new_finals (nfa: ('q) fsm_t) (qs: 'q list) : 'q list list =
if List.exists (fun q -> List.mem q nfa.fs) qs then [qs] else []
  let nfa_to_dfa_step (nfa: int fsm_t) (dfa: int list fsm_t) (worklist: int list list) =
    match worklist with
    | [] -> (dfa, [])
    | qs :: rest ->
      let states = new_states nfa qs in  
      let transitions = new_trans nfa qs in  
      let finals = new_finals nfa qs in 
        let updated_dfa = {
          sigma = dfa.sigma;
          qs = union dfa.qs states;
          q0 = dfa.q0;
          fs = union dfa.fs finals;
          delta = union dfa.delta transitions;
      } in
      
      let new_worklist = union rest (minus states (List.map (fun (t,a,b) -> t) dfa.delta)) in
      (updated_dfa, new_worklist)

  let nfa_to_dfa (nfa: ('q) fsm_t) : ('q list) fsm_t =
    let initial_state = e_closure nfa [nfa.q0] in  
    let initial_dfa = {
      sigma = nfa.sigma;
      qs = [initial_state];
      q0 = initial_state;
      fs = if List.exists (fun q -> List.mem q nfa.fs) initial_state then [initial_state] else [];
      delta = [];
    } in
    
    let rec loop dfa worklist =
      match worklist with
      | [] -> dfa  
      | _ ->
        let (updated_dfa, new_worklist) = nfa_to_dfa_step nfa dfa worklist in
        loop updated_dfa new_worklist
    in
    loop initial_dfa [initial_state] 

  end