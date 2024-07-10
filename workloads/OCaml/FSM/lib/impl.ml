
module Impl = struct
include Core 
type ('q, 's) transition = 'q * 's option * 'q [@@deriving sexp_of, quickcheck]
(*WILL BE MARKED AS NONEXISTANT UNTIL REDLINING IS FIXED!!!*)
type ('q, 's) fsm_t = {
  sigma: 's Core.List.t;
  qs: 'q Core.List.t;
  q0: 'q;
  fs: 'q Core.List.t;
  delta: ('q, 's) transition Core.List.t;
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

  let move (fsm: ('q, 's) fsm_t) (qs: 'q list) (s: 's) : 'q list =
    List.fold_left (fun acc x ->
      union acc (List.fold_left (fun a (tup1, tup2, tup3) ->
        (match (tup1, tup2, tup3) with
        | (q, sym, q') when q = x && sym = s -> q' :: a
        | _ -> a)
      ) [] fsm.delta)
    ) [] qs
  
    
let rec e_closure (fsm: ('q, 's) fsm_t) (qs: 'q list) : 'q list = 
  let next_qs = union qs (move fsm qs None) 
     in
      if eq qs next_qs 
      then qs 
      else e_closure fsm next_qs
                
let accept (fsm: ('q, 's) fsm_t) (s: string) : bool =
  let rec aux states input =
    match input with
    | [] -> (match (intersection fsm.fs (e_closure fsm states)) with
            |[] -> false
            |h::t-> true)
    | c :: cs -> aux (e_closure fsm (move fsm (e_closure fsm states) (Some c))) cs
  in
  aux [fsm.q0] (explode s)


let new_states (nfa: ('q, 's) fsm_t) (qs: 'q list) : 'q list list =
  List.fold_right (fun sym acc ->
      let dest = e_closure nfa (move nfa qs (Some sym)) in
      if dest = [] then []::acc else dest::acc
  ) nfa.sigma []

            
let new_trans (nfa: ('q, 's) fsm_t) (qs: 'q list): ('q list) transition list =
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
                
                   
                   
let new_finals (nfa: ('q, 's) fsm_t) (qs: 'q list) : 'q list list =
if List.exists qs (fun q -> (List.mem nfa.fs q)) then [qs] else []
  let nfa_to_dfa_step (nfa: ('q, 's) fsm_t) (dfa: ('q list, 's) fsm_t) (worklist: 'q list list) =
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

  let nfa_to_dfa (nfa: ('q, 's) fsm_t) : ('q list, 's) fsm_t =
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