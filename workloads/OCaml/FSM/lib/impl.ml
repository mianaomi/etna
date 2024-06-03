type ('q) transition = 'q * char option * 'q [@@deriving sexp_of, quickcheck]

type ('q) fsm_t = {
  sigma: char list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q) transition list;
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
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

let move (fsm: fsm_t) (qs: int list) (s: char option) : int list =
  List.fold_left (fun acc x -> 
                     union (List.fold_left (fun a (tup1,tup2,tup3) ->  
                                          if (tup1=x && tup2=s)
                                            then tup3::a 
                                            else a) 
                        [] fsm.delta) acc 
                 ) 
                [] qs


let rec e_closure (fsm: fsm_t) (qs: int list) : int list = 
  let next_qs = union qs (move fsm qs None) 
     in
      if eq qs next_qs 
      then qs 
      else e_closure fsm next_qs
                
let accept (fsm: fsm_t) (s: string) : bool =
  let rec aux states input =
    match input with
    | [] -> (match (intersection fsm.fs (e_closure fsm states)) with
            |[] -> false
            |h::t-> true)
    | c :: cs -> aux (e_closure fsm (move fsm (e_closure fsm states) (Some c))) cs
  in
  aux [fsm.q0] (explode s)


let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_right (fun sym acc ->
      let dest = e_closure nfa (move nfa qs (Some sym)) in
      if dest = [] then []::acc else dest::acc
  ) nfa.sigma []

            
let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list): ('q list,'s) transition list =
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
                
                   
                   
let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
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
          qs = Sets.union dfa.qs states;
          q0 = dfa.q0;
          fs = Sets.union dfa.fs finals;
          delta = Sets.union dfa.delta transitions;
      } in
      
      let new_worklist = Sets.union rest (Sets.minus states (List.map (fun (t,a,b) -> t) dfa.delta)) in
      (updated_dfa, new_worklist)

  let nfa_to_dfa (nfa: (int) fsm_t) : (int list) fsm_t =
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