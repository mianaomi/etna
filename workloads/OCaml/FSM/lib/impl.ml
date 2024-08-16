
module Impl = struct
include Core 
include Base.Container

type q =
  | Int of int
  | Int_list of int list [@@deriving sexp_of, quickcheck]

type s =
  | Char of char option
  | Char_list of (char list) option [@@deriving sexp_of, quickcheck]

type transition = q * s * q [@@deriving sexp_of, quickcheck]

type  fsm_t = {
  sigma: s List.t;
  qs: q List.t;
  q0: q;
  fs: q List.t;
  delta: transition List.t;
} [@@deriving sexp_of, quickcheck]

let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []
let  q_equal (a: q) (b: q) =
  match (a, b) with 
  | (Int x, Int y) -> (x=y)
  | (Int_list x, Int_list y) ->  Base.List.equal Int.equal x y
  | (Int x, Int_list y) -> false
  | (Int_list x, Int y) -> false
  | _ -> false

    let  s_equal (a: s) (b: s) =
    match (a, b) with 
    | (Char Some x, Char Some y) -> 
      (Char.equal x y)
    | (Char None, Char None) -> true
    | (Char_list None, Char_list None) -> true
    | (Char_list Some x, Char_list Some y) ->  Base.List.equal Char.equal x y
    | (Char Some x, Char_list Some y) -> false
    | (Char_list Some x, Char Some y) -> false
    | _ -> false

  let move (fsm: fsm_t) (frm: q) (trns: s) : q list =
    List.fold 
      fsm.delta 
      ~init:[] 
      ~f:(fun acc (src, sym, dst) ->
        if q_equal src frm && s_equal sym trns 
        then dst :: acc 
        else acc
      )

  let rec union (set_a: q list) (set_b: q list) : q list =
    Base.List.fold set_b ~init:set_a ~f:(fun acc q ->
     f List.exists acc ~f:(q_equal q) then acc else q :: acc
   )
    
  let rec e_closure (fsm: fsm_t) (qs: q list) : q list = 
    let next_states =
      List.fold qs ~init:qs ~f:(fun acc q ->
        let reachable = move (fsm) q (Char None) in
          union acc reachable
        )
        in
        if Base.List.equal q_equal qs next_states then
          qs
        else
          e_closure fsm next_states
                
   let accept_fsm (fsm: fsm_t) (s: string) : bool =
    let pieces = explode s in
    let is_dfa = List.exists fsm.delta ~f:(fun (_, sym, _) ->
      match sym with
        | Char_list _ -> true
        | _ -> false
      ) 
    in
    if is_dfa then
      let rec find_transition cursor remaining_pieces =
        match remaining_pieces with
        | [] -> List.exists fsm.fs ~f:(q_equal cursor)
        | _ ->
          let rec try_pieces i =
            if i > List.length remaining_pieces then false
            else
              let segment = List.take remaining_pieces i in
              match move fsm cursor (Char_list (Some segment)) with
              | [next_state] ->
                let remaining = List.drop remaining_pieces i in
                find_transition next_state remaining
              | _ -> try_pieces (i + 1)
          in
          try_pieces 1
      in
      find_transition fsm.q0 pieces
    else
      let rec step states chars =
        match chars with
        | [] -> states
        | c :: rest ->
          let next_states =
            List.fold states ~init:[] ~f:(fun acc state ->
              let moved_states = move fsm state (Char (Some c)) in
              let e_closure_states = e_closure fsm moved_states in
              Base.List.fold e_closure_states ~init:acc ~f:(fun a s -> if List.exists a ~f:(q_equal s) then a else s :: a)
            )
          in
          step next_states rest
      in
      let initial_states = e_closure fsm [fsm.q0] in
      let final_states = step initial_states pieces in
      List.exists final_states ~f:(fun state ->
        List.exists fsm.fs ~f:(q_equal state)
      )

  let nfa_to_dfa (nfa: fsm_t) : fsm_t = ()


  end