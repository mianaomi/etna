type transition = int * char option * int 


type fsm_t = {
  sigma: char list;
  qs: int list;
  q0: int;
  fs: int list;
  delta: transition list;
} 


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