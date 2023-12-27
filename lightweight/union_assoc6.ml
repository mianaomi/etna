type tree = E | T of tree * int * int * tree

let counter = (T (E, 2, 0, E), T (E, 0, 0, E), T (T (E, -2, 1, E), 2, 1, E))
(* (T E (Key 2) (Val False) E,T E (Key 0) (Val False) E,T (T E (Key (-2)) (Val True) E) (Key 2) (Val True) E) *)
