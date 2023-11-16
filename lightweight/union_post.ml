 (* (T (T E (Key (-3)) (Val False) E) (Key 3) (Val True) E,T E (Key (-3)) (Val True) (T E (Key 3) (Val True) E),Key (-3)) *)

type tree = E | T of tree * int * int * tree

let counter = (T (T (E, -3, 0, E), 3, 1, E), T (E, -3, 1, T (E, 3, 1, E)), -3)
