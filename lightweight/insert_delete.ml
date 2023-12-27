type color = B | R
type ('a, 'b) tree = E | T of color * ('a, 'b) tree * 'a * 'b * ('a, 'b) tree

let counter = (T (B, E, -1, 1, E), 1, 1, 0)

(* ((T B E (Key (-1)) (Val True) E), (Key 1), (Key 1), (Val False)) *)