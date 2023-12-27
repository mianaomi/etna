type tree = E | T of tree * int * int * tree

let counter = (T (E, -1, 1, T (E, 2, 1, E)), T (E, 2, 0, E), T (E, 0, 1, E))
