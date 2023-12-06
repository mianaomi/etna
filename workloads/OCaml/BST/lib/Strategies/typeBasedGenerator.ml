open Impl

let typebased =
  let open QCheck.Gen in
  let rec tree_gen n =
    if n <= 0 then return E
    else
      frequency
        [
          (1, return E);
          ( 2,
            map2
              (fun (left, right) (key, value) -> T (left, key, value, right))
              (pair (tree_gen (n / 2)) (tree_gen (n / 2)))
              (pair nat nat) );
        ]
  in
  sized (fun n -> tree_gen n)

let rec print_tree = function
  | E -> "Empty"
  | T (l, k, v, r) ->
      "Tree (" ^ print_tree l ^ "," ^ string_of_int k ^ "," ^ string_of_int v
      ^ "," ^ print_tree r ^ ")"

let typebased = QCheck.make typebased ~print:print_tree
