open Impl
open QCheck

let typebased =
  let open QCheck.Gen in
  let t c l k v r = T (c, l, k, v, r) in
  let rec tree_gen n =
    if n <= 0 then return E
    else
      Gen.(
        t
        <$> frequency [ (1, return R); (1, return B) ]
        <*> tree_gen (n / 2)
        <*> nat <*> nat
        <*> tree_gen (n / 2))
  in
  sized (fun n -> tree_gen n)

let rec print_tree t =
  match t with
  | E -> "Empty"
  | T (c, l, k, v, r) ->
      let cs = if c = R then "R" else "B" in
      "Tree (" ^ cs ^ "," ^ print_tree l ^ "," ^ string_of_int k ^ ","
      ^ string_of_int v ^ "," ^ print_tree r ^ ")"

let typebased = QCheck.make typebased ~print:print_tree

