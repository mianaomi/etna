open Impl
open List


let rec insert_correct (k: int) (v: int) (t: tree) =
  match t with
  | E -> T (E, k, v, E)
  | T (l, k', v', r) ->
      if k < k' then T ((insert_correct k v l), k', v', r)
      else if k' < k then T (l, k', v', (insert_correct k v r))
      else T (l, k', v, r)


let bespoke =
  let open QCheck.Gen in
  sized (fun n -> list_repeat n (pair small_int small_int) >>= (fun kvs -> return (fold_left (fun t (k, v) -> insert_correct k v t) E kvs)))

let rec print_tree = function
  | E -> "Empty"
  | T (l, k, v, r) ->
      "Tree (" ^ print_tree l ^ "," ^ string_of_int k ^ "," ^ string_of_int v
      ^ "," ^ print_tree r ^ ")"

let bespoke = QCheck.make bespoke ~print:print_tree;;

