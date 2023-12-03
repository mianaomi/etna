open Crowbar
open Impl
open Spec


(*
  Legacy functions in case you want to use the QCheck properties
  These should be removed in favor of more generalized properties
*)

let curry f a b = f (a, b)
let curry3 f a b c = f (a, b, c)
let test_of_prop p = (fun x -> check @@ p x)

let rec print_tree = function
  | E -> "Empty"
  | T (l, k, v, r) -> "Tree (" ^ (print_tree l) ^ "," ^ (string_of_int k) ^ "," ^ (string_of_int v) ^ "," ^ (print_tree r) ^ ")"


(* -------------------------------------------------------------- *)

(* Type-based Generator *)


let _cbtype : tree gen = fix (fun cbtype ->
  choose [
    const E;
    map [cbtype; cbtype; int8; int8] (fun l r k v -> T (l, k, v, r))];
  )

let rec pp_tree ppf tree =
  let open Format in
  match tree with
  | E ->
    fprintf ppf "E"
  | T (left, key, value, right) ->
    fprintf ppf "@[<hv 1>T(@[<hv 1>%a,@ %d,@ %d,@ %a@])@]"
      pp_tree left
      key
      value
      pp_tree right

let cbtype = with_printer pp_tree _cbtype

(* Temporary copied properties *)

let crow_prop_InsertValid : tree -> key -> value -> unit =
  fun t k v ->
    guard (isBST t);
    isBST (insert k v t) |> check

let crow_test_prop_InsertValid treeGen =
  let gs : (tree -> int -> int -> unit, unit) gens = [treeGen; int8; int8] in
  ("crow_prop_InsertValid", gs, crow_prop_InsertValid)

let crow_prop_InsertPost : tree -> key -> key -> value -> unit =
  fun t k k' v ->
    guard (isBST t);
    Impl.find k' (insert k v t) = (if k = k' then Some v else Impl.find k' t) |> check

let crow_test_prop_InsertPost treeGen =
  let gs : (tree -> int -> int -> int -> unit, unit) gens = [treeGen; int8; int8; int8] in
  ("crow_prop_InsertPost", gs, crow_prop_InsertPost)


(* Temporary runner code *)


let tests ts gen =
  ignore @@ List.map (fun t -> let (n, gs, p) = t gen in
    add_test ~name:n gs p) ts

let crow () =
  tests [
    crow_test_prop_InsertPost;
  ]
  cbtype