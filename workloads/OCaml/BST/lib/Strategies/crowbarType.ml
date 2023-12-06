open Crowbar
open Impl

(* dune exec BST -- crowbar prop_InsertValid typeBasedCrowbar out2.txt *)

(* Type-based Generator *)

let _cbtype : tree gen =
  fix (fun cbtype ->
      choose
        [
          const E;
          map [ cbtype; cbtype; int8; int8 ] (fun l r k v -> T (l, k, v, r));
        ])

let rec pp_tree ppf tree =
  let open Format in
  match tree with
  | E -> fprintf ppf "E"
  | T (left, key, value, right) ->
      fprintf ppf "@[<hv 1>T(@[<hv 1>%a,@ %d,@ %d,@ %a@])@]" pp_tree left key
        value pp_tree right

let typebasedcrow = with_printer pp_tree _cbtype

