open Impl
open Crowbar

let _cbtype : rbt gen =
  let t c l k v r = T (c, l, k, v, r) in
  fix (fun cbtype ->
      choose
        [
          const E;
          map [ choose [ const R; const B ]; cbtype; int8; int8; cbtype ] t;
        ])

let format_color fmt c =
  match c with R -> Format.fprintf fmt "R" | B -> Format.fprintf fmt "B"

let rec format_tree fmt tree =
  match tree with
  | E -> Format.fprintf fmt "E"
  | T (color, left, key, value, right) ->
      Format.fprintf fmt "(T %a %a %d %d %a)" format_color color format_tree
        left key value format_tree right

let typebasedcrow = with_printer format_tree _cbtype
