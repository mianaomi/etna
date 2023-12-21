open Impl
open Crowbar

let blacken_correct (t : ('a, 'b) tree) : ('a, 'b) tree =
  match t with E -> E | T (_, a, k, v, b) -> T (B, a, k, v, b)

let balance_correct (col : color) (tl : ('a, 'b) tree) (k : 'a) (v : 'b)
    (tr : ('a, 'b) tree) : ('a, 'b) tree =
  match (col, tl, k, v, tr) with
  | B, T (R, T (R, a, x, vx, b), y, vy, c), z, vz, d ->
      T (R, T (B, a, x, vx, b), y, vy, T (B, c, z, vz, d))
  | B, T (R, a, x, vx, T (R, b, y, vy, c)), z, vz, d ->
      T (R, T (B, a, x, vx, b), y, vy, T (B, c, z, vz, d))
  | B, a, x, vx, T (R, T (R, b, y, vy, c), z, vz, d) ->
      T (R, T (B, a, x, vx, b), y, vy, T (B, c, z, vz, d))
  | B, a, x, vx, T (R, b, y, vy, T (R, c, z, vz, d)) ->
      T (R, T (B, a, x, vx, b), y, vy, T (B, c, z, vz, d))
  | rb, a, x, vx, b -> T (rb, a, x, vx, b)

let insert_correct (k : 'a) (vk : 'b) (s : ('a, 'b) tree) : ('a, 'b) tree =
  let rec ins x vx t =
    match t with
    | E -> T (R, E, x, vx, E)
    | T (rb, a, y, vy, b) ->
        if x < y then balance_correct rb (ins x vx a) y vy b
        else if x > y then balance_correct rb a y vy (ins x vx b)
        else T (rb, a, y, vx, b)
  in
  blacken_correct (ins k vk s)

let format_color fmt c =
  match c with R -> Format.fprintf fmt "R" | B -> Format.fprintf fmt "B"

let rec format_tree fmt tree =
  match tree with
  | E -> Format.fprintf fmt "E"
  | T (color, left, key, value, right) ->
      Format.fprintf fmt "(T %a %a %d %d %a)" format_color color format_tree
        left key value format_tree right

let listn (g : 'a gen) : 'a list gen =
  fix (fun listn ->
      dynamic_bind (range 15) (fun n ->
          if n = 0 then const []
          else
            dynamic_bind g (fun x ->
                dynamic_bind listn (fun xs -> const (x :: xs)))))

let _cbbespoke : rbt gen =
  dynamic_bind
    (listn (pair int8 int8))
    (fun kvs ->
      const (List.fold_left (fun t (k, v) -> insert_correct k v t) E kvs))

let bespokecrow = with_printer format_tree _cbbespoke
