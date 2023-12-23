open Impl
open Crowbar

let typegen : typ gen =
  let t_fun t t' = TFun (t, t') in
  fix (fun typeGen -> choose [ const TBool; map [ typeGen; typeGen ] t_fun ])

let crowbarType : expr gen =
  let e_var v = Var v in
  let e_bool b = Bool b in
  let e_abs t e = Abs (t, e) in
  let e_app e e' = App (e, e') in
  fix (fun exprGen ->
      choose
        [
          map [ uint8 ] e_var;
          map [ bool ] e_bool;
          map [ typegen; exprGen ] e_abs;
          map [ exprGen; exprGen ] e_app;
        ])

(* i dont think this works in terms of making variables match up *)
let format_expr fmt e =
  let string_of_var n =
    String.make 1 (String.get "abcdefghijklmnopqrstuvwxyz" (n mod 26))
  in
  let rec format_typ fmt t =
    match t with
    | TBool -> Format.fprintf fmt "Bool"
    | TFun (t, t') -> Format.fprintf fmt "(%a -> %a)" format_typ t format_typ t'
  in
  let rec _format_expr fmt (x, e) =
    match e with
    | Var n -> Format.fprintf fmt "%s" (string_of_var n)
    | Bool b -> Format.fprintf fmt "%s" (string_of_bool b)
    | Abs (t, e') ->
        Format.fprintf fmt "\\%s:%a, %a" (string_of_var x) format_typ t
          _format_expr
          (1 + x, e')
    | App (e1, e2) ->
        Format.fprintf fmt "%a %a" _format_expr (x, e1) _format_expr (x, e2)
  in
  _format_expr fmt (0, e)

let crowbarType = with_printer format_expr crowbarType
