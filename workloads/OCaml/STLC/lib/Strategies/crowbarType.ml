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
let rec format_expr fmt e =
  let rec format_typ fmt t =
    match t with
    | TBool -> Format.fprintf fmt "TBool"
    | TFun (t, t') -> Format.fprintf fmt "TFun (%a, %a)" format_typ t format_typ t'
  in
  match e with
  | Bool b -> Format.fprintf fmt "Bool %b" b
  | Var i -> Format.fprintf fmt "Var %i" i
  | Abs (t, e') -> Format.fprintf fmt "Abs (%a, %a)" format_typ t format_expr e'
  | App (e1, e2) -> Format.fprintf fmt "App (%a, %a)" format_expr e1 format_expr e2

let crowbarType = with_printer format_expr crowbarType
