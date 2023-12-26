open Impl

let typebased =
  let tfun a b = TFun (a, b) in
  let e_var v = Var v in
  let e_bool b = Bool b in
  let e_abs t e = Abs (t, e) in
  let e_app e e' = App (e, e') in
  let open QCheck.Gen in
  let typeGen =
    let rec _typeGen n =
      if n <= 0 then return TBool
      else
        oneof [ map2 tfun (_typeGen (n / 2)) (_typeGen (n / 2)); return TBool ]
    in
    sized _typeGen
  in
  let rec _exprGen n =
    if n <= 0 then oneof [ map e_var nat; map e_bool bool ]
    else
      oneof
        [
          map2 e_abs typeGen (_exprGen (n / 2));
          map2 e_app (_exprGen (n / 2)) (_exprGen (n / 2));
        ]
  in
  sized _exprGen

let rec string_of_expr (e : expr) : string =
  let rec string_of_typ (t : typ) : string =
    match t with
    | TBool -> "TBool"
    | TFun (t1, t2) ->
        "TFun (" ^ string_of_typ t1 ^ ", " ^ string_of_typ t2 ^ ")"
  in
  match e with
  | Bool b -> "Bool " ^ string_of_bool b
  | Var i -> "Var " ^ string_of_int i
  | Abs (t, e') -> "Abs (" ^ string_of_typ t ^ ", " ^ string_of_expr e' ^ ")"
  | App (e1, e2) -> "App (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"

let typebased = QCheck.make typebased ~print:string_of_expr
