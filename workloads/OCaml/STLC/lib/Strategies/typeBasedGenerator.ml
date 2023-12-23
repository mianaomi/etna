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
  sized (fun n -> _exprGen n)

(* i think this is wrong *)
let print_expr e =
  let var_to_string n =
    String.make 1 (String.get "abcdefghijklmnopqrstuvwxyz" (n mod 26))
  in
  let rec print_typ t =
    match t with
    | TBool -> "Bool"
    | TFun (t, t') -> "(" ^ print_typ t ^ "->" ^ print_typ t' ^ ")"
  in
  let rec _print_expr n e =
    match e with
    | Var n -> var_to_string n
    | Bool b -> string_of_bool b
    | Abs (t, e') ->
        "\\" ^ var_to_string n ^ ":" ^ print_typ t ^ ","
        ^ _print_expr (1 + n) e'
    | App (e1, e2) -> _print_expr n e1 ^ " " ^ _print_expr n e2
  in
  _print_expr 0 e

let typebased = QCheck.make typebased ~print:print_expr
