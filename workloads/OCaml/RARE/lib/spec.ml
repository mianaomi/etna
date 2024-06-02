open Util.Runner
open Impl

(* Rarity Properties *)

(* everything goes through the precondition, property fails rarely *)
let prop_NothingPasses : string -> test =
  fun s -> true ->> rarely_false s