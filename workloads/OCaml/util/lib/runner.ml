(* `cbtest`s for Crowbar are just lazy calls to Crowbar.add_test *)
type qtest = QCheck.Test.t
type ctest = unit -> unit

(* Generalizing pre and post conditions *)
type test = Pre of bool * test | Post of bool

let ( =>> ) pre post = Pre (pre, post)
let ( ->> ) pre post = Pre (pre, Post post)

(* Generalizing parameterization of tests *)

type 'a property = {
  name : string;
  q : 'a QCheck.arbitrary -> string -> qtest;
  c : 'a Crowbar.gen -> string -> ctest;
}

(* Functions for realizing preconditions *)
let rec qmake (t : test) : bool =
  match t with
  | Pre (pre, post) ->
      QCheck.assume pre;
      qmake post
  | Post post -> post

let rec cmake (t : test) : unit =
  match t with
  | Pre (pre, post) ->
      Crowbar.guard pre;
      cmake post
  | Post post -> Crowbar.check post

(* Helpers to build `'a property` types. Note that `'b` is the input to the property, INCLUDING the other parameters. *)
let qbuild (g : 'b QCheck.arbitrary) (p : 'b -> bool) : string -> qtest =
 fun name -> QCheck.Test.make ~name ~count:500000000 g p

let cbuild (g : ('b, unit) Crowbar.gens) (p : 'b) : string -> ctest =
 fun name () -> Crowbar.add_test ~name g p


