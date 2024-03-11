open QCheck
open Crowbar
open Quickchick_ocaml
open Util.Runner
open Util.Io
open BST.Impl
open BST.Test
open BST.QcheckType
open BST.QcheckBespoke
open BST.CrowbarType
open BST.CrowbarBespoke
open BST.BaseType
open BST.BaseBespoke

(*
  dune exec BST -- qcheck prop_InsertInsert bespoke out
  dune exec BST -- qcheck prop_InsertInsert type out
  dune exec BST -- crowbar prop_InsertInsert bespoke out
  dune exec BST -- crowbar prop_InsertInsert type out
  dune exec BST -- afl prop_InsertInsert bespoke out
  dune exec BST -- afl prop_InsertInsert type out
  dune exec BST -- base prop_InsertInsert bespoke out
  dune exec BST -- base prop_InsertInsert type out
  *)

let properties : (string * tree property) list =
  [
    ("prop_InsertValid", test_prop_InsertValid);
    ("prop_DeleteValid", test_prop_DeleteValid);
    ("prop_UnionValid", test_prop_UnionValid);
    ("prop_InsertPost", test_prop_InsertPost);
    ("prop_DeletePost", test_prop_DeletePost);
    ("prop_UnionPost", test_prop_UnionPost);
    ("prop_InsertModel", test_prop_InsertModel);
    ("prop_DeleteModel", test_prop_DeleteModel);
    ("prop_UnionModel", test_prop_UnionModel);
    ("prop_InsertInsert", test_prop_InsertInsert);
    ("prop_InsertDelete", test_prop_InsertDelete);
    ("prop_InsertUnion", test_prop_InsertUnion);
    ("prop_DeleteInsert", test_prop_DeleteInsert);
    ("prop_DeleteDelete", test_prop_DeleteDelete);
    ("prop_DeleteUnion", test_prop_DeleteUnion);
    ("prop_UnionDeleteInsert", test_prop_UnionDeleteInsert);
    ("prop_UnionUnionIdem", test_prop_UnionUnionIdem);
    ("prop_UnionUnionAssoc", test_prop_UnionUnionAssoc);
  ]

let qstrategies : (string * tree arbitrary) list =
  [ ("type", qcheck_type); ("bespoke", qcheck_bespoke) ]

let cstrategies : (string * tree gen) list =
  [ ("type", crowbar_type); ("bespoke", crowbar_bespoke) ]

let bstrategies : (string * tree basegen) list =
  [ ("type", (module BaseType)); ("bespoke", (module BaseBespoke)) ]

(* let () = main properties qstrategies cstrategies bstrategies *)

let () =
  let setup = Quickchick_ocaml.Required.({
    extractions = [
      Ty {type_name = "tree"; package = "Impl"; constructors = ["E"; "T"]};
      Func {function_name = "prop_InsertPost2"; package = "Impl"};
      File "/Users/nikhil/Code/Research/etna/workloads/OCaml/BST/lib/impl.ml";
      File "/Users/nikhil/Code/Research/etna/workloads/OCaml/BST/lib/Strategies/nat.ml";
    ];
    libraries = [ "qcheck"; "crowbar"; "util"; "core"];
    ml_file = "/Users/nikhil/Code/Research/etna/workloads/OCaml/BST/lib/impl.ml";
    temp_dir = "/Users/nikhil/Code/Research/etna/workloads/OCaml/BST/temporary";
    target = "prop_InsertPost2";
  }) in
  Quickchick_ocaml.Driver.quickchick_arg setup