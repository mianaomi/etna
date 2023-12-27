open STLC.QcheckType
open STLC.QcheckBespoke
open STLC.CrowbarType
open STLC.CrowbarBespoke
open STLC.Impl
open STLC.Test
open Util.Io
open Util.Runner
open QCheck

(* RUNNER COMMAND:
  dune exec STLC -- qcheck prop_SinglePreserve bespoke out.txt
  dune exec STLC -- qcheck prop_SinglePreserve type out.txt
  dune exec STLC -- crowbar prop_SinglePreserve bespoke out.txt
  dune exec STLC -- crowbar prop_SinglePreserve type out.txt
  dune exec STLC -- afl prop_SinglePreserve bespoke out.txt
  dune exec STLC -- afl prop_SinglePreserve type out.txt
*)


let properties : (string * expr property) list =
  [
    ("prop_SinglePreserve", test_prop_SinglePreserve);
    ("prop_MultiPreserve", test_prop_MultiPreserve);
  ]

let qstrategies : (string * expr arbitrary) list =
  [ ("type", qcheck_type); ("bespoke", qcheck_bespoke) ]

let cstrategies = [
  ("type", crowbar_type); ("bespoke", crowbar_bespoke)
]
let () = main properties qstrategies cstrategies


