open Spec
open Util
open Runner
open Crowbar

let ( << ) f g x = f (g x)

let test_prop_NothingPasses : string property = {
  name = "test_prop_NothingPasses";
  q = (fun a -> qbuild a (qmake << prop_NothingPasses));
  c = (fun g -> cbuild [g] (cmake << prop_NothingPasses));
  b = (fun m -> bbuild m (bmake << prop_NothingPasses));
}


let fuzz_prop_NothingPasses : fuzz_property = {
  pbt = test_prop_NothingPasses;
  afl = cmake << prop_NothingPasses;
}