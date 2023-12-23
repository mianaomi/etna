open Impl
open Spec
open Util.Runner
open Crowbar

let ( << ) f g x = f (g x)

let test_prop_SinglePreserve : expr property =
  {
    name = "test_prop_SinglePreserve";
    q = (fun a -> qbuild a (qmake << prop_SinglePreserve));
    c = (fun g -> cbuild [ g ] (cmake << prop_SinglePreserve));
  }

let test_prop_MultiPreserve : expr property =
  {
    name = "test_prop_MultiPreserve";
    q = (fun a -> qbuild a (qmake << prop_MultiPreserve));
    c = (fun g -> cbuild [ g ] (cmake << prop_MultiPreserve));
  }
