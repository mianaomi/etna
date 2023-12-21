open Impl
open Spec
open Util.Runner
open QCheck
open Crowbar

let ( << ) f g x = f (g x)
let qi = small_int
let ci = int8

let test_prop_InsertValid : rbt property =
  {
    name = "test_prop_InsertValid";
    q = (fun a -> qbuild (triple a qi qi) (qmake << prop_InsertValid));
    c =
      (fun g ->
        cbuild [ g; ci; ci ] (fun t k v -> prop_InsertValid (t, k, v) |> cmake));
  }

(*! QCheck test_prop_InsertValid. *)

let test_prop_DeleteValid : rbt property =
  {
    name = "test_prop_DeleteValid";
    q = (fun a -> qbuild (QCheck.pair a qi) (qmake << prop_DeleteValid));
    c =
      (fun g -> cbuild [ g; ci ] (fun t k -> prop_DeleteValid (t, k) |> cmake));
  }

(*! QCheck test_prop_DeleteValid. *)

let test_prop_InsertPost : rbt property =
  {
    name = "test_prop_InsertPost";
    q = (fun a -> qbuild (quad a qi qi qi) (qmake << prop_InsertPost));
    c =
      (fun g ->
        cbuild [ g; ci; ci; ci ] (fun t k k' v ->
            prop_InsertPost (t, k, k', v) |> cmake));
  }

(*! QCheck test_prop_InsertPost. *)

let test_prop_DeletePost : rbt property =
  {
    name = "test_prop_DeletePost";
    q = (fun a -> qbuild (triple a qi qi) (qmake << prop_DeletePost));
    c =
      (fun g ->
        cbuild [ g; ci; ci ] (fun t k k' -> prop_DeletePost (t, k, k') |> cmake));
  }

(*! QCheck test_prop_DeletePost. *)

let test_prop_InsertModel : rbt property =
  {
    name = "test_prop_InsertModel";
    q = (fun a -> qbuild (triple a qi qi) (qmake << prop_InsertModel));
    c =
      (fun g ->
        cbuild [ g; ci; ci ] (fun t k v -> prop_InsertModel (t, k, v) |> cmake));
  }

(*! QCheck test_prop_InsertModel. *)

let test_prop_DeleteModel : rbt property =
  {
    name = "test_prop_DeleteModel";
    q = (fun a -> qbuild (QCheck.pair a qi) (qmake << prop_DeleteModel));
    c =
      (fun g -> cbuild [ g; ci ] (fun t k -> prop_DeleteModel (t, k) |> cmake));
  }

(*! QCheck test_prop_DeleteModel. *)

let test_prop_InsertInsert : rbt property =
  {
    name = "test_prop_InsertInsert";
    q =
      (fun a ->
        qbuild
          (QCheck.pair a (quad qi qi qi qi))
          ( qmake << fun (t, (k, k', v, v')) ->
            prop_InsertInsert (t, k, k', v, v') ));
    c =
      (fun g ->
        cbuild [ g; ci; ci; ci; ci ] (fun t k k' v v' ->
            prop_InsertInsert (t, k, k', v, v') |> cmake));
  }

(*! QCheck test_prop_InsertInsert. *)

let test_prop_InsertDelete : rbt property =
  {
    name = "test_prop_InsertDelete";
    q = (fun a -> qbuild (quad a qi qi qi) (qmake << prop_InsertDelete));
    c =
      (fun g ->
        cbuild [ g; ci; ci; ci ] (fun t k k' v ->
            prop_InsertDelete (t, k, k', v) |> cmake));
  }

(*! QCheck test_prop_InsertDelete. *)

let test_prop_DeleteInsert : rbt property =
  {
    name = "test_prop_DeleteInsert";
    q = (fun a -> qbuild (quad a qi qi qi) (qmake << prop_DeleteInsert));
    c =
      (fun g ->
        cbuild [ g; ci; ci; ci ] (fun t k k' v' ->
            prop_DeleteInsert (t, k, k', v') |> cmake));
  }

(*! QCheck test_prop_DeleteInsert. *)

let test_prop_DeleteDelete : rbt property =
  {
    name = "test_prop_DeleteDelete";
    q = (fun a -> qbuild (triple a qi qi) (qmake << prop_DeleteDelete));
    c =
      (fun g ->
        cbuild [ g; ci; ci ] (fun t k k' ->
            prop_DeleteDelete (t, k, k') |> cmake));
  }

(*! QCheck test_prop_DeleteDelete. *)
