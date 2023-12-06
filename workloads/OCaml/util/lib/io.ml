open Runner
open QCheck
open Crowbar

let rec lookup l k =
  match l with
  | [] -> None
  | (k', v) :: l' -> if k = k' then Some v else lookup l' k

let qmain t ts s ss file =
  let tst = lookup ts t in
  let arb = lookup ss s in
  match (tst, arb) with
  | None, _ -> Printf.printf "Test %s not found\n" t
  | _, None -> Printf.printf "Strategy %s not found\n" s
  | Some tst, Some arb ->
      let oc =
        open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 file
      in
      let _ = Printf.fprintf oc "[%s|\n" t in
      let st = Sys.time () in
      let _ = qrun tst arb oc in
      let dt = (Sys.time () -. st) *. 1000.0 in
      Printf.fprintf oc "|%s -> %.2f]\n" t dt;
      close_out oc

let cmain t ts s ss =
  let tst = lookup ts t in
  let gen = lookup ss s in
  match (tst, gen) with
  | None, _ -> Printf.printf "Test %s not found\n" t
  | _, None -> Printf.printf "Strategy %s not found\n" s
  | Some tst, Some gen ->
      Printf.printf "[%f start]\n" (Unix.gettimeofday ());
      crun tst gen;
      at_exit (fun () -> Printf.printf "[%f end]\n" (Unix.gettimeofday ()));
      ()

let load_env () =
  let get = Unix.getenv in
  (get "framework", get "test", get "strategy", get "filename")

let make_env framework test strategy filename =
  [|
    "framework=" ^ framework;
    "test=" ^ test;
    "strategy=" ^ strategy;
    "filename=" ^ filename;
  |]

let crowbar_child tests strats =
  let _framework, test, strat, _filename = load_env () in
  cmain test tests strat strats

let crowbar_fork framework test strat filename =
  let oc =
    open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 filename
  in
  let od = Unix.descr_of_out_channel oc in
  let cur = Sys.executable_name in
  match
    Unix.create_process_env cur
      [| cur; "--repeat=100000000" |]
      (* pass the # of tests as a command line argument to the child process *)
      (make_env framework test strat filename)
      Unix.stdin od Unix.stderr
  with
  | 0 -> () (* child thread *)
  | pid -> (
      (* parent thread *)
      let _, status = Unix.waitpid [] pid in
      match status with
      | Unix.WEXITED c ->
          Printf.fprintf oc "[%f exit %i]\n" (Unix.gettimeofday ()) c
      | _ -> Printf.fprintf oc "[%f exit unsafely]\n" (Unix.gettimeofday ()))

(* Call format:
   dune exec <workload> -- <framework> <testname> <strategy> <filename>
   for example,
   dune exec BST -- qcheck prop_InsertValid bespokeGenerator out.txt
   or
   dune exec BST -- crowbar prop_InsertPost typeBasedCrowbar out2.txt
*)
let main (props : (string * 'a property) list)
    (qstrats : (string * 'a arbitrary) list) (cstrats : (string * 'a gen) list)
    : unit =
  if Array.length Sys.argv < 5 then crowbar_child props cstrats
  else
    let framework = Sys.argv.(1) in
    let testname = Sys.argv.(2) in
    let strategy = Sys.argv.(3) in
    let filename = Sys.argv.(4) in
    Printf.printf
      "Executing test %s into file %s using strategy %s on framework %s\n"
      testname filename strategy framework;
    match framework with
    | "qcheck" ->
        Printf.printf "Valid framework QCheck\n";
        qmain testname props strategy qstrats filename
    | "crowbar" ->
        Printf.printf "Valid framework Crowbar\n";
        crowbar_fork framework testname strategy filename
    | _ -> Printf.printf "Framework %s was not found\n" framework
