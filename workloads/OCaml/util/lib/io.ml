open Runner
open QCheck
open Crowbar
open Parse

(* global timeout for test threads (only crowbar is supported using this for now) *)
let timeout = 60

let qmain t ts s ss oc =
  let tst = lookup ts t in
  let arb = lookup ss s in
  match (tst, arb) with
  | None, _ -> Printf.printf "Test %s not found\n" t
  | _, None -> Printf.printf "Strategy %s not found\n" s
  | Some tst, Some arb ->
      Printf.fprintf oc "[%f start]\n" (Unix.gettimeofday ());
      flush oc;
      qrun tst arb oc

let cmain t ts s ss =
  let tst = lookup ts t in
  let gen = lookup ss s in
  match (tst, gen) with
  | None, _ -> Printf.printf "Test %s not found\n" t
  | _, None -> Printf.printf "Strategy %s not found\n" s
  | Some tst, Some gen ->
      Printf.printf "[%f start]\n" (Unix.gettimeofday ());
      flush stdout;
      crun tst gen

let fmain t ts s ss =
  let tst = lookup ts t in
  let gen = lookup ss s in
  match (tst, gen) with
  | None, _ -> Printf.printf "Test %s not found\n" t
  | _, None -> Printf.printf "Strategy %s not found\n" s
  | Some tst, Some gen -> crun tst gen

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

let afl_child tests strats =
  let _framework, test, strat, _filename = load_env () in
  fmain test tests strat strats

let crowbar_fork framework test strat filename =
  let oc =
    open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 filename
  in
  let od = Unix.descr_of_out_channel oc in
  let cur = Sys.executable_name in
  Random.self_init ();
  let seed = string_of_int (Random.bits ()) in
  match
    Unix.create_process_env cur
      [| cur; "--repeat=100000000"; "--seed=" ^ seed |]
      (* pass the # of tests as a command line argument to the child process *)
      (make_env framework test strat filename)
      Unix.stdin od Unix.stderr
  with
  | 0 -> () (* runner/child thread *)
  | pid -> (
      match Unix.fork () with
      | 0 ->
          (* timeout thread *)
          Unix.sleep timeout;
          Unix.kill pid Sys.sigalrm
      | pid' -> (
          (* waiting thread *)
          let _, status = Unix.waitpid [] pid in
          Unix.kill pid' Sys.sigterm;
          let endtime = Unix.gettimeofday () in
          match status with
          | Unix.WEXITED c -> Printf.fprintf oc "[%f exit %i]\n" endtime c
          | Unix.WSIGNALED c when c = Sys.sigalrm ->
              Printf.fprintf oc "[%f exit timeout]\n" endtime
          | _ -> Printf.fprintf oc "[%f exit unexpected]\n" endtime))

let afl_fork framework test strat filename : unit =
  Sys.command "mkdir -p input && echo asdf > input/nikhil" |> ignore;
  let channels =
    Unix.open_process_args_full "afl-fuzz"
      [|
        "afl-fuzz";
        "-i";
        "input";
        "-o";
        "output";
        "-V";
        string_of_int timeout;
        Sys.executable_name;
        "@@";
      |]
      (Array.append
         (make_env framework test strat "-")
         [| "AFL_BENCH_UNTIL_CRASH=true" |])
  in
  let _status = Unix.close_process_full channels in
  (* TODO: parse the fuzzing stats into the same format as the rest of the runs so we don't have to rewrite a parser. *)
  let now = Unix.gettimeofday () in
  let get = parse "output/default/fuzzer_stats" in
  match (get "execs_done", get "execs_per_sec", get "saved_crashes") with
  | Some runs, Some rps, Some found when found >= 0.5 ->
      let oc =
        open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 filename
      in
      Printf.fprintf oc "[%f start]\n" now;
      Printf.fprintf oc "[%f exit 0]\n" (now +. (runs /. rps));
      close_out oc
  | Some runs, Some rps, Some _ ->
      let oc =
        open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 filename
      in
      Printf.fprintf oc "[%f start]\n" now;
      Printf.fprintf oc "[%f exit timeout]\n" (now +. (runs /. rps));
      close_out oc
  | _ -> failwith "Error attempting to read AFL output file"

let qcheck_fork t ts s ss f =
  let oc = open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 f in
  match Unix.fork () with
  (* runner/child thread *)
  | 0 -> qmain t ts s ss oc
  | pid -> (
      match Unix.fork () with
      | 0 ->
          (* timeout thread *)
          Unix.sleep timeout;
          Unix.kill pid Sys.sigalrm
      | pid' -> (
          (* waiting thread *)
          let _, status = Unix.waitpid [] pid in
          Unix.kill pid' Sys.sigterm;
          let endtime = Unix.gettimeofday () in
          match status with
          | Unix.WEXITED c -> Printf.fprintf oc "[%f exit %i]\n" endtime c
          | Unix.WSIGNALED c when c = Sys.sigalrm ->
              Printf.fprintf oc "[%f exit timeout]\n" endtime
          | _ -> Printf.fprintf oc "[%f exit unexpected]\n" endtime))

(* Call format:
   dune exec <workload> -- <framework> <testname> <strategy> <filename>
   for example,
   dune exec BST -- qcheck prop_InsertValid bespokeGenerator out.txt
   or
   dune exec BST -- crowbar prop_InsertPost crowbarType out2.txt
*)
let main (props : (string * 'a property) list)
    (qstrats : (string * 'a arbitrary) list) (cstrats : (string * 'a gen) list)
    : unit =
  if Array.length Sys.argv < 5 then
    match Unix.getenv "framework" with
    | "crowbar" -> crowbar_child props cstrats
    | "afl" -> afl_child props cstrats
    | _ ->
        Printf.printf
          "Not enough arguments were passed. Could not determine whether this \
           was a child process."
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
        qcheck_fork testname props strategy qstrats filename
    | "crowbar" ->
        Printf.printf "Valid framework Crowbar\n";
        crowbar_fork framework testname strategy filename
    | "afl" ->
        Printf.printf "Valid framework AFL\n";
        afl_fork framework testname strategy filename
    | _ -> Printf.printf "Framework %s was not found\n" framework
