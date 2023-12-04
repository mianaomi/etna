open Runner
open QCheck
open Crowbar

let rec lookup l k =
  match l with
  | [] -> None
  | (k', v) :: l' -> if k = k' then Some v else lookup l' k

(* Runs f, redirecting outputs to file *)
let redirect (file : string) (f : unit -> unit) : unit =
  let oldout = Unix.dup Unix.stdout in
  let newout =
    open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 file
  in
  Unix.dup2 (Unix.descr_of_out_channel newout) Unix.stdout;
  f ();
  flush stdout;
  Unix.dup2 oldout Unix.stdout

let qmain t ts s ss =
  let tst = lookup ts t in
  let arb = lookup ss s in
  match (tst, arb) with
  | None, _ -> Printf.printf "Test %s not found\n" t
  | _, None -> Printf.printf "Strategy %s not found\n" s
  | Some tst, Some arb ->
      let _ = Printf.printf "[%s|\n" t in
      let st = Sys.time () in
      let _ = qrun tst arb in
      let dt = (Sys.time () -. st) *. 1000.0 in
      Printf.printf "|%s -> %.2f]\n" t dt

let cmain t ts s ss =
  let tst = lookup ts t in
  let gen = lookup ss s in
  match (tst, gen) with
  | None, _ -> Printf.printf "Test %s not found\n" t
  | _, None -> Printf.printf "Strategy %s not found\n" s
  | Some tst, Some gen ->
      let _ = Printf.printf "[%s|\n" t in
      let st = Sys.time () in
      let _ = crun tst gen in
      let dt = (Sys.time () -. st) *. 1000.0 in
      Printf.printf "|%s -> %.2f]\n" t dt

(* Call format:
   dune exec <workload> -- <framework> <testname> <strategy> <filename>
   for example,
   dune exec BST -- qcheck prop_InsertValid bespokeGenerator out.txt
*)
let main (props : (string * 'a property) list)
    (qstrats : (string * 'a arbitrary) list) (cstrats : (string * 'a gen) list)
    : unit =
  if Array.length Sys.argv < 5 then
    Printf.printf "Not enough arguments were provided to `dune exec` (%i) \n" (Array.length Sys.argv)
  else
    let framework = Sys.argv.(1) in
    let testname = Sys.argv.(2) in
    let strategy = Sys.argv.(3) in
    let filename = Sys.argv.(4) in
    let _ =
      Printf.printf
        "Executing test %s into file %s using strategy %s on framework %s"
        testname filename strategy framework
    in
    match framework with
    | "qcheck" ->
        redirect filename (fun () -> qmain testname props strategy qstrats)
    | "crowbar" ->
        redirect filename (fun () -> cmain testname props strategy cstrats)
    | _ -> Printf.printf "Framework %s was not found" framework
