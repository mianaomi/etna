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