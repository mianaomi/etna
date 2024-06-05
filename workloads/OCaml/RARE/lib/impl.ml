let rarely_false (s: string) =
  match Array.to_list (Array.init (String.length s) (String.get s)) with
  (*! *)
  | _ -> true
  (*!! rare_1 *)
  (*!
    | ['a'] -> false
    | _ -> true *)

  (*!! rare_2 *)
  (*! *)
    (* | ['h'; 'i'] -> false
    | _ -> true *)

  (*!! rare_3 *)
  (*!
    | ['h'; 'e'; 'y'] -> false
    | _ -> true *)

  (*!! rare_4 *)
  (*!
    | ['f'; 'u'; 'z'; 'z'] -> false
    | _ -> true *)

  (*!! rare_5 *)
  (*!
    | ['f'; 'u'; 'z'; 'Z'; 'y'] -> false
    | _ -> true *)

  (*!! rare_6 *)
  (*!
    | ['n'; 'i'; 'k'; 'h'; 'i'; 'l'] -> false
    | _ -> true *)

  (*!! rare_7 *)
  (*!
    | ['a'; 'f'; 'l'; 'f'; 'u'; 'z'; 'z'] -> false
    | _ -> true *)

  (*!! rare_8 *)
  (*!
    | ['p'; 'r'; 'o'; 'p'; 'e'; 'r'; 't'; 'y'] -> false
    | _ -> true *)

  (*!! rare_9 *)
  (*!
    | ['f'; 'u'; 'z'; 'z'; 'i'; 'n'; 'g'; '!'; '!'] -> false
    | _ -> true *)

  (*!! rare_10 *)
  (*!
    | ['s'; 'e'; 'c'; 'r'; 'e'; 't'; ' '; 'k'; 'e'; 'y'] -> false
    | _ -> true *)

  (*!! rare_11 *)
  (*!
    | ['h'; 'e'; 'l'; 'l'; 'o'; ' '; 'w'; 'o'; 'r'; 'l'; 'd'] -> false
    | _ -> true *)

