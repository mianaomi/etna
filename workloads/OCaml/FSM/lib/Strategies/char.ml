module Char = struct
  include Core.Char

  type t = Core.Char.t [@@deriving sexp, quickcheck]
  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    char >>= fun c -> return c
end
