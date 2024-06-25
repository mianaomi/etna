open Impl

module BaseType : Base_quickcheck.Test.S with type t = int fsm_t = struct
  type t = int fsm_t [@@deriving sexp, quickcheck]
end
