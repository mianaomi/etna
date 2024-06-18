open Impl

module BaseType : Base_quickcheck.Test.S with type fsm_t = fsm_t = struct
  type fsm_t = fsm_t [@@deriving sexp, quickcheck]
end
