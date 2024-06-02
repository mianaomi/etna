let rarely_false (s: string) =
  match s with
  (*! *)
  | _ -> true
  (*!! rare_1_a *)
  (*!
    | "a" -> false
    | _ -> true *)
  (*!! rare_1_b *)
  (*!
    | "b" -> false
    | _ -> true *)
  (*!! rare_1_c *)
  (*!
    | "c" -> false
    | _ -> true *)
  (*!! rare_1_d *)
  (*!
    | "d" -> false
    | _ -> true *)
  (*!! rare_1_e *)
  (*!
    | "e" -> false
    | _ -> true *)

  (*!! rare_2_a *)
  (*!
    | "ab" -> false
    | _ -> true *)
  (*!! rare_2_b *)
  (*!
    | "cd" -> false
    | _ -> true *)
  (*!! rare_2_c *)
  (*!
    | "ef" -> false
    | _ -> true *)
  (*!! rare_2_d *)
  (*!
    | "gh" -> false
    | _ -> true *)
  (*!! rare_2_e *)
  (*!
    | "xy" -> false
    | _ -> true *)

  (*!! rare_3_a *)
  (*!
    | "xyz" -> false
    | _ -> true *)
  (*!! rare_3_b *)
  (*!
    | "abc" -> false
    | _ -> true *)
  (*!! rare_3_c *)
  (*!
    | "def" -> false
    | _ -> true *)
  (*!! rare_3_d *)
  (*!
    | "123" -> false
    | _ -> true *)
  (*!! rare_3_e *)
  (*!
    | "*@#" -> false
    | _ -> true *)

  (*!! rare_4_a *)
  (*!
    | "wxyz" -> false
    | _ -> true *)
  (*!! rare_4_b *)
  (*!
    | "abcd" -> false
    | _ -> true *)
  (*!! rare_4_c *)
  (*!
    | "efgh" -> false
    | _ -> true *)
  (*!! rare_4_d *)
  (*!
    | "1234" -> false
    | _ -> true *)
  (*!! rare_4_e *)
  (*!
    | "*@#)" -> false
    | _ -> true *)

  (*!! rare_5_a *)
  (*!
    | "vwxyz" -> false
    | _ -> true *)
  (*!! rare_5_b *)
  (*!
    | "abcde" -> false
    | _ -> true *)
  (*!! rare_5_c *)
  (*!
    | "lmnop" -> false
    | _ -> true *)
  (*!! rare_5_d *)
  (*!
    | "12345" -> false
    | _ -> true *)
  (*!! rare_5_e *)
  (*!
    | "*@#)(" -> false
    | _ -> true *)

