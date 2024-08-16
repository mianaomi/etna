open Impl
  
module BaseBespoke : Base_quickcheck.Test.S with type t = (Nat.t, Char.t) fsm_t = struct
  type t = (Nat.t, Char.t) fsm_t [@@deriving sexp, quickcheck]

  let unique_list gen size =
    let open Base_quickcheck.Generator in
    let rec aux acc size =
      if size = 0 then return (List.rev acc)
      else gen >>= fun x ->
           if List.mem acc x ~equal:(=) then aux acc size
           else aux (x :: acc) (size - 1)
    in
    aux [] size

  let make_transition from to_ sigma =
    let open Base_quickcheck.Generator in
    frequency [
      (1, return (from, None, to_));
      (4, oneofl sigma >>= fun c -> return (from, Some c, to_));
    ]

  let gen_transitions q0 qs sigma =
    let open Base_quickcheck.Generator in
    let rec necessary_conns worklist transitions flag =
      match List.find qs ~f:(fun s -> not (List.mem worklist s ~equal:(=)) && s <> q0) with
      | None -> return (worklist, transitions)
      | Some state1 ->
        let rec trace_back state1 worklist transitions flag =
          let worklist = state1 :: worklist in
          let state2 =
            if not flag then
              List.find_exn qs ~f:(fun s -> not (List.mem worklist s ~equal:(=)))
            else
              List.find_exn qs ~f:(fun s -> s <> state1)
          in
          make_transition state2 state1 sigma >>= fun transition ->
          let transitions = transition :: transitions in
          if state2 = q0 || (flag && List.mem worklist state2 ~equal:(=)) then
            return (worklist, transitions)
          else
            trace_back state2 worklist transitions (state2 = q0 || flag)
        in
        trace_back state1 worklist transitions flag >>= fun (new_worklist, new_transitions) ->
        necessary_conns new_worklist new_transitions (flag || List.mem new_worklist q0 ~equal:(=))
    in
    necessary_conns [q0] [] false >>= fun (worklist, necessary_transitions) ->
    let rec additional_conns counter acc =
      if counter = 0 then return acc
      else
        oneofl qs >>= fun state_a ->
        oneofl qs >>= fun state_b ->
        make_transition state_a state_b sigma >>= fun transition ->
        additional_conns (counter - 1) (transition :: acc)
    in
    let additional_transitions_count = List.length qs in
    additional_conns additional_transitions_count necessary_transitions

  let nfa_generator =
    let open Base_quickcheck.Generator in

    let gen_sigma n =
      int_inclusive 1 (n / 2) >>= fun sigma_size ->
      unique_list Char.quickcheck_generator sigma_size
    in

    let gen_states n =
      int_inclusive 1 (n / 2) >>= fun states_size ->
      unique_list Nat.quickcheck_generator states_size
    in

    let gen_initial_state states =
      oneofl states
    in

    let gen_final_states states =
      int_inclusive 1 (List.length states) >>= fun final_size ->
      unique_list (oneofl states) final_size
    in

    int >>= fun n ->
    gen_sigma n >>= fun sigma ->
    gen_states n >>= fun states ->
    gen_initial_state states >>= fun initial_state ->
    gen_final_states states >>= fun final_states ->
    gen_transitions initial_state states sigma >>= fun transitions ->
    return { sigma; qs = states; q0 = initial_state; fs = final_states; delta = transitions }

  let quickcheck_generator = nfa_generator
end
  

