open Impl
(*no size cap*)
module QcheckType = struct
  let typebased =
    let open QCheck.Gen in
    let nfa_gen n =
      if n <= 0 then return ([], [], 0, [], [])
      else
       (*very list sizes*) 
        let gen_size = int_range 0 n in

        let gen_sigma = (*gen alphabet length s, generate char list of length s*) 
          gen_size >>= fun sigma_size ->
          list_size (return sigma_size) char
        in
        let gen_states = (*gen number of states n, generate state list of length n*)
          gen_size >>= fun states_size ->
          list_size (return states_size) nat
        in
        let gen_initial_state = (*gen initial state*)
          nat
        in
        let gen_final_states = (*gen number of states n, generate final state list of length n*)
          gen_size >>= fun final_states_size ->
          list_size (return final_states_size) nat
        in
        let gen_transition =
          nat >>= fun from ->
          nat >>= fun to_ ->
          frequency [
            (1, return (from, None, to_));
            (4, char >>= fun c -> return (from, Some c, to_))
          ]
         (*delta:generate a number x, then add a new transiton x times
          for each transition: generate 2 ints, 
          and 1/5 option = none 4/5 option = some char
          transition = (int1, option, int2); *)
        in
        let gen_transitions =
          gen_size >>= fun transitions_size ->
          list_size (return transitions_size) gen_transition
        in

        gen_sigma >>= fun sigma ->
        gen_states >>= fun qs ->
        gen_initial_state >>= fun q0 ->
        gen_final_states >>= fun fs ->
        gen_transitions >>= fun delta ->
        return (sigma, qs, q0, fs, delta)
    in
    sized (fun n -> nfa_gen n)

   let qcheck_type = QCheck.make typebased
end
