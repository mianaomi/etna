open Impl

module QcheckBespoke = struct
  let bespokegen = 
    let open QCheck.Gen in 
    
    let nfa_gen n =
      let unique_list gen size = (*unique constraint*)
        let rec aux acc size =
          if size = 0 then return (List.rev acc)
          else gen >>= fun x ->
               if List.mem x acc then aux acc size
               else aux (x :: acc) (size - 1)
        in
        aux [] size
      in

      let gen_sigma = (*generate a number a, then add a new UNIQUE char a times*)
        int_range 0 n >>= fun sigma_size -> 
        unique_list char sigma_size
      in
      let gen_states = 
        int_range 1 n >>= fun states_size -> (*UNIQUE int value 1<=x times*)
        unique_list nat states_size
      in
      let gen_initial_state qs = (*pull a value from qs*)
        oneofl qs
      in
      let gen_final_states qs = (*generate a number 1<=y<=x, then pull a NEW value from qs y times*)
        int_range 1 (List.length qs) >>= fun final_size -> 
        unique_list (oneofl qs) final_size  
      in
      let gen_transitions qs sigma = (* ??? *)
        0
      in
        gen_sigma >>= fun sigma ->
        gen_states >>= fun qs ->
        gen_initial_state qs >>= fun q0 ->
        gen_final_states qs >>= fun fs ->
        gen_transitions qs sigma>>= fun delta ->
        return (sigma, qs, q0, fs, delta)
  in
  sized (fun n -> nfa_gen n)

  let qcheck_bespoke = QCheck.make bespokegen
end