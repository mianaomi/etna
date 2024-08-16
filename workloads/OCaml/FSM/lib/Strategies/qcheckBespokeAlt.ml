open Impl

module QcheckBespoke = struct
  let bespokegen = 
    let open QCheck.Gen in 

    let nfa_gen n =
      let unique_list gen size =
        let rec aux acc size =
          if size = 0 then return (List.rev acc)
          else gen >>= fun x ->
               if List.mem x acc then aux acc size
               else aux (x :: acc) (size - 1)
        in
        aux [] size
      in

      let gen_sigma = 
        int_range 1 (n/(generate1 (int_range 1 n))) >>= fun sigma_size -> 
        unique_list char sigma_size
      in

      let gen_states = 
        int_range 1 (n/(generate1 (int_range 1 n))) >>= fun states_size -> 
        unique_list nat states_size
      in

      let gen_initial_state qs = 
        oneofl qs
      in

      let gen_final_states qs = 
        int_range 1 (List.length qs) >>= fun final_size -> 
        unique_list (oneofl qs) final_size
      in
      (*Function to create a transition between states 'from' and 'to'*)
      let make_transition from to_ sigma = (*function make_transition(from, to):*)
        frequency [
          (1, return (from, None, to_));
          (4, oneofl sigma >>= fun c -> return (from, Some c, to_));
        ] (*return randomly select:
        - (from, None, to) with probability 1/5
        - (from, Some(c), to) where c is a random character from sigma with probability 4/5*)
      in

      let gen_transitionsALT q0 qs sigma = 
        (* Generate transitions starting from the initial state q0 *) 
        (* Ensure all states are reachable from q0 *)
        let rec necessary_conns queue transitions =
          match queue with
          | [] -> return transitions  (* All states processed, return the transitions *)
          | current :: rest ->
            let rec add_connections state transitions visited =
              if List.mem state visited then return (visited, transitions)
              else
                oneofl qs >>= fun next_state ->
                make_transition state next_state sigma >>= fun transition ->
                let new_transitions = transition :: transitions in
                let new_visited = state :: visited in
                add_connections next_state new_transitions new_visited
            in
            add_connections current transitions [] >>= fun (visited, new_transitions) ->
            let new_queue = List.filter (fun s -> not (List.mem s visited)) qs @ rest in
            necessary_conns new_queue new_transitions
        in

        (* Start the process from the initial state q0 *)
        necessary_conns [q0] [] >>= fun necessary_transitions ->
        
        (*add additional random transitions*)
        let rec additional_conns counter acc =
          if counter = 0 then return acc
          else
            oneofl qs >>= fun state_a ->
            oneofl qs >>= fun state_b ->
            make_transition state_a state_b sigma >>= fun transition ->
            additional_conns (counter - 1) (transition :: acc)
        in
        
        let additional_transitions_count = n - List.length qs in
        additional_conns additional_transitions_count necessary_transitions
      in

      gen_sigma >>= fun sigma ->
      gen_states >>= fun qs ->
      gen_initial_state qs >>= fun q0 ->
      gen_final_states qs >>= fun fs ->
      gen_transitionsALT q0 qs sigma >>= fun delta ->
      return (sigma, qs, q0, fs, delta)
    in

    sized (fun n -> nfa_gen n)

  let qcheck_bespoke = QCheck.make bespokegen
end

   

