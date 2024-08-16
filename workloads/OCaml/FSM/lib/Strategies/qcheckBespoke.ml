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

      let gen_transitions q0 qs sigma = 
        (*Start by creating the necessary transitions for the FSM to be "valid"-- 
        every state can be reached from q0. The problem arises in the edge case where we have 
        only one state: q0, which acts as our only final state.*)
        let rec necessary_conns worklist transitions flag =
          match List.find_opt (fun s -> not (List.mem s worklist) && s <> q0) qs with (*take a state1 not in the worklist and not q0*)
          | None -> return (worklist, transitions)
          | Some state1 ->
            let rec trace_back state1 worklist transitions flag = (*recursive function traces_back(state1):*)
              let worklist = state1 :: worklist in (*add state1 to worklist*)
              let state2 =
                if not flag then
                  List.find (fun s -> not (List.mem s worklist)) qs
                else
                  List.find (fun s -> s <> state1) qs
              (*if q0 is NOT in the worklist yet:
                pick a state2 not in the worklist
              else:
                pick a state2 that isn't state1*)
              in
              (*call make_transitions(state2, state1) // Meaning I draw backwards*)
              make_transition state2 state1 sigma >>= fun transition -> 
              let transitions = transition :: transitions in (*if state2 is not in the worklist yet, add it (union function)*)
              if state2 = q0 || (flag && List.mem state2 worklist) then
                return (worklist, transitions)
              else
                trace_back state2 worklist transitions (state2 = q0 || flag)
             (*if state2 = q0 OR ((q0 IS in the worklist) AND (state2 IS in the worklist)):
              breaking case: end recursive loop
             else:
              call traces_first state2*)
            in
            trace_back state1 worklist transitions flag >>= fun (new_worklist, new_transitions) ->
            necessary_conns new_worklist new_transitions (flag || List.mem q0 new_worklist)
            
        in
        (*Initialize empty worklist*)
        necessary_conns [q0] [] false >>= fun (worklist, necessary_transitions) ->
        (*Now for the fun stuff: add the rest of the transitions *)
        let rec additional_conns counter acc =
          if counter = 0 then return acc
          else
            oneofl qs >>= fun state_a ->
            oneofl qs >>= fun state_b ->
            make_transition state_a state_b sigma >>= fun transition ->
            additional_conns (counter - 1) (transition :: acc)
          (*function additional_conns (counter)
          if counter = size variable n - qs.length
            end case: return 
          else
            pull 2 random states from qs
            call make_transitions(state_a, state_b)*)
        in
        
        let additional_transitions_count = n - List.length qs in
        additional_conns additional_transitions_count necessary_transitions
      in

      gen_sigma >>= fun sigma ->
      gen_states >>= fun qs ->
      gen_initial_state qs >>= fun q0 ->
      gen_final_states qs >>= fun fs ->
      gen_transitions q0 qs sigma >>= fun delta ->
      return (sigma, qs, q0, fs, delta)
    in

    sized (fun n -> nfa_gen n)

  let qcheck_bespoke = QCheck.make bespokegen
end

