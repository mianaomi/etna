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
        int_range 0 (n/(generate1 (int_range 1 n))) >>= fun sigma_size -> 
        unique_list char sigma_size
      in
      let gen_states = 
        int_range 1 (n/(generate1 (int_range 1 n))) >>= fun states_size -> (*UNIQUE int value 1<=x times*)
        unique_list nat states_size (*replace with 0 to states_size*)
      in
      let gen_initial_state qs = (*pull a value from qs*)
        oneofl qs
      in
      let gen_final_states qs = (*generate a number 1<=y<=x, then pull a NEW value from qs y times*)
        int_range 1 (List.length qs) >>= fun final_size -> 
        unique_list (oneofl qs) final_size  
      in
      let gen_transitions q0 qs sigma = 
        let make_transition from to_ = 
          frequency
          [
            (1, return (from, None, to_));
            (4, oneofl sigma >>= fun c -> return (from, Some c, to_));
          ]
        in
        let worklist = [q0] in
        let rec transloop state1 =
          if List.mem state1 worklist then
            () (* Exit condition if state1 is already in worklist *)
          else begin
            let new_worklist = state1 :: worklist in (* Add state1 to worklist *)
            let state2 =
              if List.exists (fun s -> s <> q0 && List.mem s worklist) qs then
                (* Pick a state2 not in worklist that has a direct line to q0 *)
                List.find (fun s -> s <> state1 && List.mem s worklist) qs
              else
                (* Pick any unconnected state2 *)
                List.find (fun s -> not (List.mem s new_worklist)) qs
            in
            transloop state2 (* Recursively process state2 *)
          end
        in
        List.iter transloop qs; (* Process each state in qs using transloop *)

        (* Generate random transitions for any remaining unconnected states *)
        let unconnected_states = List.filter (fun s -> not (List.mem s worklist)) qs in
        let rec generate_random_transitions = function
          | [] -> return []
          | state :: rest ->
              let random_state = oneofl worklist in
              make_transition state random_state >>= fun transition ->
              generate_random_transitions rest >>= fun transitions ->
              return (transition :: transitions)
        in
        generate_random_transitions unconnected_states >>= fun random_transitions ->

        return random_transitions
      in
        gen_sigma >>= fun sigma ->
        gen_states >>= fun qs ->
        gen_initial_state qs >>= fun q0 ->
        gen_final_states qs >>= fun fs ->
        gen_transitions q0 qs sigma>>= fun delta ->
        return (sigma, qs, q0, fs, delta)
  in
  sized (fun n -> nfa_gen n)

  let qcheck_bespoke = QCheck.make bespokegen
end

(*
function gen_transitions(q0, qs, sigma):
    // Function to create a transition between states 'from' and 'to'
    function make_transition(from, to):
        return randomly select:
            - (from, None, to) with probability 1/5
            - (from, Some(c), to) where c is a random character from sigma with probability 4/5

    // Initialize worklist with the initial state q0
    worklist = [q0]

    // Function to ensure connectivity from q0 to all states in qs
    function ensure_connectivity(state1):
        if state1 is not in worklist:
            add state1 to worklist
            if there exists state2 in qs such that state2 is not q0 and state2 is in worklist:
                find such state2 and ensure_connectivity(state2)
            else:
                find any unconnected state2 in qs and ensure_connectivity(state2)

    // Start ensuring connectivity from q0
    ensure_connectivity(q0)

    // Filter out states that are not connected to q0
    unconnected_states = filter states in qs where state not in worklist

    // Function to generate random transitions for unconnected states
    function generate_random_transitions(states):
        if states is empty:
            return []
        else:
            state = states[0]
            random_state = randomly select a state from worklist
            transition = make_transition(state, random_state)
            rest_transitions = generate_random_transitions(states[1:])
            return [transition] + rest_transitions

    // Generate random transitions for unconnected states
    random_transitions = generate_random_transitions(unconnected_states)

    // Return the complete set of transitions
    return random_transitions*)