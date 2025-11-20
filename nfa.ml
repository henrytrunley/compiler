open Stdio

module IntSet = Set.Make(Base.Int)

type automata = {
    trans : IntSet.t -> char -> IntSet.t;
    accept : IntSet.t -> bool;
    mutable states : IntSet.t;
}

let delta = function
    | 1 -> (function | '0' -> IntSet.of_list[1] | '1' -> IntSet.of_list[2] | _ -> IntSet.empty )
    | 2 -> (function | '0' -> IntSet.of_list[3] | '1' -> IntSet.of_list[2] | _ -> IntSet.empty )
    | 3 -> (function | '0' -> IntSet.of_list[2] | '1' -> IntSet.of_list[2] | _ -> IntSet.empty )
    | _ -> (function | _ -> IntSet.empty)

let trans states c =
    IntSet.fold (fun state acc ->
        let new_states = delta state c in
        IntSet.union acc new_states
    ) states IntSet.empty

let f = function
    | 1 -> false
    | 2 -> true
    | 3 -> false
    | _ -> false

let accept states =
    IntSet.exists f states

let q0 = IntSet.of_list[1]

let run nfa input =
    String.iter (fun c ->
        nfa.states <- trans nfa.states c
    ) input;
    nfa.accept nfa.states

let () =
    let dfa = { trans ; accept ; states = q0 } in
    let res = run dfa "0100" in
    printf "%b\n" res
