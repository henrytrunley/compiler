open Stdio

module IntSet = Set.Make(Base.Int)

type nfa = {
    trans : IntSet.t -> char -> IntSet.t;
    accepts : IntSet.t -> bool;
    start : IntSet.t;
}

let trans delta states c =
    IntSet.fold (fun state acc ->
        let new_states = IntSet.of_list (delta state c) in
        IntSet.union acc new_states
    ) states IntSet.empty

let accept f states =
    IntSet.exists f states

let construct d f q0 = {
    trans = trans d;
    accepts = accept f;
    start = IntSet.of_list q0
}

let d = function
    | 1 -> (function | '0' -> [1] | '1' -> [2] | _ -> [] )
    | 2 -> (function | '0' -> [3] | '1' -> [2] | _ -> [] )
    | 3 -> (function | '0' -> [2] | '1' -> [2] | _ -> [] )
    | _ -> (function | _ -> [])

let f = function
    | 1 -> false
    | 2 -> true
    | 3 -> false
    | _ -> false

let q0 = [1]


let run nfa input =
    let state = ref nfa.start in
    String.iter (fun c ->
        state := nfa.trans !state c
    ) input;
    nfa.accepts !state

let () =
    let nfa = construct d f q0 in
    let res = run nfa "0100000000000000000000000001010101010110100" in
    printf "%b\n" res
