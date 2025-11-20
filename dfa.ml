open Stdio
open Base

type q =
    | Dead
    | Q1
    | Q2
    | Q3

type ('q, 'sigma) automata = {
    delta : 'q -> char -> 'q;
    final : 'q -> bool;
    mutable state : 'q;
}

let delta = function
    | Q1 -> (function | '0' -> Q1 | '1' -> Q2 | _ -> Dead)
    | Q2 -> (function | '0' -> Q3 | '1' -> Q2 | _ -> Dead)
    | Q3 -> (function | '0' -> Q2 | '1' -> Q2 | _ -> Dead)
    | Dead -> (function | _ -> Dead)

let f = function
    | Q1 -> false
    | Q2 -> true
    | Q3 -> false
    | Dead -> false

let q0 = Q1

let run dfa input =
    String.iter input ~f:(fun c ->
        dfa.state <- delta dfa.state c
    );
    f dfa.state

let () =
    let dfa = { delta ; final = f; state = q0} in
    let res = run dfa "01000" in
    printf "%b\n" res
