open Stdio

module IntSet = Set.Make(Base.Int)

type nfa = {
    dfa_edge : IntSet.t -> string -> IntSet.t;
    initial : IntSet.t;
    final : IntSet.t;
}

let expand edge states c =
    IntSet.fold (fun state acc ->
        IntSet.union acc (IntSet.of_list (edge state c))
    ) states IntSet.empty

let rec closure edge states =
    let t = IntSet.union states (expand edge states "") in
    if IntSet.equal t states then t else closure edge t

let dfa_edge edge states c =
    let new_states = expand edge states c in
    closure edge new_states

let accept f states =
    not (IntSet.disjoint f states)

let construct d f q0 = {
    dfa_edge = dfa_edge d;
    final = IntSet.of_list f;
    initial = closure d (IntSet.of_list q0)
}

let run nfa input =
    let state = ref nfa.initial in
    String.iter (fun c ->
        state := nfa.dfa_edge !state (String.make 1 c)
    ) input;
    accept nfa.final !state

(* User Inputs *)
let edge s c = match s with
    | 1 -> (match c with | "0" -> [1] | "1" -> [2] | _ -> [] )
    | 2 -> (match c with | "0" -> [3] | "1" -> [2] | _ -> [] )
    | 3 -> (match c with | "0" -> [2] | "1" -> [2] | _ -> [] )
    | _ -> (match c with | _ -> [])
let f = [2]
let q0 = [1]

let () =
    let nfa = construct edge f q0 in
    let res = run nfa "010000000000000000000000000101010101011010" in
    printf "%b\n" res
