open Stdio

module IntSet = Set.Make(Base.Int)

type node = int
type nfa = {
    edge : node -> string -> node list;
    initial : node;
    final : node list;
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
    edge = d;
    final = f;
    initial = q0
}

let run nfa input =
    let state = ref (closure nfa.edge (IntSet.of_list [nfa.initial])) in
    String.iter (fun c ->
        state := dfa_edge nfa.edge !state (String.make 1 c)
    ) input;
    accept (IntSet.of_list nfa.final) !state

(* User Inputs *)
let edge s c = match s with
    | 1 -> (match c with | "0" -> [1] | "1" -> [2] | _ -> [] )
    | 2 -> (match c with | "0" -> [3] | "1" -> [2] | _ -> [] )
    | 3 -> (match c with | "0" -> [2] | "1" -> [2] | _ -> [] )
    | _ -> (match c with | _ -> [])
let f = [2]
let q0 = 1

let () =
    let nfa = construct edge f q0 in
    let res = run nfa "0100000000000000000000000001010101010110100" in
    printf "%b\n" res
