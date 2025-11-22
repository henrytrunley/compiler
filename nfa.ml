open Stdio

module IntSet = Set.Make(Base.Int)

type nfa = {
    edges : (int * string, int list) Hashtbl.t;
    initial : int;
    final : int list;
}

let next_states edges state symbol =
    match Hashtbl.find_opt edges (state, symbol) with
        | None -> IntSet.empty
        | Some new_states -> IntSet.of_list new_states

let expand edges states symbol =
    IntSet.fold (fun state acc ->
        IntSet.union acc (next_states edges state symbol)
    ) states IntSet.empty

let rec closure edges states =
    let t = IntSet.union states (expand edges states "") in
    if IntSet.equal t states then t else closure edges t

let dfa_edge edges states symbol =
    let new_states = expand edges states symbol in
    closure edges new_states

let accept final states =
    not (IntSet.disjoint final states)

let list_to_hashtbl alist = Hashtbl.of_seq (List.to_seq alist)

let construct d f q0 = {
    edges = list_to_hashtbl d;
    final = f;
    initial = q0
}

let run nfa input =
    let state = ref (closure nfa.edges (IntSet.of_list [nfa.initial])) in
    String.iter (fun c ->
        state := dfa_edge nfa.edges !state (String.make 1 c)
    ) input;
    accept (IntSet.of_list nfa.final) !state

(* User Inputs *)
let edges = [
    ((1, "0"), [1]);
    ((1, "1"), [2]);
    ((2, "0"), [3]);
    ((2, "1"), [2]);
    ((3, "0"), [2]);
    ((3, "1"), [2]);
]
let f = [2]
let q0 = 1

let () =
    let nfa = construct edges f q0 in
    let res = run nfa "0100000000000000000000000001010101010110100" in
    printf "%b\n" res
