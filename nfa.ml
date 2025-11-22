open Base
open Stdio

(* module Node_symbol = struct *)
(*     module T = struct *)
(*         type t = int * string *)
(**)
(*         let compare (node1, symbol1) (node2, symbol2) = *)
(*             let node_cmp = Int.compare node1 node2 in *)
(*             if node_cmp <> 0 then node_cmp else *)
(*             String.compare symbol1 symbol2 *)
(**)
(*         let sexp_of_t (node, symbol) : Sexp.t = *)
(*             List [ Atom (Int.to_string node) ; Atom symbol ] *)
(*     end *)
(*     include T *)
(*     include Comparator.Make(T) *)
(* end *)
module Node_symbol = struct
    type t = int * string
    [@@deriving compare, sexp_of, hash]
end

type t = {
    edges : (int * string, int list) Hashtbl.t;
    initial : int;
    final : int list;
}

let next_states edges state symbol =
    match Hashtbl.find edges (state, symbol) with
        | None -> Set.empty (module Int)
        | Some new_states -> Set.of_list (module Int) new_states

let expand edges states symbol =
    let empty_set = Set.empty (module Int) in
    Set.fold states ~f:(fun acc state ->
        Set.union acc (next_states edges state symbol)
    ) ~init:empty_set

let rec closure edges states =
    let t = Set.union states (expand edges states "") in
    if Set.equal t states then t else closure edges t

let dfa_edge edges states symbol =
    let new_states = expand edges states symbol in
    closure edges new_states

let accept final states =
    not (Set.are_disjoint final states)

let list_to_hashtbl alist = 
    match Hashtbl.of_alist (module Node_symbol) alist with
    | `Ok tbl -> tbl
    | `Duplicate_key _ -> failwith "Couln't generate hashtable with duplicate key: %i"

let nfa_from_list d f q0 = {
    edges = list_to_hashtbl d;
    final = f;
    initial = q0
}

let run nfa input =
    let state = ref (closure nfa.edges (Set.of_list (module Int) [nfa.initial])) in
    String.iter ~f:(fun c ->
        state := dfa_edge nfa.edges !state (String.make 1 c)
    ) input;
    accept (Set.of_list (module Int) nfa.final) !state

(* User Inputs *)
let edges = [
    ((1, "0"), [1]);
    ((1, ""), [2]);
    ((1, "1"), [2]);
    ((2, "0"), [3]);
    ((2, "1"), [2]);
    ((3, "0"), [2]);
    ((3, "1"), [2]);
]
let f = [2]
let q0 = 1

let () =
    let nfa = nfa_from_list edges f q0 in
    let res = run nfa "0100000000000000000000000001010101010110100" in
    printf "%b\n" res
