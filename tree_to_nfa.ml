open Base 
open Nfa
open Re_to_nfa

let cha_nfa c last_id =
    let q0 = last_id + 1 in
    let f = [last_id + 2] in
    let edges = [(q0, c), f] in
    nfa_from_list edges f q0, last_id + 2

let quant_nfa nfa last_id = failwith "quant"

let concat_nfa nfa1 nfa2 last_id = failwith "concat"

let add_edge edges from to_ on =
    let res = Base.Hashtbl.add edges ~key:(from, on) ~data:[to_] in
    match res with
        | `Ok -> ()
        | `Duplicate -> failwith (Printf.sprintf "Unexpectedly added duplicate edge from %i to %i on %s" from to_ on)

let union_nfa nfa1 nfa2 last_id =
    (* let all_edges = Base.Hashtbl.merge nfa1.edges nfa2.edges in *)
    let all_edges = nfa1.edges in
    let new_initial = last_id + 1 in
    let new_final = [last_id + 2] in
    add_edge all_edges new_initial nfa1.initial "";
    add_edge all_edges new_initial nfa2.initial "";
    add_edge all_edges nfa1.final new_final "";
    add_edge all_edges nfa2.final new_final "";
    { edges = all_edges; initial = new_initial; final = new_final }, last_id + 2
    all_edges, last_id + 2
    

let rec to_nfa_ tree last_id = match tree with
    | Quant t -> quant_nfa (to_nfa t) last_id
    | Group t -> to_nfa t last_id
    | Concat (l, r) -> concat_nfa (to_nfa l) (to_nfa r) last_id
    | Union (l, r) -> union_nfa (to_nfa l) (to_nfa r) last_id
    | Base c -> cha_nfa c last_id


let to_nfa tree = to_nfa_ tree 0
