open Base 
open Nfa
open Re_to_nfa

let cha_nfa c last_id =
    let q0 = last_id + 1 in
    let f = [last_id + 2] in
    let edges = [(q0, c), f] in
    nfa_from_lists edges f q0, last_id + 2

let quant_nfa nfa last_id = failwith "quant"

let concat_nfa nfa1 nfa2 last_id = failwith "concat"

let make_set node = Set.of_list (module Int) [node]

let add_edge edges from to_ on =
    let res = Base.Hashtbl.add edges ~key:(from, on) ~data:(make_set to_) in
    match res with
        | `Ok -> ()
        | `Duplicate -> failwith (Printf.sprintf "Unexpectedly added duplicate edge from %i on %s" from on)

let union_nfa nfa1 nfa2 last_id =
    let all_edges = Base.Hashtbl.merge nfa1.edges nfa2.edges ~f:(
        fun ~key:_ d -> match d with
            | `Left x -> Some x
            | `Right x -> Some x
            | `Both (x, y) -> Some (Set.union x y)
    ) in
    let new_initial = last_id + 1 in
    let new_final = last_id + 2 in
    add_edge all_edges new_initial nfa1.initial "";
    add_edge all_edges new_initial nfa2.initial "";
    let all_final = Set.union nfa1.final nfa2.final in
    Set.iter all_final ~f:(fun final -> add_edge all_edges final new_final "");
    { edges = all_edges; initial = new_initial; final = make_set new_final }, new_final
    

let rec to_nfa_ tree last_id = match tree with
    | Quant t -> quant_nfa (to_nfa_ t) last_id
    | Group t -> to_nfa_ t last_id
    | Concat (l, r) -> concat_nfa (to_nfa_ l last_id) (to_nfa_ r last_id) last_id
    | Union (l, r) -> let l_nfa, last_id = to_nfa_ l last_id in let r_nfa, last_id = to_nfa_ r last_id in union_nfa l_nfa r_nfa last_id
    | Base c -> cha_nfa c last_id


let to_nfa tree = to_nfa_ tree 0
