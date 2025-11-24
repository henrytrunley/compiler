open Base 
open Nfa
open Re_parser

let make_set node = Set.of_list (module Int) [node]

let combine_edges edges1 edges2 = Base.Hashtbl.merge edges1 edges2 ~f:(
        fun ~key:_ d -> match d with
            | `Left x -> Some x
            | `Right x -> Some x
            | `Both (x, y) -> Some (Set.union x y)
    )

let add_edge edges from to_ on =
    let old_to = Base.Hashtbl.find_or_add edges (from, on) ~default:(fun () -> Set.empty (module Int)) in
    let new_to = Set.add old_to to_ in
    Base.Hashtbl.remove edges (from, on);
    let res = Base.Hashtbl.add edges ~key:(from, on) ~data:new_to in
    match res with
        | `Ok -> ()
        | `Duplicate -> failwith (Printf.sprintf "Unexpectedly added duplicate edge from %i to %i on %s" from to_ on)

let cha_nfa c last_id =
    let q0 = last_id + 1 in
    let f = [last_id + 2] in
    let edges = [(q0, c), f] in
    nfa_from_lists edges f q0, last_id + 2

let quant_nfa nfa last_id =
    let new_initial = last_id + 1 in
    let new_final = last_id + 2 in
    add_edge nfa.edges new_initial nfa.initial "";
    add_edge nfa.edges new_initial new_final "";
    Set.iter nfa.final ~f:(fun old_final -> add_edge nfa.edges old_final nfa.initial ""; add_edge nfa.edges old_final new_final "");
    { edges = nfa.edges ; initial = new_initial; final = make_set new_final }, new_final

let concat_nfa nfa1 nfa2 =
    let new_edges = combine_edges nfa1.edges nfa2.edges in
    Set.iter nfa1.final ~f:(fun node -> add_edge new_edges node nfa2.initial "");
    { edges = new_edges; initial = nfa1.initial; final = nfa2.final }

let union_nfa nfa1 nfa2 last_id =
    let new_initial = last_id + 1 in
    let new_final = last_id + 2 in
    let new_edges = combine_edges nfa1.edges nfa2.edges in
    add_edge new_edges new_initial nfa1.initial "";
    add_edge new_edges new_initial nfa2.initial "";
    let all_final = Set.union nfa1.final nfa2.final in
    Set.iter all_final ~f:(fun final -> add_edge new_edges final new_final "");
    { edges = new_edges; initial = new_initial; final = make_set new_final }, new_final
    

let rec to_nfa_ tree last_id = match tree with
    | Quant t -> let nfa, last_id = to_nfa_ t last_id in quant_nfa nfa last_id
    | Group t -> to_nfa_ t last_id
    | Concat (l, r) -> let l_nfa, last_id = to_nfa_ l last_id in let r_nfa, last_id = to_nfa_ r last_id in concat_nfa l_nfa r_nfa, last_id
    | Union (l, r) -> let l_nfa, last_id = to_nfa_ l last_id in let r_nfa, last_id = to_nfa_ r last_id in union_nfa l_nfa r_nfa last_id
    | Base c -> cha_nfa c last_id

let to_nfa tree = let (nfa, _) = to_nfa_ tree 0 in nfa
