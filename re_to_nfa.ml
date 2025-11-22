
(* let symbol s c = match c with | s -> true | _ -> false *)

(* let union nfa1 nfa2 = failwith "union" *)
    (* let edge = ? in { *)
    (*     edge = edge; *)
    (*     initial = ; *)
    (*     final =  *)
(* let concat nfa1 nfa2 = failwith "concat" *)
(* let star nfa = failwith "star" *)

(* let escape s = match s with *)
(*     |  *)

(* let rec to_nfa re edges last_id = match re with *)
(*     | [] -> failwith "empty re" *)
(*     | [ c ] -> Hashtbl.replace edges (last_id, c) (last_id + 1); last_id + 1 *)
(*     | _ -> failwith "long list" *)
(*         (*     (match hd with *) *)
(*         (* | '\' -> failwith "escape" *) *)
(*         (* | c ->  *) *)
(**)
(* let construct_nfa re = *)
(*     let s = List.of_seq (String.to_seq re) in *)
(*     let edges = Hashtbl.create 100 in *)
(*     let last_id = to_nfa s edges 0 in *)
(*     last_id *)
(**)
(* type token = *)
(*     | Base of string *)
(*     | Group of token *)
(*     | Concat of token * token *)
(*     | Union of token * token *)
(*     | Star of token *)

type token =
    | Cha of char
    | Star
    | Bar
    | LParen
    | RParen
    | EOF

(* let peek s = String.sub s 0 1 *)

let escape c = Cha c


let rec tokenise re = 
    let process curr tl = (
        if Char.equal curr '(' then LParen :: (tokenise tl) else
        if Char.equal curr ')' then RParen :: (tokenise tl) else
        if Char.equal curr '|' then Bar :: (tokenise tl) else
        if Char.equal curr '*' then Star :: (tokenise tl) else
        (Cha curr) :: (tokenise tl)
    ) in match re with
        | [] -> [ EOF ]
        | [ curr ] -> process curr []
        | curr :: next :: tl -> (
            if Char.equal curr '\\' then (escape next) :: (tokenise tl) else
            process curr (next :: tl)
    )

let to_tokens re = tokenise (List.of_seq (String.to_seq re))

let print_token token = match token with
    | Cha c -> Printf.printf "Char %c\n" c
    | Bar -> Printf.printf "Bar\n"
    | Star -> Printf.printf "Start\n"
    | LParen -> Printf.printf "LParen\n"
    | RParen -> Printf.printf "RParen\n"
    | EOF -> Printf.printf "EOF\n"

type tree =
    | Base of char
    | Group of tree
    | Quant of tree
    | Union of tree * tree
    | Concat of tree * tree
    | Dummy

(* let pop l = match l with *)
(*     | [] -> (None, tl) *)
(*     | hd :: tl -> (Some hd, tl) *)

let rec parse_union l =
    (* let () = match l with *)
    (* (* | LParen :: _ -> Printf.printf "first character is (" *) *)
    (* | _ -> failwith "unexpected first char" in *)
    Printf.printf "calling parse_union with list of length %i\n" (List.length l);
    let left_tree, tl = parse_concat l in
    Printf.printf "received from parse_concat list of length %i\n" (List.length tl);
    match tl with
        | [] -> (left_tree, tl)
        | hd :: tl -> (match hd with
            | Bar -> let right_tree, tl = parse_concat tl in Union (left_tree, right_tree), tl
            | Cha _ | LParen | RParen | Star | EOF -> left_tree, hd :: tl
        )
and parse_concat l = 
    (* let () = match l with *)
    (* | LParen :: _ -> Printf.printf "first character is (" *)
    (* | _ -> failwith "unexpected first char" in *)
    Printf.printf "calling parse_concat with list of length %i\n" (List.length l);
    let left_tree, tl = parse_quant l in
    Printf.printf "received from parse_quant list of length %i\n" (List.length tl);
    match tl with
        | [] -> (left_tree, tl)
        | hd :: tl -> (match hd with
            | Cha _ | LParen | Star -> print_token hd; let right_tree, tl = parse_quant (hd :: tl) in Concat (left_tree, right_tree), tl
            | Bar | RParen | EOF -> left_tree, hd :: tl
        )
and parse_quant l =
    (* let () = match l with *)
    (* | LParen :: _ -> Printf.printf "first character is (" *)
    (* | _ -> failwith "unexpected first char" in *)
    Printf.printf "calling parse_quant with list of length %i\n" (List.length l);
    let tree, tl = parse_value l in
    Printf.printf "received from parse_value list of length %i\n" (List.length tl);
    match tl with
        | [] -> (tree, tl)
        | hd :: tl -> (match hd with
            | Star -> Quant tree, tl
            | Bar | LParen | Cha _ | RParen | EOF -> tree, hd :: tl
        )
and parse_value l =
    (* let () = match l with *)
    (* | LParen :: _ -> Printf.printf "first character is ("; *)
    (* | _ -> failwith "unexpected first char" in *)
    Printf.printf "calling parse_value with list of length %i\n" (List.length l);
    match l with
        | [] -> failwith "Unexpected missing token"
        | hd :: tl -> (match hd with
            | Cha c -> Base c, tl
            | LParen -> let tree, tl = parse_union tl in (match tl with | RParen :: tl -> Group tree, tl | _ -> failwith "Unmatched parentheses in expression")
            | Bar | RParen | Star | EOF -> Printf.printf "returning dummy with length %i\n" (List.length l); Dummy, tl
            (* | RParen -> failwith "Unexpected LParen" *)
            (* | Star -> failwith "Unexpected Star" *)
            (* | EOF -> failwith "Unexpected EOF" *)
        )

(*     | Cha c -> Concat (root, Base c) *)
(*     | RParen | LParen | Star -> parse_quant curr root tl *)
(*     | Bar -> failwith "unreachable" *)
(* and parse_quant curr root tl = match curr with *)
(*     | Star -> Quant (parse_value curr root tl) *)
(*     | _ -> failwith "idk2" *)
(* and parse_value curr root tl = match curr with *)
(*     | Cha c -> Base c *)
(*     | RParen -> treeise tl root *)
(*     | LParen -> failwith "idk3" *)
(*     | *)


(* let rec treeise tokens root = match tokens with *)
(*     | [] -> root *)
(*     | [ _ ] -> failwith "idk" *)
(*     | curr :: _ :: tl -> ( *)
(*         parse_union curr root tl *)
(*     ) *)
(* and parse_union curr root tl = match curr with *)
(*     | Bar -> Union (root, (treeise tl root)) *)
(*     | Cha _ | LParen | RParen | Star -> parse_concat curr root tl *)
(* and parse_concat curr root tl = match curr with *)
(*     | Cha c -> Concat (root, Base c) *)
(*     | RParen | LParen | Star -> parse_quant curr root tl *)
(*     | Bar -> failwith "unreachable" *)
(* and parse_quant curr root tl = match curr with *)
(*     | Star -> Quant (parse_value curr root tl) *)
(*     | _ -> failwith "idk2" *)
(* and parse_value curr root tl = match curr with *)
(*     | Cha c -> Base c *)
(*     | RParen -> treeise tl root *)
(*     | LParen -> failwith "idk3" *)
(*     | *)



    (*     match hd with *)
    (*     | Star -> Quant (treeise next) *)
    (*     | Or -> Union (left, right) *)
    (*     | Conca *)
    (*     | Char c -> Base c *)
    (*     |  *)
    (* ) *)


let to_tree tokens = parse_union tokens
    (* let out = ref [] *)
    (* let get i = String.sub re i 1 in *)
    (* for i = 0 to (String.length re) do *)
    (*     let curr = get i in *)
    (*     let next = get (i+1) in *)
    (*     if curr = '\' then ... else *)
    (*     out := (Char curr) :: !out; *)
    (* done *)

let rec print_tree tree = match tree with
    | Base c -> Printf.printf "%c\n" c
    | Quant t -> Printf.printf "--Quant--\n"; print_tree t
    | Group t -> Printf.printf "--Group--\n"; print_tree t
    | Dummy -> Printf.printf "DUMMY\n"
    | Concat (l, r) -> Printf.printf "--Concat--\n"; print_tree l; print_tree r
    | Union (l, r) -> Printf.printf "--Union--\n"; print_tree l; print_tree r

(**)
(* let rec tokenise re = *)
(*     let i = ref 0 in *)
(*     let j = ref 1 in *)
(*     let get k = String.sub re k 1 in *)
(*     1 *)
(* and parse_union left curr right = *)
(*     if curr = '|' then Union (left, parse_concat  *)
(*     else parse_concat left curr right *)
(* and parse_concat left curr right = *)
(*     if peek right = '*' then parse_quant left curr right else Concat(left, curr) *)
(**)

    (* parse_union re *)
    (* parens re *)
(*     (* Check for tokens in of precedence: Paren, Star, Concat, Union *) *)
(* and parens re = *)
(*     let lparen_idx = String.index_opt re '(' in *)
(*     let rparen_idx = String.rindex_opt re ')' in *)
(*     match lparen_idx, rparen_idx with *)
(*         | None, None ->   *)
(*         | Some l, Some r -> tokenise s (l+1) (r-1) *)
(*         | None, Some _ | Some _, None -> failwith "mismatched brackets" *)
(* and parse_union re = *)
(*     let or_idx = String.index_opt re '|' in *)
(*     match or_idx with *)
(*     | None -> parse_concat re *)
(*     | Some i -> Union (tokenise (String.sub re 0 i), tokenise (String.sub re (i+1) (String.length re))) *)
(* and parse_concat re = *)
(*     let non_and_idx = String.index_opt re '*' in *)
(*     match non_and_idx with *)
(*     | None -> parse_quant re *)
(*     | Some i -> Concat (parse_quant re, parse_quant re) *)
(* and parse_quant re = *)
(*     let quant_idx = String.index_opt re '*' in *)
(*     match quant_id with *)

let () =
    let _ = "ababababab" in
    let re = "(ab|a)*" in
    let tokens = to_tokens re in
    let tree, _ = to_tree tokens in
    print_tree tree
    (* let _ = construct_nfa tokens in () *)
    (* Nfa.run nfa input *)
