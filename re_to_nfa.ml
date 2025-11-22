type token =
    | Cha of char
    | Star
    | Bar
    | LParen
    | RParen
    | EOF

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
    | Base of string
    | Group of tree
    | Quant of tree
    | Union of tree * tree
    | Concat of tree * tree

let rec parse_union l =
    let left_tree, tl = parse_concat l in
    match tl with
        | [] -> (left_tree, tl)
        | hd :: tl -> (match hd with
            | Bar -> let right_tree, tl = parse_concat tl in Union (left_tree, right_tree), tl
            | Cha _ | LParen | RParen | Star | EOF -> left_tree, hd :: tl
        )
and parse_concat l = 
    let left_tree, tl = parse_quant l in
    match tl with
        | [] -> (left_tree, tl)
        | hd :: tl -> (match hd with
            | Cha _ | LParen | Star -> let right_tree, tl = parse_quant (hd :: tl) in Concat (left_tree, right_tree), tl
            | Bar | RParen | EOF -> left_tree, hd :: tl
        )
and parse_quant l =
    let tree, tl = parse_value l in
    match tl with
        | [] -> (tree, tl)
        | hd :: tl -> (match hd with
            | Star -> Quant tree, tl
            | Bar | LParen | Cha _ | RParen | EOF -> tree, hd :: tl
        )
and parse_value l =
    match l with
        | [] -> failwith "Unexpected missing token"
        | hd :: tl -> (match hd with
            | Cha c -> Base (String.make 1 c), tl
            | LParen -> let tree, tl = parse_union tl in (match tl with | RParen :: tl -> Group tree, tl | _ -> failwith "Unmatched parentheses in expression")
            | Bar -> failwith "Unexpected Bar"
            | RParen -> failwith "Unexpected LParen"
            | Star -> failwith "Unexpected Star"
            | EOF -> failwith "Unexpected EOF"
        )

let to_tree tokens =
    let tree, leftovers = parse_union tokens in
    assert ((List.length leftovers) = 1); (* EOF remains unprocessed *)
    tree

let rec print_tree tree = match tree with
    | Base c -> Printf.printf "%s\n" c
    | Quant t -> Printf.printf "--Quant--\n"; print_tree t
    | Group t -> Printf.printf "--Group--\n"; print_tree t
    | Concat (l, r) -> Printf.printf "--Concat--\n"; print_tree l; print_tree r
    | Union (l, r) -> Printf.printf "--Union--\n"; print_tree l; print_tree r

let () =
    let _ = "ababababab" in
    let re = "(ab|a)*" in
    let tokens = to_tokens re in
    let () = List.iter print_token tokens in
    let tree = to_tree tokens in
    print_tree tree
