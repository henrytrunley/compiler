type token =
    | Cha of char
    | Star
    | Bar
    | LParen
    | RParen
    | EOF

val to_tokens : string -> token list

type tree =
    | Base of string
    | Group of tree
    | Quant of tree
    | Union of tree * tree
    | Concat of tree * tree

val to_tree : token list -> tree

val print_tree : tree -> unit
