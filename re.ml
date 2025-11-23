

let () =
    let re = "(ab|a)*" in
    let tokens = Re_parser.to_tokens re in
    let () = List.iter Re_parser.print_token tokens in
    let tree = Re_parser.to_tree tokens in
    let () = Re_parser.print_tree tree in
    let nfa = Tree_to_nfa.to_nfa tree in
    let input_str = "ababababab" in
    let res = Nfa.run nfa input_str in
    Printf.printf "%b\n" res
