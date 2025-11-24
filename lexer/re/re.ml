let re s =
    let tokens = Re_parser.to_tokens s in
    let tree = Re_parser.to_tree tokens in
    let nfa = Tree_to_nfa.to_nfa tree in
    Nfa.run nfa
