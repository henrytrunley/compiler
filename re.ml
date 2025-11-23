
let re s =
    let tokens = Re_parser.to_tokens s in
    let tree = Re_parser.to_tree tokens in
    let nfa = Tree_to_nfa.to_nfa tree in
    Nfa.run nfa

let () =
    (* let re = "(ab|a)*" in *)
    let matches_re = re "(ab)*|b*" in
    let res = matches_re "ababababab" in
    Printf.printf "%b\n" res
