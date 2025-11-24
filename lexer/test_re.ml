let () =
    (* let re = "(ab|a)*" in *)
    let matches_re = Re.re "a((ab)*|b*)" in
    let res = matches_re "aababababab" in
    Printf.printf "%b\n" res
