(* open Stdio *)
(* open Base *)

let parse_args () =
    let usage_msg = "ml_lex </path/to/input_file.lex> </path/to/output_file.ml" in
    let args = ref [] in
    let speclist = [] in
    let anon_fun arg = args := arg :: !args in
    Stdlib.Arg.parse speclist anon_fun usage_msg;
    (match !args with
        | input_file :: output_file :: [] -> (input_file, output_file)
        | _ -> failwith "This program takes two arguments as input"
    )

let read_file _ =
    failwith "read file"

let () =
    let input_file, _ = parse_args () in
    let _ = read_file input_file in
    ()
