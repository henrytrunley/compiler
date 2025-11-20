(* open Stdio *)
(* open Base *)

let usage_msg = "ml_lex </path/to/input_file.lex> </path/to/output_file.ml"
let args = ref []
let speclist = []
let anon_fun arg = args := arg :: !args

let read_file _ =
    failwith "read file"

let () =
    Stdlib.Arg.parse speclist anon_fun usage_msg;
    let input_file = (match !args with
        | input_file :: _ :: [] -> input_file
        | _ -> failwith "This program takes two arguments as input"
    ) in
    let _ = read_file input_file in
    ()
