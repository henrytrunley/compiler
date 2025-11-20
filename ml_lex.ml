open Stdio
open Base

let parse_args () =
    let usage_msg = "ml_lex </path/to/input_file.lex> </path/to/output_file.ml" in
    let args = ref [] in
    let speclist = [] in
    let anon_fun arg = args := arg :: !args in
    Stdlib.Arg.parse speclist anon_fun usage_msg;
    (match !args with
        | output_file :: input_file :: [] -> (input_file, output_file)
        | _ -> failwith "This program takes two arguments as input"
    )

let generate_lexer input_file =
    let contents = In_channel.read_all input_file in
    let parts = Str.split (Str.regexp "%%") contents in
    List.iter parts ~f:(fun s -> printf "\n\nPART:\n%s" s)

let () =
    let input_file, _ = parse_args () in
    generate_lexer input_file
