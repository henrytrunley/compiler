open Stdio
open Base

(* let re = Re.re *)

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

let find_and_replace_all string map =
    List.fold map ~init:string ~f:(
        fun string (pattern, with_) -> String.substr_replace_all string ~pattern:pattern ~with_:with_
    )
    

let process_aliases aliases =
    let lines = String.split aliases ~on:'\n' in
    List.fold lines ~init:[] ~f:(
        fun map line ->
            let processed_line = find_and_replace_all line map in
            match String.lsplit2 processed_line ~on:'=' with
            | None -> map
            | Some (pattern, with_) -> ("{" ^ pattern ^ "}", with_) :: map
    )

let process_map map aliases = 
    let processed_map = find_and_replace_all map aliases in
    printf "%s" processed_map


let generate_lexer input_file =
    let contents = In_channel.read_all input_file in
    let parts = Str.split (Str.regexp "%%") contents in
    match parts with
        | _ :: a :: m :: [] -> let aliases = process_aliases a in let map = process_map m aliases in map
        | _ -> failwith "Incorrectly formatted input file. 3 parts were expected, separated by %%."

let () =
    let input_file, _ = parse_args () in
    generate_lexer input_file
