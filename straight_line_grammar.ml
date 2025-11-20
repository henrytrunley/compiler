open Base
open Stdio

type id = string
type num = int
type binop =
    | Plus
    | Minus
    | Multiply
    | Divide

type stm =
    | CompoundStm of stm * stm
    | AssignStm of id * exp
    | PrintStm of exp list
and exp =
    | IdExp of id
    | NumExp of num
    | OpExp of exp * binop * exp
    | EseqExp of stm * exp

let get_id_exn id env =
    match List.Assoc.find env id ~equal:(String.(=)) with | None -> failwith "id not found" | Some x -> x

let assign id value env = List.Assoc.add env id value ~equal:(String.(=))

let rec run stm env = match stm with
    | CompoundStm (stm1, stm2) -> let env = run stm1 env in let env = run stm2 env in env
    | AssignStm (id, exp) -> let num, env = eval exp env in assign id num env
    | PrintStm exp_list -> let env = print_exps exp_list env in env
and eval exp (env: (id, num) List.Assoc.t) = match exp with
    | IdExp id -> (get_id_exn id env, env)
    | NumExp num -> num, env
    | OpExp (exp1, op, exp2) -> binop exp1 op exp2 env
    | EseqExp (stm, exp) -> let env = run stm env in eval exp env
and print_exps exps env =
    let to_str i = Printf.sprintf "%d" i in
    let rec update_env_and_concat exps (out, env) = (match exps with
        | [] -> (out ^ "\n", env)
        | [ exp ] -> let num, env = eval exp env in (out ^ (to_str num) ^ "\n", env)
        | exp :: tl ->
            let num, env = eval exp env in
            let out = (to_str num) ^ " " in
            update_env_and_concat tl (out, env)
    ) in
    let out, env = update_env_and_concat exps ("", env) in
    print_string out;
    env
and binop exp1 op exp2 env =
    let (num1, env) = eval exp1 env in
    let (num2, env) = eval exp2 env in
    let res = (match op with
        | Plus -> num1 + num2
        | Minus -> num1 - num2
        | Multiply -> num1 * num2
        | Divide -> num1 / num2
    ) in (res, env)

let interp prog = let _ = run prog [] in ()

let program =
    CompoundStm(
        AssignStm(
            "a",
            OpExp(
                NumExp 5,
                Plus,
                NumExp 3
            )
        ),
        CompoundStm(
            AssignStm(
                "b",
                EseqExp(
                    PrintStm([
                        IdExp "a";
                        OpExp(
                            IdExp "a",
                            Minus,
                            NumExp 1
                        );
                    ]),
                    OpExp(
                        NumExp(10),
                        Multiply,
                        IdExp "a"
                    )
                )
            ),
            PrintStm([
                IdExp "b";
            ])
        )
    )

let () = interp program;
