
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

val interp : stm -> unit
