type pos = int
type t =
    | Eof of pos * pos
    | If of pos * pos
    | Id of string * pos * pos
    | Num of int * pos * pos
    | Real of float * pos * pos
