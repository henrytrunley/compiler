open Base

type t = {
    edges : (int * string, int list) Hashtbl.t;
    initial : int;
    final : int list;
}

val nfa_from_list : ((int * string) * int list) list -> int list -> int -> t
