open Base

type t = {
    edges : (int * string, (int, (Int.comparator_witness)) Set.t) Hashtbl.t;
    initial : int;
    final : (int, (Int.comparator_witness)) Set.t;
}

val nfa_from_lists : ((int * string) * int list) list -> int list -> int -> t

val run : t -> string -> bool
