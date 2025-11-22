type nfa = {
    edges : (int * string, int list) Hashtbl.t;
    initial : int;
    final : int list;
}

val construct : ((int * string) * int list) list -> int list -> int -> nfa
