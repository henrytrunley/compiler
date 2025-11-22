type node
type nfa = {
    edge : node -> string -> node list;
    initial : node list;
    final : node list;
}

val construct : (node -> string -> node list) -> node list -> node list -> nfa
