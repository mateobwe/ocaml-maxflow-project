open Graph

type flot = {
  capacite : int ;
  flot_act : int; 
}


val find_path: 'a graph -> int -> int -> int list option
val init_flot_graph: int graph -> flot graph 
val update_arc: flot arc -> int list -> int -> flot arc
val update_path: flot graph -> int list -> int -> flot graph 
val create_graphe_ecart: flot graph -> int graph
val find_max_possible: 'a graph -> id list -> 'a -> 'a 