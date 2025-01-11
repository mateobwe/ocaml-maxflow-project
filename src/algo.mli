open Graph

type flot = {
  capacite : int ;
  flot_act : int; 
}


val update_path2: int graph -> int list -> int -> int graph 
val create_graphe_ecart: int graph -> int graph
val find_max_possible: int graph -> int list -> int -> int 
val ford_fulkerson: int graph -> int -> int -> int graph
val convert_graph: int graph -> int graph -> string graph

