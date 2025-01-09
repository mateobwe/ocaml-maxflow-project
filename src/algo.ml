open Graph
open Tools

type flot = {
  capacite : int;
  flot_act : int;
};;

let arc_valid arc =
  if arc.lbl > 0 then Some arc.tgt else None;;

let rec trans_liste liste aux =
  match liste with
  | [] -> aux
  | None :: rest -> trans_liste rest aux
  | Some x :: rest -> trans_liste rest (x :: aux);;

let find_path2 (grDC : id graph) (id1 : id) (id2 : id) =
  let rec aux current_path visited = function
    | [] -> None
    | id :: rest ->
        if List.mem id visited then aux current_path visited rest
        else if id = id2 then Some (List.rev (id :: current_path))
        else
          let out = out_arcs grDC id in
          let next_nodes = List.map arc_valid out in
          match aux (id :: current_path) (id :: visited) (trans_liste next_nodes rest) with
          | None -> aux current_path visited rest
          | some_path -> some_path
  in
  aux [id1] [] [id1];;

let find_arc_in_path gr id1 id2 =
  try
    Some (List.find (fun arc -> arc.tgt = id2) (out_arcs gr id1))
  with
  | Not_found -> None;;

let rec update_path2 (gr : id graph) (chemin : id list) valeur =
  match chemin with
  | [] | [_] -> gr
  | x :: y :: rest -> (
      match find_arc_in_path gr x y with
      | None -> assert false
      | Some arc ->
          let new_gr = add_arc gr arc.src arc.tgt (arc.lbl - valeur) in
          update_path2 new_gr (y :: rest) valeur);;

let create_graphe_ecart (gr : id graph) =
  let new_gr = clone_nodes gr in
  e_fold gr
    (fun g arc ->
      let g' = new_arc g { src = arc.tgt; tgt = arc.src; lbl = 0 } in
      new_arc g' { src = arc.src; tgt = arc.tgt; lbl = arc.lbl })
    new_gr;;

let rec find_max_possible graph chemin aux =
  match chemin with
  | [] | [_] -> aux
  | x :: y :: rest -> (
      match find_arc_in_path graph x y with
      | None -> aux
      | Some arc ->
          if arc.lbl < aux then find_max_possible graph (y :: rest) arc.lbl
          else find_max_possible graph (y :: rest) aux);;

let ford_fulkerson (graph : id graph) dep fin =
  let residual_graph = create_graphe_ecart graph in
  let rec ff_aux current_graph =
    match find_path2 current_graph dep fin with
    | None -> current_graph
    | Some chemin ->
        let max = find_max_possible current_graph chemin max_int in
        Printf.printf "Valeur max actuelle: %d%!\n" max;
        if max = 0 then current_graph
        else
          let new_graph = update_path2 current_graph chemin (-max) in
          let new_graph2 = update_path2 new_graph (List.rev chemin) max in
          ff_aux new_graph2
  in ff_aux residual_graph;;




(* Implémentation de Ford-Fulkerson pour trouver le flot maximum. 
let rec ford_fulkerson (graph : id graph) dep fin =
  match find_path2 graph dep fin with
  | None -> graph
  | Some chemin ->
      let max = find_max_possible graph chemin max_int in
      if max = 0 then graph
      else
        let new_graph = update_path2 graph chemin (-max) in
        let new_graph2 = update_path2 new_graph (List.rev chemin) max in
        ford_fulkerson new_graph2 dep fin;; *)
