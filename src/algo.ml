open Tools
open Graph

type flot = {
capacite : int ;
flot_act : int;
};;


(* On vérifie si l'arc est valide (Label >= 0) *)
let arc_valid arc =
  assert (arc.lbl >= 0);
  if (arc.lbl != 0) then (Some arc.tgt) else None ;;

(* On supprime les "None" de l'option list et on récupère la valeur x des "Some x" *)
let rec trans_liste liste aux = match liste with
  | [] -> aux
  | None::rest -> trans_liste rest aux
  | (Some(x))::rest -> trans_liste rest (x::aux);;

(* On cherche un chemin d'un noeud à un autre *)
let rec find_path graph visited id1 id2 =
  if id1 = id2 then Some [id1]
  else if List.mem id1 visited then None
  else
    let rec aux = function
      | [] -> None
      | node::rest ->
        match find_path graph (id1::visited) node id2 with
        | None -> aux rest
        | Some p -> Some (id1::p) in
    aux (trans_liste (List.map arc_valid (out_arcs graph id1))[]);; 

(* On vérifie s'il y a bien un arc entre les deux noeuds qu'on traite *)
let find_arc_in_path gr id1 id2 =
  try
    Some (List.find (fun arc -> arc.tgt = id2) (out_arcs gr id1))
  with
  | Not_found -> None;;

(* On met en forme l'arc pour avoir "flot actuel/capacité" dans le label *)
let transfo_arc grff g arc=
  let val1= find_arc grff arc.src arc.tgt in
  match val1 with
  |Some arc1 ->  if (arc1.lbl > arc.lbl ) then new_arc g {src = arc.src ; tgt= arc.tgt; lbl= ("0/" ^ string_of_int (arc.lbl))}
  else new_arc g {src = arc.src ; tgt= arc.tgt; lbl= (string_of_int (arc.lbl-arc1.lbl) ^ "/" ^ string_of_int (arc.lbl))}
  | _ -> assert false

(* On convertit tous les labels du graphe en String grâce à transfo_arc*)
let convert_graph (grff: int graph ) (grb: int graph) =
  let new_gr = clone_nodes grb in
  e_fold grb (fun g arc -> (transfo_arc grff g arc)) new_gr ;;

(* On met à jour le chemin trouvé avec la valeur donnée *)
let rec update_path2 (gr:id graph) (chemin: id list) valeur =
   match chemin with
  |[] -> gr
  |_::[]-> gr
  | x::y::rest ->  match find_arc_in_path gr x y with
    | None -> gr
    | Some(arc) -> update_path2 (add_arc gr arc.src arc.tgt valeur) (y::rest) valeur;;

(* On crée un graphe d'écart à partir du graphe qu'on a construit depuis le .txt *)
let create_graphe_ecart (gr:int graph) =
  let new_gr = clone_nodes gr in
  e_fold gr (fun g arc ->
    let g = new_arc g {src = arc.src; tgt = arc.tgt; lbl = arc.lbl} in
    if find_arc_in_path gr arc.tgt arc.src = None then
      new_arc g {src = arc.tgt; tgt = arc.src; lbl = 0}
    else g
  ) new_gr;;

(* On cherche la valeur qu'on va pouvoir incrémenter sur le chemin qu'on a trouvé *)
let rec find_max_possible graph chemin aux =
  match chemin with
  |[] -> aux
  |_::[]-> aux
  |x::y::rest -> match find_arc_in_path graph x y with
    | None -> aux
    | Some arc  ->  if (arc.lbl < aux && arc.lbl >= 0) then find_max_possible graph (y::rest) (arc.lbl)
    else  find_max_possible graph (y::rest) aux 
;;

(* On applique l'algorithme de Ford-Fulkerson sur notre graphe d'écart*)
let rec ford_fulkerson (graph: int graph) dep fin =
  match find_path graph [] dep fin with
  |None -> graph
  |Some chemin ->
    let max =  find_max_possible graph chemin max_int
  in match max with
  | 0 -> assert false 
  | _ -> let new_graph = update_path2 graph chemin (-max) in
          let new_graph2 =  update_path2 new_graph (List.rev chemin) max in
  ford_fulkerson new_graph2 dep fin;;
 
