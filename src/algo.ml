open Tools
open Graph

type flot = {
capacite : int ;
flot_act : int;
};;


let arc_valid arc =
  assert (arc.lbl >= 0);
  if (arc.lbl != 0) then (Some arc.tgt) else None ;;


let rec trans_liste liste aux = match liste with
  | [] -> aux
  | None::rest -> trans_liste rest aux
  | (Some(x))::rest -> trans_liste rest (x::aux);;

(* 
  let find_path2 (grDC: id graph) (id1:id) (id2:id) =
    let rec aux current_path visited = function
    | [] -> None
    | id::[] -> Printf.printf "cc\n" ; aux (id::current_path) (id::visited) (trans_liste (List.map arc_valid (out_arcs grDC id))[])
    | id::rest ->
      if List.mem id visited then aux current_path visited rest
      else if id = id2 then Some (List.rev (id :: current_path))
      else
        let out = out_arcs grDC id in 
        let next_nodes = List.map (fun arc -> (Printf.printf "label actuel : %d\n" arc.lbl); arc_valid arc) out in
        Printf.printf "Next nodes %s\n: " (String.concat " -> " (List.map string_of_int  (trans_liste next_nodes [] ) )) ;
        match aux (id :: current_path) (id :: visited) (List.filter (fun x -> not (List.mem x visited)) ( trans_liste next_nodes rest)) with
          | None -> Printf.printf "on est où là\n " ; aux current_path visited rest
          | some_path -> match some_path with
            | Some x -> Printf.printf "Chemin: %s\n" (String.concat " -> " (List.map string_of_int  x )); some_path
            | _ -> assert false in
aux [] [id1] [id1] ;; *)

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

(* Le problème c'est qu'on marque mal les noeuds visités (par exemple pour aller de 0 à 7 dans le graphe 7 on va faire 0->8->0->7 mais on ne renote pas 0 dans l'itinéraire ce qui crée un bug. Il faut réussir à enlever le chemin 0->8 8->0 qui est inutile car il apporte rien de 0 à 5)*)
let find_arc_in_path gr id1 id2 =
  try
    Some (List.find (fun arc -> arc.tgt = id2) (out_arcs gr id1))
  with
  | Not_found -> Printf.printf("Arc non trouvé dans le chemin ");None;;

let transfo_arc grff g arc=
  let val1= find_arc grff arc.src arc.tgt in
  match val1 with
  |Some arc1 ->  if (arc1.lbl > arc.lbl ) then new_arc g {src = arc.src ; tgt= arc.tgt; lbl= ("0/" ^ string_of_int (arc.lbl))}
  else new_arc g {src = arc.src ; tgt= arc.tgt; lbl= (string_of_int (arc.lbl-arc1.lbl) ^ "/" ^ string_of_int (arc.lbl))}
  | _ -> assert false


let convert_graph (grff: int graph ) (grb: int graph) =
  let new_gr = clone_nodes grb in
  e_fold grb (fun g arc -> (transfo_arc grff g arc)) new_gr ;;

let rec update_path2 (gr:id graph) (chemin: id list) valeur =
   match chemin with
  |[] -> gr
  |_::[]-> gr
  | x::y::rest ->  match find_arc_in_path gr x y with
    | None -> (*assert false*) Printf.printf "L'arc de %d à %d n'a pas été trouvé \n" x y ; gr
    | Some(arc) -> Printf.printf "Entrain d'augmenter l'arc de %d à %d, de %d \n" arc.src arc.tgt valeur ; update_path2 (add_arc gr arc.src arc.tgt valeur) (y::rest) valeur;;

  (* | x::y::rest ->  let arc_trouve = find_arc_in_path gr x y in match arc_trouve with
  | None -> assert false
  | Some(arc) -> Printf.printf "Entrain d'augmenter l'arc de %d à %d, de %d \n" arc.src arc.tgt valeur ; update_path2 (add_arc gr arc.src arc.tgt valeur) (y::rest) valeur;; *)

let create_graphe_ecart (gr:id graph) =
  let new_gr = clone_nodes gr in
  e_fold gr (fun g arc -> (new_arc (new_arc g {src = arc.tgt ; tgt= arc.src; lbl=arc.lbl (* C'est 0 ici normalement*)}) {src = arc.src; tgt= arc.tgt; lbl= arc.lbl})) new_gr ;; 


(*let find_arc_in_path gr id1 id2 = List.find (fun arc -> arc.tgt = id2) (out_arcs gr id1)*)

let rec find_max_possible graph chemin aux =
  match chemin with
  |[] -> aux
  |_::[]-> aux
  |x::y::rest -> match find_arc_in_path graph x y with
    | None -> aux
    | Some arc  ->  if (arc.lbl < aux && arc.lbl >= 0) then (Printf.printf "here\n " ; find_max_possible graph (y::rest) (arc.lbl))
    else (Printf.printf "\n Label de l'arc: %d\n  " arc.lbl ; find_max_possible graph (y::rest) aux )
;;



let rec ford_fulkerson (graph: id graph) dep fin =
  match find_path graph [] dep fin with
  |None -> graph
  |Some chemin ->
    let max =  find_max_possible graph chemin max_int
  in Printf.printf "Valeur max actuelle: %d%!\n" max ; match max with
  | 0 -> graph
  | _ -> let new_graph = update_path2 graph chemin (-max) in
          let new_graph2 =  update_path2 new_graph (List.rev chemin) max in
  ford_fulkerson new_graph2 dep fin;;
 
