(* Yes, we have to repeat open Graph. *)
open Graph
open Tools

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
  
  let find_path2 (grDC: id graph) (id1:id) (id2:id) =
    let rec aux current_path visited = function
    | [] -> None
    | id::rest ->
      if List.mem id visited then aux current_path visited rest
      else if id = id2 then Some (List.rev (id::current_path))
      else let out = out_arcs grDC id in 
  let next_nodes = List.map arc_valid out in
  match aux (id::current_path) (id::visited) (trans_liste next_nodes []) with
  | None -> aux current_path visited rest
  | some_path -> some_path
in
let res = aux [id1] [] [id1] in
match res with 
| None -> None
| Some (x::y::[]) -> if (x==y) then None else Some (y::[])
| Some (_::suite) -> Some suite
| Some _ -> None;;

let find_arc_in_path gr id1 id2 =
  try
    Some (List.find (fun arc -> arc.tgt = id2) (out_arcs gr id1))
  with
  | Not_found -> Printf.printf("Erreur pas cool");None;;

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
  |x::y::rest ->  let arc_trouve = find_arc_in_path gr x y in match arc_trouve with 
  | None -> assert false
  | Some(arc) -> update_path2 (add_arc gr arc.src arc.tgt valeur) (y::rest) valeur;;
    
let create_graphe_ecart (gr:id graph) =
  let new_gr = clone_nodes gr in 
  e_fold gr (fun g arc -> (new_arc (new_arc g {src = arc.tgt ; tgt= arc.src; lbl=0}) {src = arc.src; tgt= arc.tgt; lbl= arc.lbl})) new_gr ;;
  
  
(*let find_arc_in_path gr id1 id2 = List.find (fun arc -> arc.tgt = id2) (out_arcs gr id1)*)


let rec find_max_possible graph chemin aux = 
  match chemin with 
  |[] -> aux
  |_::[]-> aux
  |x::y::rest -> let arc_trouve = find_arc_in_path graph x y in match arc_trouve with 
  | None -> aux
  | Some(arc) ->  if (arc.lbl < aux) then find_max_possible graph (y::rest) (arc.lbl) 
  else find_max_possible graph (y::rest) aux 
;;



  let rec ford_fulkerson (graph: id graph) dep fin =
    match find_path2 graph dep fin with 
    |None -> graph 
    |Some chemin -> 
      let max =  find_max_possible graph chemin max_int 
    in Printf.printf "Valeur max actuelle: %d%!\n" max ; match max with 
    | 0 -> assert false
    | _ -> let new_graph = update_path2 graph chemin (-max) in 
          let new_graph2 =  update_path2 new_graph (List.rev chemin) max in 
    ford_fulkerson new_graph2 dep fin;; 
