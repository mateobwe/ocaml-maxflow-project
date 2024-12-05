(* Yes, we have to repeat open Graph. *)
open Graph
open Tools

type flot = {
  capacite : int ;
  flot_act : int; 
};;
let init_flot_graph (gr:'a graph) = 
  gmap gr (fun label -> {capacite = label; flot_act= 0} )
;;
let find_path (gr:'a graph) (id1:id) (id2:id) =
  let rec aux current_path visited = function
    | [] -> None
    | id::rest ->
      if List.mem id visited then aux current_path visited rest
      else if id = id2 then Some (List.rev (id::current_path))
      else let out = out_arcs gr id in
        let next_nodes = List.map (fun arc -> arc.tgt) out in
        match aux (id::current_path) (id::visited) next_nodes with
        | None -> aux current_path visited rest
        | some_path -> some_path
    in
    let res = aux [id1] [] [id1] in
    match res with 
    | None -> None
    | Some (x::y::[]) -> if (x==y) then None else Some (y::[])
    | Some (_::suite) -> Some suite
    | Some _ -> None;;

let rec update_arc arc chemin valeur = 
  match chemin with 
  |[] -> arc 
  |_::[]-> arc
  |x::y::rest -> if (arc.src == x && arc.tgt == y) then ({src=arc.src;tgt = arc.tgt; lbl={capacite = arc.lbl.capacite ; flot_act = arc.lbl.flot_act+valeur}}) else update_arc arc (y::rest) valeur 
;;

let update_path (gr:flot graph) (chemin: id list) valeur = 
  let new_gr = clone_nodes gr in 
    e_fold gr (fun g arc -> (new_arc g (update_arc arc chemin valeur))) new_gr ;;

   
let create_graphe_ecart (gr:flot graph) =
  let new_gr = clone_nodes gr in 
    e_fold gr (fun g arc -> (new_arc (new_arc g {src = arc.tgt ; tgt= arc.src; lbl=arc.lbl.flot_act}) {src = arc.src; tgt= arc.tgt; lbl=(arc.lbl.capacite-arc.lbl.flot_act)})) new_gr ;;



let find_arc_in_path gr id1 id2 = List.find (fun arc -> arc.tgt = id2) (out_arcs gr id1)

let rec find_max_possible ecart_graph chemin aux = 
  match chemin with 
  |[] -> aux
  |_::[]-> aux
  |x::y::rest -> if ((find_arc_in_path ecart_graph x y).lbl < aux) then find_max_possible ecart_graph (y::rest) ((find_arc_in_path ecart_graph x y).lbl) 
                else find_max_possible ecart_graph (y::rest) aux 
;;

