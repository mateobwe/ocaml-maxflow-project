(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr:'a graph) = n_fold gr new_node empty_graph;;


let gmap (gr:'a graph) (f:'a -> 'b) =
    let new_gr = clone_nodes gr in 
    e_fold gr (fun g arc -> (new_arc g {src = arc.src; tgt= arc.tgt; lbl= (f arc.lbl)} )) new_gr ;;

let add_arc (gr:int graph) (id1:id) (id2:id) (n:int) = match (find_arc gr id1 id2) with
    | Some arc -> new_arc gr {src = id1; tgt = id2; lbl = arc.lbl + n}
    | None -> new_arc gr {src = id1; tgt = id2; lbl = n} ;;

