(* Yes, we have to repeat open Graph. *)
open Graph

let find_path (gr:'a graph) (id1:id) (id2:id) =
  let rec aux current_path visited = function
    | [] -> None
    | id::rest ->
      if List.mem id visited then aux current_path visited rest
      else if id = id2 then Some (List.rev (id::current_path))
      else match out_arcs gr id with
        | [] -> aux current_path visited rest
        | arcs ->
          let next_nodes = List.map (fun arc -> arc.tgt) arcs in
          match aux (id::current_path) (id::visited) next_nodes with
          | None -> aux current_path visited rest
          | some_path -> some_path
    in
    aux [id1] [] [id1]

