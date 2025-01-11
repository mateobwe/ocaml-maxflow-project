open Gfile
open Tools
open Algo
open Graph
open Printf

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        (
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = "graphs/autun.txt"
  and lieux = Hashtbl.create 12 in

  Hashtbl.add lieux 0 "Mairie" ;
  Hashtbl.add lieux 1 "Boulangerie";
  Hashtbl.add lieux 2 "Musee";
  Hashtbl.add lieux 3 "Parc";
  Hashtbl.add lieux 4 "Ginette";
  Hashtbl.add lieux 5 "Stade";
  Hashtbl.add lieux 6 "Hopital";
  Hashtbl.add lieux 7 "Ecole";
  Hashtbl.add lieux 8 "Theatre";
  Hashtbl.add lieux 9 "Supermarche";
  Hashtbl.add lieux 10 "Camping";
  Hashtbl.add lieux 11 "Marche" ;

  (* These command"graph.dot"-line arguments are not used for the moment. *)

  (* Fonction pour trouver la clé associée à une valeur *)
let find_key_by_value table value =
  try
    Hashtbl.fold (fun key v acc ->
      if v = value then Some key 
      else acc
    ) table None 
  with Not_found -> Printf.printf "Valeur non trouvée" ; assert false in

(* Exemple d'utilisation pour "Marché" *)
let _source = match find_key_by_value lieux Sys.argv.(1) with 
  | Some key -> key
  | None -> Printf.printf "Source non trouvée" ; assert false in
Printf.printf "Source: %d\n" _source ;
let _sink = match find_key_by_value lieux Sys.argv.(2) with 
  | Some key -> key
  | None -> Printf.printf "Source non trouvée" ; assert false 

  in

  Printf.printf "Puits: %d\n" _sink ;
  Printf.printf "Le fichier a été écrit avec succès\n" ;
  let graph = from_file infile in
  let int_graph = gmap graph int_of_string in
  let graph_ecart = create_graphe_ecart int_graph in
  let graphe_final = ford_fulkerson graph_ecart _source _sink in
  let graph_a_exporter = convert_graph graphe_final int_graph in
  let labels = List.map (fun arc -> String.split_on_char '/' arc.lbl) (out_arcs graph_a_exporter _source) in
  let flot_trouve = List.fold_left (fun acc label ->match label with 
  |x::_ -> acc + int_of_string x
  | _ -> assert false
    ) 0 labels in
  let ff = open_out "resultat_autun" in
  let texte = (( string_of_int flot_trouve) ^ " personnes peuvent être transportées de \"" ^ Sys.argv.(1) ^ "\" (" ^ (string_of_int _source) ^ ") à \"" ^ Sys.argv.(2) ^"\" (" ^ (string_of_int _sink) ^ ").\n") in
  fprintf ff "%s" texte ; 
  close_out ff ;
  Printf.printf "Le fichier a été écrit avec succès 2\n" ;
  export  "SOLUTION.dot" graph_a_exporter;
 
  ()
