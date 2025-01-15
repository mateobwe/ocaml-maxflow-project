open Gfile
open Tools
open Algo
open Graph
open Printf

let () =
   
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        (
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ) ;
      exit 0
    end ;
  
  let infile = "graphs/autun.txt"
  and lieux = Hashtbl.create 12 in (* On crée une table de correspondance lieu/id pour pouvoir associer chaque lieu à un noeud du graphe*)

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

(* On récupère la clé associée au lieu qu'on a choisi *)
let find_key_by_value table lieu_choisi =
    Hashtbl.fold (fun key valeur acc ->
      if valeur = lieu_choisi then Some key 
      else acc
    ) table None 
  in

(* On récupère la source que l'on a rentré en argument*)
let _source = match find_key_by_value lieux Sys.argv.(1) with 
  | Some key -> key
  | None -> Printf.printf "Source non trouvée\n%!" ; assert false in

(* On récupère le puits que l'on a rentré en argument*)
let _sink = match find_key_by_value lieux Sys.argv.(2) with 
  | Some key -> key
  | None -> Printf.printf "Source non trouvée\n%!" ; assert false 

  in
  let graph = from_file infile in                                           (* On récupère le graphe depuis le fichier .txt *)
  let int_graph = gmap graph int_of_string in                               (* On convertit les labels en int *)
  let graph_ecart = create_graphe_ecart int_graph in                        (* On crée le graphe d'écart *)
  let graphe_final = ford_fulkerson graph_ecart _source _sink in            (* On applique Ford-Fulkerson*)
  let graph_a_exporter = convert_graph graphe_final int_graph in            (* On reconvertit le graphe en path Graph avec l'affichage "flot/capacité" sur les arcs*)

  let graph_2 = e_fold graph_a_exporter (fun g arc -> if arc.lbl.[0] = '0' then g else new_arc g arc) (clone_nodes graph_a_exporter) in

  (*CHANGEMENTS A VERIFIER AVEC ELIAN*)

  let labels = List.map (fun arc -> String.split_on_char '/' arc.lbl) (out_arcs graph_a_exporter _source) in  
  let flot_trouve = List.fold_left (fun acc label -> match label with        (* On récupère la valeur du flot max sur notre graphe de String*)
  |x::_ -> acc + int_of_string x
  | _ -> assert false
    ) 0 labels in
  let ff = open_out "resultat_autun" in
  let texte = (( string_of_int flot_trouve) ^ " personnes peuvent être transportées de \"" ^ Sys.argv.(1) ^ "\" (" ^ (string_of_int _source) ^ ") à \"" ^ Sys.argv.(2) ^"\" (" ^ (string_of_int _sink) ^ ").\n") in
  fprintf ff "\n%s" texte ; 
  close_out ff ;
  export  "SOLUTION.dot" graph_a_exporter;
  export  "SOLUTION_SANS_ZERO.dot" graph_2;
 
  ()
