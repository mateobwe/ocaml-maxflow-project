open Gfile
open Tools
open Algo
 
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command"graph.dot"-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in
    
  (* Rewrite the graph that has been read. *)
  (*let chemin = (find_path graph 1 7) in*)
  let int_graph = gmap graph (fun label -> int_of_string label) in 
  let graph = create_graphe_ecart int_graph in 
  let vrai_chemin = 0::1::2::3::5::7::[] in
  let graphe_actualise = ford_fulkerson graph 0 5 in
  let max = find_max_possible graphe_actualise vrai_chemin 100 in 
  let () = write_file_path outfile (Some (max::[])) in
  let graph_a_exporter = convert_graph graphe_actualise int_graph in
  let graph_a_exporter2 = gmap graphe_actualise (fun label ->  string_of_int label)  in 
  export "7.dot" graph_a_exporter ;
  export  "ff7.dot" graph_a_exporter2;;
 
  ()
