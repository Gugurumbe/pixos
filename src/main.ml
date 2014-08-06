print_endline Path.datadir ;;
let rec read_all fichier =
  try
    let ligne = input_line fichier in
    ligne^(read_all fichier)
  with
  | End_of_file -> ""
;;
open Simplist ;;
let interprete = make_interp () ;;
let dofile = function
  | [String nom] -> 
    begin
      let nom = Filename.concat (Path.lvldir) nom in
      let f = open_in nom in
      let r = interprete.eval (read_all f) in
      close_in f ;
      r
    end
| _ -> failwith "Mauvais usage de dofile"
;;
let campagne = function
  | [String nom ; List niveaux] ->
    begin
      print_endline ("Ajout de la campagne "^nom) ;
      List []
    end
  | _ -> failwith "Mauvais usage de campagne."
;;
interprete.add "do-file" (Function dofile) ;;
interprete.add "campagne" (Function campagne) ;;
(* Interprétation de la liste *)
let f = open_in (Path.lvllist) ;;
interprete.eval (read_all f) ;;
close_in f ;;
print_endline "Interprétation réussie !" ;;
      
