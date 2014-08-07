print_endline "Répertoire de l'application : " ;;
print_endline Path.datadir ;;
print_endline "Répertoire de l'utilisateur : " ;;
print_endline Path.userdir ;;
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
let table_campagnes = Hashtbl.create 1 ;;
let campagne arguments =
  let c = Niveaux.interpreter_campagne arguments in
  Hashtbl.add table_campagnes c.Niveaux.nom c ;
  print_endline ("Ajout de la campagne "^c.Niveaux.nom^" : ") ;
  List.iter (Niveaux.presenter) c.Niveaux.niveaux ;
  List []
;;
interprete.add "do-file" (Function dofile) ;;
interprete.add "campagne" (Function campagne) ;;
(* Interprétation de la liste *)
let f = open_in (Path.lvllist) ;;
interprete.eval (read_all f) ;;
close_in f ;;
print_endline "Interprétation réussie !" ;;
      
