let preparer_niveau campagne okref numsuivant =
  let chemin_sauvegarde = Filename.concat Path.userdir "progression" in
  let chemin_fichier = Filename.concat chemin_sauvegarde 
    (campagne.Niveaux.nom) in
  let numero = 
    try
      let fichier = open_in chemin_fichier in
      let i = input_binary_int fichier in
      close_in fichier ;
      i
    with
    | _ -> 0
  in
  if numero < List.length campagne.Niveaux.niveaux 
  then 
    begin
      numsuivant := numero ;
      List.nth campagne.Niveaux.niveaux numero
    end
  else 
    begin
      print_endline "La campagne proposée est finie." ;
      match campagne.Niveaux.niveaux with
      | [] -> 
	begin
	  print_endline "Normal : elle est vide !" ;
	  okref := false ;
	  {
	    Niveaux.murs = [|[|false|]|] ;
	    Niveaux.depart = (0, 0) ;
	    Niveaux.caisses = [] ;
	    Niveaux.objectifs = [] ;
	    Niveaux.descr = "" ;
	  }
	end
      | niv :: _ ->
	begin
	  numsuivant := 0 ;
	  niv
	end
    end
;;

let preparer table_campagnes okref numsuivant =
  let liste = Hashtbl.fold 
    (fun nom campagne fin -> nom :: fin) table_campagnes [] in
  let tableau = Array.of_list liste in
  print_endline "Sélectionnez votre campagne..." ;
  Array.iteri (fun i nom -> print_int (i + 1) ; print_string " : " ; print_endline nom)
    tableau ;
  print_endline "0 : quitter" ;
  try
    let reponse = String.trim (read_line ()) in
    match (int_of_string reponse) with
    | 0 -> 
      begin
	okref := false ;
	{
	  Niveaux.murs = [|[|false|]|] ;
	  Niveaux.depart = (0, 0) ;
	  Niveaux.caisses = [] ;
	  Niveaux.objectifs = [] ;
	  Niveaux.descr = "" ;
	}
      end
    | i when i <= Array.length tableau ->
      preparer_niveau (Hashtbl.find table_campagnes tableau.(i - 1)) 
	okref numsuivant
    | _ -> failwith "int_of_string"
  with
  | Failure "int_of_string" -> 
    begin
      print_endline "Ce n'est pas un numéro autorisé." ;
      okref := false ;
      {
	Niveaux.murs = [|[|false|]|] ;
	Niveaux.depart = (0, 0) ;
	Niveaux.caisses = [] ;
	  Niveaux.objectifs = [] ;
	  Niveaux.descr = "" ;
      }
    end
;;
