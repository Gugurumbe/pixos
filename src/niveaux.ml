type niveau =
{
  murs : bool array array ;
  depart : (int * int) ;
  caisses : (int * int) list ;
  objectifs : (int * int) list ;
  descr : string ;
} ;;

type campagne = 
  {
    nom : string ;
    niveaux : niveau list
  }
;;

let creer_niveau w h masque_murs x y caisses objectifs description =
  let murs = Array.init h (fun i -> Array.init w (fun j ->
    try
      match masque_murs.[w * i + j] with
      | ' ' -> false
      | _ -> true
    with
    | _ -> failwith "Votre masque n'est pas assez grand."
  )) in
  {
    murs = murs ;
    depart = (x, y) ;
    caisses = caisses ;
    objectifs = objectifs ;
    descr = description
  }
;;

open Simplist ;;

let interpreter_niveau = function
  | [List [List [Int w ; Int h] ; String masque ; List [Int x ; Int y] ; List liste_caisses ; List liste_objectifs ; String description]] ->
    let caisses = List.map (function 
      | List [Int x ; Int y] -> (x, y)
      | _ -> failwith "Mauvais usage de l'interprétation de niveau (caisses)") 
      liste_caisses in
    let objectifs = List.map (function 
      | List [Int x ; Int y] -> (x, y)
      | _ -> failwith "Mauvais usage de l'interprétation de niveau (objectifs)") 
      liste_objectifs in
    creer_niveau w h masque x y caisses objectifs description
  | _ -> failwith "Mauvais usage de l'interprétation de niveau."
;;

let interpreter_campagne = function
  | [String nom ; List niveaux] ->
    {
      nom = nom ;
      niveaux = List.map (fun niveau -> interpreter_niveau [niveau]) niveaux
    }
  | _ -> failwith "Mauvais usage de l'interprétation de campagne."
;;

let string_of_pos (i, j) = "(x="^(string_of_int i)^", y="^(string_of_int j)^")" ;;

let presenter n =
  print_string "Niveau suivant : " ;
  print_endline n.descr ;
  print_endline ("Taille : "^(string_of_pos (Array.length n.murs.(0), Array.length n.murs))) ;
  let caracteres = Array.map (Array.map (fun b -> if b then "X" else " ")) n.murs in
  print_endline ("Départ : "^(string_of_pos n.depart)) ;
  caracteres.(snd n.depart).(fst n.depart) <- "P" ;
  List.iter (fun (x, y) -> print_endline ("Caisse en "^string_of_pos (x, y)) ; caracteres.(y).(x) <- "c") n.caisses ;
  List.iter (fun (x, y) -> print_endline ("Objectif en "^string_of_pos (x, y)) ; caracteres.(y).(x) <- "o") n.objectifs ;
  let lignes = Array.map (Array.to_list) caracteres in
  let strings = Array.map (String.concat "") lignes in
  Array.iter (print_endline) strings 
;;
