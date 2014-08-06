type valeur =
| Float of float
| Int of int
| String of string
| List of valeur list
| Function of (valeur list -> valeur)
;;
(* Toutes les valeurs possibles *)
type instruction =
| Valeur of valeur
| Implicite of string
| Application of instruction list
;;
(* On interprète des instructions. *)


(* I) Mémoire *)
type memoire = (string, valeur) Hashtbl.t ;;
let mem = Hashtbl.mem ;;
let rem = Hashtbl.remove ;;
let add memoire cle valeur = 
  if mem memoire cle then rem memoire cle ;
  Hashtbl.add memoire cle valeur
;;
let find = Hashtbl.find ;;
let make_mem n = Hashtbl.create n ;;

(* II) Analyser *)
type mot = 
| Mot_regulier of string
| Mot_echappe of string
(* La distinction sert à savoir si "(" est la fonction "(" définie par *)
(* l'utilisateur (!) via "\(" ou une parenthèse ouvrante *)
| Mot_cite of string
let rec lire_mot (chaine : string) (debut : int) : mot * int =
  let rec aux i guillemets =
    (* Renvoie le triplet (liste_des_lettres, indice_fin, 
       le_mot_est_il_echappe) *)
    if i < String.length chaine then
      match chaine.[i] with
      | ';' when i + 1 < String.length chaine && chaine.[i + 1] = ';' ->
	(* ;; ceci est un commentaire *)
	([], i, false)
      | '\\' when i + 1 < String.length chaine && guillemets ->
	(* Lorsqu'on est entre guillemets, \n veut dire '\n' et pas 'n' *)
	begin
	  let (fin, i_fin, _) = aux (i + 2) guillemets in
	  match chaine.[i + 1] with
	  | '\\' -> ('\\' :: fin, i_fin, true)
	  | 'n' -> ('\n' :: fin, i_fin, true)
	  | 't' -> ('\t' :: fin, i_fin, true)
	  | autre -> (autre :: fin, i_fin, true)
	end
      | '\\' when i + 1 < String.length chaine ->
	let (fin, i_fin, _) = aux (i + 2) guillemets in
	(chaine.[i + 1] :: fin, i_fin, true)
      | '('
      | ')'
      |'\''
      | ' '
      | '\n'
      | '\t' when not guillemets -> ([], i, false)
	(* On s'arrête de lire si on n'est pas entre guillemets *)
      | '\"' ->
	([], i, false)
      | '\n' 
      | '\t' ->
	([], i, false)
      (* C'est bizarre de mettre des sauts de ligne entre guillemets, non ? *)
      | c ->
	let (fin, i_fin, echappe) = aux (i + 1) guillemets in
	(c :: fin, i_fin, echappe)
    else ([], i, true)
  in
  let rec avancer (i : int) (commentaire : bool) : int =
    (* Renvoie l'indice de l'initiale du mot suivant i *)
    if i < String.length chaine then
      match chaine.[i] with
      | ';' when i + 1 < String.length chaine && chaine.[i + 1] = ';' ->
	(* on s'arrête au prochain \n *)
	avancer (i + 2) true
      | ' '
      | '\n'
      | '\t' when not commentaire -> avancer (i + 1) false
      | '\n' when commentaire -> avancer (i + 1) false
      | _ when commentaire -> avancer (i + 1) true
      | _ -> i
    else i
  in
  (* Cas particuliers : on lit à partir d'une parenthèse ou d'un guillemet *)
  if debut < String.length chaine then
    match chaine.[debut] with
    | '('
    | ')'
    |'\'' ->
      (Mot_regulier (String.make 1 chaine.[debut]), avancer (debut + 1) false)
    |'\"' ->
      let (lettres, i_fin, _) = aux (debut + 1) true in
      if i_fin < String.length chaine && chaine.[i_fin] = '\"' then
	(Mot_cite (String.concat "" (List.map (String.make 1) lettres)),
	 avancer (i_fin + 1) false) 
      else failwith "Guillemet interrompu."
    (* + 1 : sinon on tombe sur le guillemet fermant *)
    | ';' when debut + 1 < String.length chaine && chaine.[debut + 1] = ';' ->
      lire_mot chaine (avancer debut true)
	(* Sion attaque directement sur un commentaire, il ne faut pas *)
	(* lire "" ! *)

    | _ ->
      let (lettres, i_fin, echappe) = aux debut false in
      if echappe then
	(Mot_echappe (String.concat "" (List.map (String.make 1) lettres)),
	 avancer i_fin false)
      else (Mot_regulier (String.concat "" (List.map (String.make 1) lettres)),
	 avancer i_fin false)
  else (Mot_regulier "", debut)
;;
(* Et maintenant, on découpe le programme en mots...*)
let decouper chaine =
  let rec aux i =
    if i < String.length chaine then
      let (mot_suivant, i_fin) = lire_mot chaine i in
      mot_suivant :: (aux i_fin)
    else []
  in
  aux 0
;;
(* On remplace le raccourci pour "quote"... *)
let rec remplacer_quote = function
  | (Mot_regulier "'") :: (Mot_regulier "(") :: reste ->
    (Mot_regulier "(") :: (Mot_regulier "quote") :: (remplacer_quote reste)
      (* Usage le plus courant : '(1 2 3) est remplacé par (quote 1 2 3) *)
      (* NB : je sais que ce n'est pas le seul en common lisp *)
  | (Mot_regulier "'") :: _ ->
    failwith "Analyse syntaxique : votre ' n'a rien à faire ici !"
  | [] -> []
  | a :: b ->
    a :: (remplacer_quote b)
;;
(* Puis on les transforme en instructions ! *)
type objet =
| Donnee of valeur
| Mot of string
| PO
| PF
;;
(* D'abord, on repère les parenthèses. *)
let simparser chaine =
  let liste_mots = remplacer_quote (decouper chaine) in
  let reconnaitre = function
    | Mot_cite truc -> Donnee (String truc)
    | Mot_regulier "(" -> PO
    | Mot_regulier ")" -> PF
    | Mot_regulier "'" -> failwith "Un ' a échappé à mon préprocesseur !"
    | Mot_regulier machin
    | Mot_echappe machin ->
      begin
	try
	  Donnee (Int (int_of_string machin))
	with
	| _ ->
	  try
	    Donnee (Float (float_of_string machin))
	  with
	  | _ -> (* Bon, c'est pas un nombre. *)
	    Mot machin
      end
  in
  let liste_objets = List.map (reconnaitre) liste_mots in
  (* Et maintenant, on hiérarchise ! *)
  let rec aux = function
    (* Renvoie le couple (liste_des_instructions, objets_restants) *)
    | [] -> ([], [])
    | PO :: suite -> 
      begin
	let (liste, fin) = aux suite in
	let (reste_niveau, finfin) = aux fin in
	((Application liste) :: reste_niveau, finfin)
      end
    | PF :: suite ->
      ([], suite)
    | (Donnee bidule) :: suite ->
      let (reste_niveau, fin) = aux suite in
      ((Valeur bidule) :: reste_niveau, fin)
    | (Mot chose) :: suite ->
      let (reste_niveau, fin) = aux suite in
      ((Implicite chose) :: reste_niveau, fin)
  in
  fst (aux liste_objets)
;;

(* III) Évaluer *)
(* III)A) Dépendances de "if" et "cond" *)
let evalb = function
  | Float f -> f <> 0.
  | Int i -> i <> 0
  | String str -> failwith ("Ne peut convertir "^str^" en booléen.")
  | List [] -> false (* Ainsi, nil peut être compris comme faux *)
  | List _ -> true
  | Function _ -> failwith "Ne peut convertir une fonction en booléen."
;;
(* III)B) Dépendances de "defun" *)
let rec argnames = function
  | [] -> []
  | (Implicite nom) :: reste -> nom :: (argnames reste)
  | _ -> failwith "La liste des arguments ne doit contenir que des noms."
;;
let rec copier_dependances source dest = function
  (* On copie toutes les dépendances possibles pour construire une fonction *)
  (* qui ne dépend pas de l'évolution future de la mémoire principale. *)
  | Application liste ->
    List.iter (copier_dependances source dest) liste
  | Implicite k when mem source k ->
    (* Le "mem source k" n'est évidemment pas automatique : on peut définir *)
    (* une nouvelle valeur dans le corps d'une fonction, qu'on réutilisera  *)
    (* dans cette même fonction. *)
    add dest k (find source k)
  | _ -> ()
;;
let lier_args noms valeurs memoire =
  (* On tient compte de la valeur des arguments *)
  let rec aux = function
    | ([], []) -> ()
    | (nom :: autres_noms, valeur :: autres_valeurs) ->
      begin
	add memoire nom valeur ;
	aux (autres_noms, autres_valeurs)
      end
    | _ -> 
      failwith "Mauvais nombre d'arguments passés à la fonction."
  in
  aux (noms, valeurs)
;;
(* III)C) Définition de évaluer *)
let rec evaluer memoire = function
  | [] -> List []
  | [Valeur v] -> v
  | [Implicite str] when mem memoire str -> find memoire str
  | [Implicite str] -> failwith ("Valeur inconnue : "^str)
  | [Application liste] ->
    begin
      match liste with
      | [] -> List []
      | (Application _) :: _ -> evaluer memoire liste
      | (Valeur _) :: _ -> failwith "Erreur de syntaxe pour un appel de fonction"
      | (Implicite appel) :: args ->
	match appel with
	| "if" -> evaluer_if memoire liste
	| "cond" -> evaluer_cond memoire liste
	| "defun" -> evaluer_defun memoire liste
	| "and" -> evaluer_and memoire liste
	| "or" -> evaluer_or memoire liste
	| "let" -> evaluer_let memoire liste
	| "lambda" -> evaluer_lambda memoire liste
	| _ ->
	  if mem memoire appel then
	    match find memoire appel with
	    | Function f -> f (List.map (fun arg -> evaluer memoire [arg]) args)
	    | _ -> failwith (appel^" n'est pas une fonction !")
	  else failwith ("Function inconnue : "^appel)
    end
  | a :: b ->
    match evaluer memoire [a] with
    | List [] -> evaluer memoire b
    | _ -> failwith ("Les instructions intermédiaires doivent renvoyer []")
and evaluer_if memoire = function
  | [Implicite "if" ; test ; effet ; _] when evalb (evaluer memoire [test]) ->
    evaluer memoire [effet]
  | [Implicite "if" ; _ ; _ ; effet] ->
    evaluer memoire [effet]
  | _ -> failwith ("if : erreur de syntaxe")
and evaluer_cond memoire = function
  | (Implicite "cond") :: paires ->
    let fonction_iteree (fait, resultat) = function
      | Application [test  ; cas]
	  when (not fait) && evalb (evaluer memoire [test])
	    -> (true, evaluer memoire [cas])
      | Application _ when not fait ->
	(false, List [])
      | Application _ -> (true, resultat)
      | _ -> failwith "cond : erreur de syntaxe"
    in
    let (ok, resultat) = List.fold_left (fonction_iteree) 
      (false, List []) paires in
    if ok then resultat
    else failwith "cond : cas non répertorié"
  | _ -> failwith "Ne peut arriver : on appelle cond."
and evaluer_defun memoire = function
  | (Implicite "defun") :: 
      (Implicite nom) :: 
      (Application liste_args) :: 
      resultat ->
    begin
      try
	let noms = argnames liste_args in
	let memoire_locale = Hashtbl.create 20 in
	List.iter (copier_dependances memoire memoire_locale) resultat ;
	let deja_ajoute = ref false in
	let rec fonction arguments =
	  try
	    lier_args noms arguments memoire_locale ;
	    if not (!deja_ajoute) 
	    then begin
		add memoire_locale nom (Function (fonction)) ;
		deja_ajoute := true
		  (* À chaque appel récursif ! *)
	      end ;
	    (* ^^^^^^C'est LÀ que le mot-clé "rec" est justifié ^^^^^^ *)
	    evaluer memoire_locale resultat 
	  with
	  | Failure str -> 
	    failwith ("Erreur lors de l'appel de "^nom^" : "^str)
	in
	add memoire nom (Function (fonction)) ;
	List []
      with
      | Failure str ->
	failwith ("Erreur lors de la définition de "^nom^" : "^str)
    end
  | _ -> failwith "defun : erreur de syntaxe"
and evaluer_lambda memoire = function
  | (Implicite "lambda") :: (Application liste_args) :: resultat ->
    begin
      try
	let noms = argnames liste_args in
	let memoire_locale = Hashtbl.create 20 in
	List.iter (copier_dependances memoire memoire_locale) resultat ;
	let fonction arguments =
	  try
	    lier_args noms arguments memoire_locale ;
	    evaluer memoire_locale resultat
	  with
	  | Failure str ->
	    failwith ("Erreur lors de l'appel de (fonction anonyme) : "^str)
	in
	Function (fonction)
      with
      | Failure str ->
	failwith ("lambda : erreur : "^str)
    end
  | _ -> failwith "lambda : erreur de syntaxe."
and evaluer_and memoire = function
  | (Implicite "and") :: operandes ->
    let rec aux = function
      | [] -> Int 1
      | test1 :: autres when not (evalb (evaluer memoire [test1])) -> Int 0
      | _ :: autres -> aux autres
    in
    aux operandes
  | _ -> failwith "Ne peut arriver."
and evaluer_or memoire = function
  | (Implicite "or") :: operandes ->
    let rec aux = function
      | [] -> Int 0
      | test1 :: autres when evalb (evaluer memoire [test1]) -> Int 1
      | _ :: autres -> aux autres
    in
    aux operandes
  | _ -> failwith "Ne peut arriver."
and evaluer_let memoire = function
  | [Implicite "let" ; Implicite nom ; valeur] ->
    (* Version simple *)
    begin
      add memoire nom (evaluer memoire [valeur]) ;
      List []
    end
  | (Implicite "let") :: paires ->
    let rec aux = function
      | [] -> List []
      | (Application [Implicite nom ; valeur]) :: reste ->
	begin
	  add memoire nom (evaluer memoire [valeur]) ;
	  aux reste
	end
      | _ -> failwith "let : erreur de syntaxe"
    in
    aux paires
  | _ -> failwith "let : erreur de syntaxe"
;;

(* IIII) Librairie standard *)
let fonction_plus liste =
  let aux x y =
    match (x, y) with
    | (Int x, Int y) -> Int (x + y)
    | (Float x, Int y) -> Float (x +. (float_of_int y))
    | (Int x, Float y) -> Float ((float_of_int x) +. y)
    | (Float x, Float y) -> Float (x +. y)
    | _ -> failwith "La fonction + n'ajoute que des nombres."
  in
  List.fold_left (aux) (Int 0) liste
;;
let fonction_moins = function
  | [Int x ; Int y] -> Int (x - y)
  | [Float x ; Int y] -> Float (x -. (float_of_int y))
  | [Int x ; Float y] -> Float ((float_of_int x) -. y)
  | [Float x ; Float y] -> Float (x -. y)
  | _ -> failwith "La fonction - soustrait un nombre à un autre."
;;
let fonction_fois liste =
  let aux x y =
    match (x, y) with
    | (Int x, Int y) -> Int (x * y)
    | (Float x, Int y) -> Float (x *. (float_of_int y))
    | (Int x, Float y) -> Float ((float_of_int x) *. y)
    | (Float x, Float y) -> Float (x *. y)
    | _ -> failwith "La fonction * ne multiplie que des nombres."
  in
  List.fold_left (aux) (Int 1) liste
;;
let fonction_diviser = function
  | [Int x ; Int y] -> Int (x / y)
  | [Float x ; Int y] -> Float (x /. (float_of_int y))
  | [Int x ; Float y] -> Float ((float_of_int x) /. y)
  | [Float x ; Float y] -> Float (x /. y)
  | _ -> failwith "La fonction / divise un nombre par un autre."
;;
let fonction_superieur liste = 
  try
    match fonction_moins liste with
    | Int x -> x >= 0
    | Float x -> x >= 0.
    | _ -> failwith "La fonction >= compare deux nombres."
  with
  | _ -> failwith "La fonction >= compare deux nombres."
;;
let fonction_superieurstrict liste =
  try
    match fonction_moins liste with
    | Int x -> x > 0
    | Float x -> x > 0.
    | _ -> failwith "La fonction > compare deux nombres."
  with
  | _ -> failwith "La fonction > compare deux nombres."
;;
let fonction_egal liste = 
  match liste with
  | [Int x ; Int y]
    -> x = y
  | [Float x ; Float y]
    -> x = y
  | [List x ; List y]
    -> x = y
  | [String x ; String y]
    -> x = y
  | [List vide ; Int x]
  | [Int x ; List vide] when x = 0 -> vide = []
  | [Int x ; Float y]
  | [Float y ; Int x] -> y = (float_of_int x)
  | _ -> failwith "La fonction = compare deux objets de même type 
(sauf les fonctions), une liste et nil, un entier et un flottant."
;;
let fonction_different liste =
  try
    not (fonction_egal liste)
  with
  | _ -> failwith "La fonction /= compare deux objets (sauf les fonctions)."
;;
let fonction_inferieur liste =
  try
    not (fonction_superieurstrict liste)
  with
  | _ -> failwith "La fonction <= compare deux nombres."
;;
let fonction_inferieurstrict liste =
  try
    not (fonction_superieur liste)
  with
  | _ -> failwith "La fonction < compare deux nombres."
;;
let fonction_null = function
  | [List []] -> Int 1
  | [List _] -> Int 0
  | _ -> failwith "La fonction null teste la vacuité d'une liste."
;;
let fonction_rem = function
  | [Int n ; Int p] -> Int (n mod p)
  | [Float i ; Int j] -> Float (mod_float i (float_of_int j))
  | [Int i ; Float j] -> Float (mod_float (float_of_int i) j)
  | [Float i ; Float j] -> Float (mod_float i j)
  | _ -> failwith "(rem n p) renvoie le reste de n / p"
;;
let fonction_abs = function
  | [Int n] -> Int (abs n)
  | [Float f] -> Float (abs_float f)
  | _ -> failwith "(abs x) renvoie la valeur absolue de x"
;;
let fonction_max = function
  | [] -> failwith "(max) n'a pas de sens."
  | premier :: suite ->
    let rec aux maxtemp = function
      | [] -> maxtemp
      | concurrent :: fin when fonction_superieur [maxtemp ; concurrent] ->
	aux maxtemp fin
      | concurrent :: fin ->
	aux concurrent fin
    in
    aux premier suite
;;
let fonction_min = function
  | [] -> failwith "(min) n'a pas de sens."
  | premier :: suite ->
    let rec aux mintemp = function
      | [] -> mintemp
      | concurrent :: fin when fonction_inferieur [mintemp ; concurrent] ->
	aux mintemp fin
      | concurrent :: fin ->
	aux concurrent fin
    in
    aux premier suite
;;
let fonction_unplus = function
  | [Int x] -> Int (x + 1)
  | [Float x] -> Float (x +. 1.)
  | _ -> failwith "(1+ x) renvoie x + 1"
;;
let fonction_unmoins = function
  | [Int x] -> Int (x - 1)
  | [Float x] -> Float (x -. 1.)
  | _ -> failwith "(1- x) renvoie x - 1"
;;
let fonction_zerop = function
  | [Int 0]
  | [Float 0.]
  | [List []] -> Int 1
  | [Int _]
  | [Float _]
  | [List _] -> Int 0
  | _ -> failwith "(zerop k) teste si k=0 ou k=[]"
;;
let fonction_plusp = function
  | [Int k] -> Int (if k > 0 then 1 else 0)
  | [Float x] -> Int (if x > 0. then 1 else 0)
  | _ -> failwith "(plusp x) teste si x > 0"
;;
let fonction_minusp = function
  | [Int k] -> Int (if k < 0 then 1 else 0)
  | [Float x] -> Int (if x < 0. then 1 else 0)
  | _ -> failwith "(minusp x) teste si x < 0"
;;
let fonction_evenp = function
  | [Int k] -> Int (if k mod 2 = 0 then 1 else 0)
  | [Float x] -> Int (if mod_float x 2. = 0. then 1 else 0)
  | _ -> failwith "(evenp x) teste si x = 0 [2]"
;;
let fonction_oddp = function
  | [Int k] -> Int (if k mod 2 <> 0 then 1 else 0)
  | [Float x] -> Int (if mod_float x 2. <> 0. then 1 else 0)
  | _ -> failwith "(oddp x) teste si x /= 0 [2]"
;;
let fonction_not = function
  | [Int 0] 
  | [Float 0.] -> Int 1
  | [Int _]
  | [Float _] -> Int 0
  | _ -> failwith "(not test) renvoie vrai ssi test est faux."
;;
let fonction_cons = function
  | [objet ; List suite] -> List (objet :: suite)
  | _ -> failwith "(cons head tail) renvoie la liste dont la tête est head
et la queue tail."
;;
let fonction_first = function
  | [List []] -> failwith "first : liste vide !"
  | [List (a :: _)] -> a
  | _ -> failwith "(first list) renvoie la tête de list."
;;
let fonction_rest = function
  | [List []] -> failwith "rest : liste vide !"
  | [List (_ :: a)] -> List a
  | _ -> failwith "(rest list) renvoie la queue de la liste."
;;
let fonction_consp = function 
  | [List []] -> Int 0
  | [List _] -> Int 1
  | _ -> failwith "(consp list) teste si la liste n'est pas vide."
;;
let fonction_list_length = function
  | [List liste] -> Int (List.length liste)
  | _ -> failwith "(list-length list) renvoie la taille de la liste."
;;
let fonction_concat_string liste =
  let convertir = function
    | String s -> s
    | _ -> failwith "(concat-string truc machin bidule) concatène les chaînes."
  in
  String (String.concat "" (List.map (convertir) liste))
;;
let load_stl memoire =
  add memoire "t" (Int 1) ;
  add memoire "nil" (Int 1) ;
  add memoire "quote" (Function (fun liste -> List liste)) ;
  add memoire "+" (Function (fonction_plus)) ;
  add memoire "-" (Function (fonction_moins)) ;
  add memoire "*" (Function (fonction_fois)) ;
  add memoire "/" (Function (fonction_diviser)) ;
  add memoire ">=" (Function 
		      (fun liste -> 
			Int (if fonction_superieur liste then 1 else 0))) ;
  add memoire ">" (Function 
		      (fun liste -> 
			Int (if fonction_superieurstrict liste then 1 else 0))) ;
  add memoire "<=" (Function 
		      (fun liste -> 
			Int (if fonction_inferieur liste then 1 else 0))) ;
  add memoire "<" (Function 
		      (fun liste -> 
			Int (if fonction_inferieurstrict liste then 1 else 0))) ;
  add memoire "=" (Function 
		      (fun liste -> 
			Int (if fonction_egal liste then 1 else 0))) ;
  add memoire "/=" (Function 
		      (fun liste -> 
			Int (if fonction_different liste then 1 else 0))) ;
  add memoire "null" (Function (fonction_null)) ;
  add memoire "rem" (Function (fonction_rem)) ;
  add memoire "abs" (Function (fonction_abs)) ;
  add memoire "min" (Function (fonction_min)) ;
  add memoire "max" (Function (fonction_max)) ;
  add memoire "1+" (Function (fonction_unplus)) ;
  add memoire "1-" (Function (fonction_unmoins)) ;
  add memoire "zerop" (Function (fonction_zerop)) ;
  add memoire "plusp" (Function (fonction_plusp)) ;
  add memoire "minusp" (Function (fonction_minusp)) ;
  add memoire "evenp" (Function (fonction_evenp)) ;
  add memoire "oddp" (Function (fonction_oddp)) ;
  add memoire "not" (Function (fonction_not)) ;
  add memoire "cons" (Function (fonction_cons)) ;
  add memoire "first" (Function (fonction_first)) ;
  add memoire "rest" (Function (fonction_rest)) ;
  add memoire "consp" (Function (fonction_consp)) ;
  add memoire "list-length" (Function (fonction_list_length)) ;
  add memoire "concat-string" (Function (fonction_concat_string)) ;
;;

(* V) Interpréteur *)

type interprete =
{
  eval : string -> valeur ;
  add : string -> valeur -> unit
} ;;
let make_interp () =
  let memoire = make_mem 100 in
  load_stl memoire ;
  let interpreter chaine =
    evaluer memoire (simparser chaine)
  in
  let ajouter nom valeur =
    add memoire nom valeur
  in
  {
    eval = (interpreter) ;
    add = (ajouter) ;
  }
;;
