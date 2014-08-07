type niveau =
  {
    murs : bool array array ;
    depart : (int * int) ;
    caisses : (int * int) list ;
    objectifs : (int * int) list ;
    descr : string ;
  }
type campagne =
  {
    nom : string ;
    niveaux : niveau list ;
  }
val creer_niveau : int -> int -> string -> int -> int -> (int * int) list -> (int * int) list -> string -> niveau
val interpreter_niveau : Simplist.valeur list -> niveau
val interpreter_campagne : Simplist.valeur list -> campagne
val presenter : niveau -> unit
