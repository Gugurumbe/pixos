type valeur =
| Float of float
| Int of int
| String of string
| List of valeur list
| Function of (valeur list -> valeur)
type interprete =
{
  eval : string -> valeur ;
  add : string -> valeur -> unit ;
}
val make_interp : unit -> interprete
