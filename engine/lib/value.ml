(* value.ml — Valeurs SQL et logique à 3 valeurs (3VL).
   Fidèle à Guagliardo & Libkin, "A Formal Semantics of SQL Queries", PVLDB 2017 :
   - une valeur est une donnée OU le marqueur NULL ;
   - les conditions s'évaluent dans {True, False, Unknown} ;
   - WHERE ne garde une ligne que si sa condition vaut True (False ET Unknown rejettent). *)

type value =
  | VInt of int
  | VFloat of float
  | VStr of string
  | VBool of bool
  | VNull

(* Logique à trois valeurs *)
type tv = True | False | Unknown

(* Tables de vérité 3VL (Kleene) — voir §4 du papier.
   AND : False domine ; True seulement si les deux True ; sinon Unknown. *)
let tv_and a b =
  match a, b with
  | False, _ | _, False -> False
  | True, True -> True
  | _ -> Unknown

(* OR : True domine ; False seulement si les deux False ; sinon Unknown. *)
let tv_or a b =
  match a, b with
  | True, _ | _, True -> True
  | False, False -> False
  | _ -> Unknown

let tv_not = function True -> False | False -> True | Unknown -> Unknown
let tv_of_bool b = if b then True else False

let is_null = function VNull -> true | _ -> false

(* Vue numérique d'une valeur (pour l'arithmétique et la comparaison numérique). *)
let as_num = function
  | VInt i -> Some (float_of_int i)
  | VFloat f -> Some f
  | VBool b -> Some (if b then 1.0 else 0.0)
  | _ -> None

(* Chaîne canonique : uniquement comme ordre total de repli (ORDER BY, types mixtes). *)
let sort_string = function
  | VStr s -> s
  | VBool b -> if b then "true" else "false"
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VNull -> ""

(* Comparaison totale de deux valeurs NON-NULL :
   numériquement si les deux sont numériques, sinon lexicographiquement. *)
let compare_nonnull a b =
  match as_num a, as_num b with
  | Some x, Some y -> compare x y
  | _ -> compare (sort_string a) (sort_string b)

(* Égalité SQL de deux valeurs non-null (utilisée par = et IN). *)
let equal_nonnull a b = compare_nonnull a b = 0

let to_display = function
  | VNull -> "NULL"
  | VStr s -> s
  | VBool b -> if b then "true" else "false"
  | VInt i -> string_of_int i
  | VFloat f ->
    (* évite "3." -> "3" mais garde les décimales utiles *)
    let s = Printf.sprintf "%.12g" f in s
