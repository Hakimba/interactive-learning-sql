(* ast.ml — Arbre de syntaxe abstraite du fragment SQL supporté (incrément 1).
   Fragment SELECT-FROM-WHERE de Guagliardo & Libkin + ORDER BY / LIMIT / DISTINCT. *)

type arith = Add | Sub | Mul | Div | Mod
type cmp = Eq | Neq | Lt | Le | Gt | Ge

(* Expressions : s'évaluent en une VALEUR. *)
type expr =
  | Col of string
  | Lit of Value.value
  | Neg of expr
  | Arith of arith * expr * expr
  | Func of string * expr list

(* Conditions : s'évaluent en une valeur de vérité (3VL). *)
type cond =
  | Cmp of cmp * expr * expr
  | And of cond * cond
  | Or of cond * cond
  | Not of cond
  | IsNull of expr
  | IsNotNull of expr
  | In of expr * expr list * bool        (* bool = négation (NOT IN) *)
  | Like of expr * expr * bool           (* bool = négation (NOT LIKE) *)
  | Between of expr * expr * expr * bool  (* bool = négation (NOT BETWEEN) *)

type dir = Asc | Desc
type sel_item = { e : expr; alias : string option }
type select_list = Star | Items of sel_item list

type query = {
  distinct : bool;
  sel : select_list;
  from : string;
  where : cond option;
  order_by : (expr * dir) list;
  limit : int option;
  offset : int option;
}

(* Libellé d'affichage d'une expression (nom de colonne de sortie par défaut). *)
let rec label_of_expr = function
  | Col c -> c
  | Lit v -> Value.to_display v
  | Func (name, _) -> name ^ "(…)"
  | Neg e -> "-" ^ label_of_expr e
  | Arith (op, a, b) ->
    let s = match op with Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%" in
    label_of_expr a ^ " " ^ s ^ " " ^ label_of_expr b
