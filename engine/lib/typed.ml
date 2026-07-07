(* typed.ml — Cœur TYPÉ (GADT) + évaluateur total.
   Après le typechecker, une requête est représentée par ces valeurs typées :
   le paramètre de type d'un [texpr] REFLÈTE son type SQL. L'évaluateur est
   dirigé par les types → aucune branche "impossible", pas de confusion de types.
   NB : la correction *sémantique* (3VL, sacs, WHERE) vient de l'encodage fidèle
   de Guagliardo & Libkin, pas du GADT — le GADT garantit la cohérence des types. *)

open Value

(* Témoins de type SQL (classes de types agnostiques de l'incrément 1). *)
type _ ty =
  | TNum : float ty       (* INTEGER, BIGINT, DECIMAL, DOUBLE... *)
  | TText : string ty     (* VARCHAR, TEXT, UUID, DATE, TIMESTAMP *)
  | TBool : bool ty       (* BOOLEAN *)

type any_ty = AnyTy : 'a ty -> any_ty

(* Égalité de types (renvoie une preuve Refl si égaux). *)
type (_, _) eq = Refl : ('a, 'a) eq
let ty_eq : type a b. a ty -> b ty -> (a, b) eq option =
 fun a b ->
  match a, b with
  | TNum, TNum -> Some Refl
  | TText, TText -> Some Refl
  | TBool, TBool -> Some Refl
  | _ -> None

let ty_name : type a. a ty -> string = function
  | TNum -> "nombre" | TText -> "texte" | TBool -> "booléen"

(* Expressions typées. NULL est représenté par [None] lors de l'évaluation. *)
type _ texpr =
  | TLit : 'a option * 'a ty -> 'a texpr
  | TCol : string * 'a ty -> 'a texpr
  | TNeg : float texpr -> float texpr
  | TArith : Ast.arith * float texpr * float texpr -> float texpr
  | TUpper : string texpr -> string texpr
  | TLower : string texpr -> string texpr
  | TTrim : string texpr -> string texpr
  | TLength : string texpr -> float texpr
  | TAbs : float texpr -> float texpr
  | TRound : float texpr * int -> float texpr
  | TCoalesce : 'a texpr list * 'a ty -> 'a texpr

(* Conditions typées, évaluées dans la logique à 3 valeurs. *)
type tcond =
  | TCmp : Ast.cmp * 'a texpr * 'a texpr -> tcond      (* mêmes types des deux côtés *)
  | TAnd of tcond * tcond
  | TOr of tcond * tcond
  | TNot of tcond
  | TIsNull : 'a texpr -> tcond
  | TIsNotNull : 'a texpr -> tcond
  | TIn : 'a texpr * 'a texpr list * bool -> tcond
  | TLike of string texpr * string texpr * bool
  | TBetween : 'a texpr * 'a texpr * 'a texpr * bool -> tcond

(* Expression typée dont le type est caché (produit par le typechecker). *)
type packed = Pack : 'a ty * 'a texpr -> packed

(* ---- Lecture d'une colonne : seul point de contact avec les données brutes ---- *)
let read_col : type a. Db.row -> string -> a ty -> a option =
 fun row name ty ->
  match List.assoc_opt name row with
  | None | Some VNull -> None
  | Some v ->
    (match ty with
     | TNum -> as_num v
     | TText -> Some (to_display v)
     | TBool -> (match v with VBool b -> Some b | _ -> (match as_num v with Some f -> Some (f <> 0.) | None -> None)))

let arith op x y =
  match op with
  | Ast.Add -> Some (x +. y)
  | Ast.Sub -> Some (x -. y)
  | Ast.Mul -> Some (x *. y)
  | Ast.Div -> if y = 0. then None else Some (x /. y)
  | Ast.Mod -> if y = 0. then None else Some (Float.rem x y)

(* Évaluateur total, dirigé par les types. Renvoie 'a option (None = NULL). *)
let rec eval : type a. Db.row -> a texpr -> a option =
 fun row e ->
  match e with
  | TLit (v, _) -> v
  | TCol (name, ty) -> read_col row name ty
  | TNeg e -> Option.map (fun x -> -.x) (eval row e)
  | TArith (op, a, b) ->
    (match eval row a, eval row b with Some x, Some y -> arith op x y | _ -> None)
  | TUpper e -> Option.map String.uppercase_ascii (eval row e)
  | TLower e -> Option.map String.lowercase_ascii (eval row e)
  | TTrim e -> Option.map String.trim (eval row e)
  | TLength e -> Option.map (fun s -> float_of_int (String.length s)) (eval row e)
  | TAbs e -> Option.map Float.abs (eval row e)
  | TRound (e, d) ->
    Option.map (fun x -> let f = 10. ** float_of_int d in Float.round (x *. f) /. f) (eval row e)
  | TCoalesce (es, _) -> List.find_map (eval row) es

(* LIKE : % = suite quelconque, _ = un caractère. *)
let like_match pat s =
  let np = String.length pat and ns = String.length s in
  let rec go pi si =
    if pi = np then si = ns
    else
      match pat.[pi] with
      | '%' -> go (pi + 1) si || (si < ns && go pi (si + 1))
      | '_' -> si < ns && go (pi + 1) (si + 1)
      | c -> si < ns && s.[si] = c && go (pi + 1) (si + 1)
  in
  go 0 0

let cmp_bool op c =
  match op with
  | Ast.Eq -> c = 0 | Ast.Neq -> c <> 0 | Ast.Lt -> c < 0
  | Ast.Le -> c <= 0 | Ast.Gt -> c > 0 | Ast.Ge -> c >= 0

(* Évaluation d'une condition en 3VL. Les deux côtés d'une comparaison ont le
   MÊME type OCaml (garanti par le GADT) → [compare] polymorphe est bien défini. *)
let rec eval_cond (row : Db.row) (c : tcond) : tv =
  match c with
  | TCmp (op, l, r) ->
    (match eval row l, eval row r with
     | Some a, Some b -> tv_of_bool (cmp_bool op (compare a b))
     | _ -> Unknown)
  | TAnd (a, b) -> tv_and (eval_cond row a) (eval_cond row b)
  | TOr (a, b) -> tv_or (eval_cond row a) (eval_cond row b)
  | TNot a -> tv_not (eval_cond row a)
  | TIsNull e -> tv_of_bool (eval row e = None)
  | TIsNotNull e -> tv_of_bool (eval row e <> None)
  | TIn (e, items, neg) ->
    let base =
      match eval row e with
      | None -> Unknown
      | Some v ->
        let matched = ref false and saw_null = ref false in
        List.iter (fun it -> match eval row it with None -> saw_null := true | Some iv -> if compare v iv = 0 then matched := true) items;
        if !matched then True else if !saw_null then Unknown else False
    in
    if neg then tv_not base else base
  | TLike (e, p, neg) ->
    (match eval row e, eval row p with
     | Some s, Some pat -> let m = like_match pat s in tv_of_bool (if neg then not m else m)
     | _ -> Unknown)
  | TBetween (e, lo, hi, neg) ->
    (match eval row e, eval row lo, eval row hi with
     | Some v, Some l, Some h ->
       let base = tv_and (tv_of_bool (compare v l >= 0)) (tv_of_bool (compare v h <= 0)) in
       if neg then tv_not base else base
     | _ -> Unknown)

(* Convertit une valeur typée en [value] (pour l'affichage / la trace). *)
let wrap : type a. a ty -> a option -> value =
 fun ty v ->
  match v with
  | None -> VNull
  | Some x ->
    (match ty with
     | TNum -> if Float.is_integer x && Float.abs x < 1e15 then VInt (int_of_float x) else VFloat x
     | TText -> VStr x
     | TBool -> VBool x)

let eval_to_value (row : Db.row) (p : packed) : value =
  match p with Pack (ty, e) -> wrap ty (eval row e)
