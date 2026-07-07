(* typecheck.ml — Vérificateur de types : AST (permissif) -> cœur typé | erreur.
   C'est ici, et SEULEMENT ici, qu'une erreur de type peut naître. Une fois passé,
   l'évaluation est totale par construction. Les messages sont pensés pédagogiques. *)

open Typed
exception Type_error of string

(* Classe de type d'un type SQL déclaré dans le schéma. *)
let ty_of_sqltype (s : string) : any_ty =
  match String.uppercase_ascii s with
  | "INTEGER" | "BIGINT" | "SMALLINT" | "DECIMAL" | "NUMERIC" | "DOUBLE" | "REAL" | "FLOAT" -> AnyTy TNum
  | "BOOLEAN" | "BOOL" -> AnyTy TBool
  | _ -> AnyTy TText   (* VARCHAR, TEXT, CHAR, UUID, DATE, TIMESTAMP *)

let sqltype_of : type a. a ty -> string = function
  | TNum -> "DECIMAL" | TText -> "TEXT" | TBool -> "BOOLEAN"

let lookup_col cols name =
  match Db.find_col cols name with
  | Some c -> Some (ty_of_sqltype c.Db.cty)
  | None -> None

(* ---- Typage des expressions ---- *)
let rec tc_expr (cols : Db.column list) (expected : any_ty option) (e : Ast.expr) : packed =
  match e with
  | Ast.Lit Value.VNull ->
    (match expected with Some (AnyTy t) -> Pack (t, TLit (None, t)) | None -> Pack (TText, TLit (None, TText)))
  | Ast.Lit (Value.VStr s) -> Pack (TText, TLit (Some s, TText))
  | Ast.Lit (Value.VBool b) -> Pack (TBool, TLit (Some b, TBool))
  | Ast.Lit v ->
    let f = match Value.as_num v with Some f -> f | None -> 0. in
    Pack (TNum, TLit (Some f, TNum))
  | Ast.Col name ->
    (match lookup_col cols name with
     | Some (AnyTy t) -> Pack (t, TCol (name, t))
     | None -> raise (Type_error (Printf.sprintf "colonne inconnue : « %s »" name)))
  | Ast.Neg e1 -> Pack (TNum, TNeg (expect_num cols e1))
  | Ast.Arith (op, a, b) -> Pack (TNum, TArith (op, expect_num cols a, expect_num cols b))
  | Ast.Func (name, args) -> tc_func cols name args

and expect_num cols e : float texpr =
  match tc_expr cols (Some (AnyTy TNum)) e with
  | Pack (TNum, te) -> te
  | Pack (t, _) -> raise (Type_error (Printf.sprintf "un nombre est attendu ici (type trouvé : %s)" (ty_name t)))

and expect_text cols e : string texpr =
  match tc_expr cols (Some (AnyTy TText)) e with
  | Pack (TText, te) -> te
  | Pack (t, _) -> raise (Type_error (Printf.sprintf "un texte est attendu ici (type trouvé : %s)" (ty_name t)))

(* Type l'expression [e] en la forçant au type [t] (échoue si incompatible). *)
and coerce_to : type a. Db.column list -> a ty -> Ast.expr -> a texpr =
 fun cols t e ->
  match tc_expr cols (Some (AnyTy t)) e with
  | Pack (t2, e2) ->
    (match ty_eq t t2 with
     | Some Refl -> e2
     | None -> raise (Type_error (Printf.sprintf "types incompatibles : %s attendu mais %s trouvé" (ty_name t) (ty_name t2))))

and tc_func cols name args =
  match name, args with
  | ("upper" | "lower" | "trim"), [ a ] ->
    let te = expect_text cols a in
    Pack (TText, (match name with "upper" -> TUpper te | "lower" -> TLower te | _ -> TTrim te))
  | "length", [ a ] -> Pack (TNum, TLength (expect_text cols a))
  | "abs", [ a ] -> Pack (TNum, TAbs (expect_num cols a))
  | "round", [ a ] -> Pack (TNum, TRound (expect_num cols a, 0))
  | "round", [ a; Ast.Lit (Value.VInt d) ] -> Pack (TNum, TRound (expect_num cols a, d))
  | "coalesce", a0 :: rest ->
    (match tc_expr cols None a0 with
     | Pack (t, e0) -> Pack (t, TCoalesce (e0 :: List.map (fun a -> coerce_to cols t a) rest, t)))
  | _ -> raise (Type_error (Printf.sprintf "fonction inconnue ou mal utilisée : « %s » avec %d argument(s)" name (List.length args)))

(* ---- Typage des conditions ---- *)
let rec tc_cond cols (c : Ast.cond) : tcond =
  match c with
  | Ast.Cmp (op, a, b) ->
    (match tc_expr cols None a with Pack (t, ea) -> TCmp (op, ea, coerce_to cols t b))
  | Ast.And (a, b) -> TAnd (tc_cond cols a, tc_cond cols b)
  | Ast.Or (a, b) -> TOr (tc_cond cols a, tc_cond cols b)
  | Ast.Not a -> TNot (tc_cond cols a)
  | Ast.IsNull e -> (match tc_expr cols None e with Pack (_, ee) -> TIsNull ee)
  | Ast.IsNotNull e -> (match tc_expr cols None e with Pack (_, ee) -> TIsNotNull ee)
  | Ast.In (e, items, neg) ->
    (match tc_expr cols None e with
     | Pack (t, ee) -> TIn (ee, List.map (fun it -> coerce_to cols t it) items, neg))
  | Ast.Like (e, p, neg) -> TLike (expect_text cols e, expect_text cols p, neg)
  | Ast.Between (e, lo, hi, neg) ->
    (match tc_expr cols None e with
     | Pack (t, ee) -> TBetween (ee, coerce_to cols t lo, coerce_to cols t hi, neg))

(* ---- Requête typée ---- *)
type tselect = TStar | TItems of (string * packed) list

type tquery = {
  distinct : bool;
  from : Db.table;
  where : tcond option;
  select : tselect;
  order_by : (packed * Ast.dir) list;
  limit : int option;
  offset : int option;
}

(* Noms de colonnes de sortie uniques. *)
let output_names items =
  let used = Hashtbl.create 8 in
  List.mapi (fun i it ->
    let base = match it.Ast.alias with Some a -> a | None -> Ast.label_of_expr it.Ast.e in
    let name = if Hashtbl.mem used base then base ^ "_" ^ string_of_int i else base in
    Hashtbl.replace used name (); name) items

let check (q : Ast.query) (db : Db.db) : tquery =
  match Db.find_table db q.from with
  | None -> raise (Type_error (Printf.sprintf "table inconnue : « %s »" q.from))
  | Some tbl ->
    let cols = tbl.Db.cols in
    let where = Option.map (tc_cond cols) q.where in
    let select =
      match q.sel with
      | Ast.Star -> TStar
      | Ast.Items items ->
        let names = output_names items in
        TItems (List.map2 (fun n it -> (n, tc_expr cols None it.Ast.e)) names items)
    in
    (* ORDER BY peut référencer les colonnes de la table ET les alias du SELECT. *)
    let order_cols =
      match select with
      | TStar -> cols
      | TItems items -> cols @ List.map (fun (n, Pack (t, _)) -> { Db.cname = n; cty = sqltype_of t }) items
    in
    let order_by = List.map (fun (e, d) -> (tc_expr order_cols None e, d)) q.order_by in
    { distinct = q.distinct; from = tbl; where; select; order_by; limit = q.limit; offset = q.offset }
