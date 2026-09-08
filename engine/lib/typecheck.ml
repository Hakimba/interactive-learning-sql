(* typecheck.ml — Vérificateur de types : AST (permissif) -> cœur typé | erreur.
   Multi-tables : construit un environnement de champs combiné (alias.col + type),
   résout les colonnes qualifiées (a.x) et non qualifiées (x, si non ambiguë).
   C'est ici, et SEULEMENT ici, qu'une erreur de type peut naître. *)

open Typed
exception Type_error of string

let ty_of_sqltype (s : string) : any_ty =
  match String.uppercase_ascii s with
  | "INTEGER" | "BIGINT" | "SMALLINT" | "DECIMAL" | "NUMERIC" | "DOUBLE" | "REAL" | "FLOAT" -> AnyTy TNum
  | "BOOLEAN" | "BOOL" -> AnyTy TBool
  | _ -> AnyTy TText

let sqltype_of : type a. a ty -> string = function
  | TNum -> "DECIMAL" | TText -> "TEXT" | TBool -> "BOOLEAN"

(* ---- Sources & environnement de champs ---- *)
type tsource = { alias : string; table : Db.table }
type field = { fkey : string; falias : string; fname : string; fty : any_ty }

let key_of alias col = alias ^ "." ^ col

let fields_of (s : tsource) : field list =
  List.map (fun c ->
    { fkey = key_of s.alias c.Db.cname; falias = s.alias; fname = c.Db.cname; fty = ty_of_sqltype c.Db.cty })
    s.table.Db.cols

let lc = String.lowercase_ascii

(* Résolution d'une référence de colonne -> (clé canonique, type). *)
let resolve (env : field list) (q : string option) (name : string) : string * any_ty =
  let matches =
    List.filter (fun f -> lc f.fname = lc name && (match q with None -> true | Some a -> lc f.falias = lc a)) env
  in
  match matches with
  | [ f ] -> (f.fkey, f.fty)
  | [] -> raise (Type_error (Printf.sprintf "colonne inconnue : « %s »" (match q with Some a -> a ^ "." ^ name | None -> name)))
  | _ -> raise (Type_error (Printf.sprintf "colonne ambiguë : « %s » — préfixe par la table (ex. t.%s)" name name))

(* ---- Typage des expressions ---- *)
let rec tc_expr (env : field list) (expected : any_ty option) (e : Ast.expr) : packed =
  match e with
  | Ast.Lit Value.VNull ->
    (match expected with Some (AnyTy t) -> Pack (t, TLit (None, t)) | None -> Pack (TText, TLit (None, TText)))
  | Ast.Lit (Value.VStr s) -> Pack (TText, TLit (Some s, TText))
  | Ast.Lit (Value.VBool b) -> Pack (TBool, TLit (Some b, TBool))
  | Ast.Lit v ->
    let f = match Value.as_num v with Some f -> f | None -> 0. in
    Pack (TNum, TLit (Some f, TNum))
  | Ast.Col (q, name) ->
    let key, AnyTy t = resolve env q name in
    Pack (t, TCol (key, t))
  | Ast.Neg e1 -> Pack (TNum, TNeg (expect_num env e1))
  | Ast.Arith (op, a, b) -> Pack (TNum, TArith (op, expect_num env a, expect_num env b))
  | Ast.Func (name, args) -> tc_func env name args

and expect_num env e : float texpr =
  match tc_expr env (Some (AnyTy TNum)) e with
  | Pack (TNum, te) -> te
  | Pack (t, _) -> raise (Type_error (Printf.sprintf "un nombre est attendu ici (type trouvé : %s)" (ty_name t)))

and expect_text env e : string texpr =
  match tc_expr env (Some (AnyTy TText)) e with
  | Pack (TText, te) -> te
  | Pack (t, _) -> raise (Type_error (Printf.sprintf "un texte est attendu ici (type trouvé : %s)" (ty_name t)))

and coerce_to : type a. field list -> a ty -> Ast.expr -> a texpr =
 fun env t e ->
  match tc_expr env (Some (AnyTy t)) e with
  | Pack (t2, e2) ->
    (match ty_eq t t2 with
     | Some Refl -> e2
     | None -> raise (Type_error (Printf.sprintf "types incompatibles : %s attendu mais %s trouvé" (ty_name t) (ty_name t2))))

and tc_func env name args =
  match name, args with
  | ("upper" | "lower" | "trim"), [ a ] ->
    let te = expect_text env a in
    Pack (TText, (match name with "upper" -> TUpper te | "lower" -> TLower te | _ -> TTrim te))
  | "length", [ a ] -> Pack (TNum, TLength (expect_text env a))
  | "abs", [ a ] -> Pack (TNum, TAbs (expect_num env a))
  | "round", [ a ] -> Pack (TNum, TRound (expect_num env a, 0))
  | "round", [ a; Ast.Lit (Value.VInt d) ] -> Pack (TNum, TRound (expect_num env a, d))
  | "coalesce", a0 :: rest ->
    (match tc_expr env None a0 with
     | Pack (t, e0) -> Pack (t, TCoalesce (e0 :: List.map (fun a -> coerce_to env t a) rest, t)))
  | _ -> raise (Type_error (Printf.sprintf "fonction inconnue ou mal utilisée : « %s » avec %d argument(s)" name (List.length args)))

(* ---- Typage des conditions ---- *)
let rec tc_cond env (c : Ast.cond) : tcond =
  match c with
  | Ast.Cmp (op, a, b) -> (match tc_expr env None a with Pack (t, ea) -> TCmp (op, ea, coerce_to env t b))
  | Ast.And (a, b) -> TAnd (tc_cond env a, tc_cond env b)
  | Ast.Or (a, b) -> TOr (tc_cond env a, tc_cond env b)
  | Ast.Not a -> TNot (tc_cond env a)
  | Ast.IsNull e -> (match tc_expr env None e with Pack (_, ee) -> TIsNull ee)
  | Ast.IsNotNull e -> (match tc_expr env None e with Pack (_, ee) -> TIsNotNull ee)
  | Ast.In (e, items, neg) ->
    (match tc_expr env None e with Pack (t, ee) -> TIn (ee, List.map (fun it -> coerce_to env t it) items, neg))
  | Ast.Like (e, p, neg) -> TLike (expect_text env e, expect_text env p, neg)
  | Ast.Between (e, lo, hi, neg) ->
    (match tc_expr env None e with Pack (t, ee) -> TBetween (ee, coerce_to env t lo, coerce_to env t hi, neg))

(* ---- Requête typée ---- *)
type tjoin = { src : tsource; kind : Ast.join_kind; on : tcond option }
type tselect = TStar | TItems of (string * packed) list

type tquery = {
  distinct : bool;
  base : tsource;
  joins : tjoin list;
  where : tcond option;
  select : tselect;
  order_by : (packed * Ast.dir) list;
  limit : int option;
  offset : int option;
}

let output_names items =
  let used = Hashtbl.create 8 in
  List.mapi (fun i it ->
    let base = match it.Ast.alias with Some a -> a | None -> Ast.label_of_expr it.Ast.e in
    let name = if Hashtbl.mem used base then base ^ "_" ^ string_of_int i else base in
    Hashtbl.replace used name (); name) items

let resolve_source db name alias_opt : tsource =
  match Db.find_table db name with
  | None -> raise (Type_error (Printf.sprintf "table inconnue : « %s »" name))
  | Some t -> { alias = (match alias_opt with Some a -> a | None -> t.Db.tname); table = t }

(* ---- DDL des index ----
   Vérifie table/colonnes/nom ; l'index lui-même est construit par la couche physique.
   Espace de noms partagé entre index de toutes les tables et tables (comme Postgres). *)
type ddl_result = DdlCreate of Db.table * Db.index_def | DdlDrop of Db.table * Db.index_def

let all_indexes (db : Db.db) = List.concat_map (fun t -> List.map (fun i -> (t, i)) t.Db.indexes) db.Db.tables

let check_ddl (d : Ast.ddl) (db : Db.db) : (ddl_result, string) result =
  match d with
  | Ast.CreateIndex { iname; itable; icols; iunique } ->
    (match Db.find_table db itable with
     | None -> Error (Printf.sprintf "table inconnue : « %s »" itable)
     | Some t ->
       let rec resolve acc = function
         | [] -> Ok (List.rev acc)
         | c :: rest ->
           (match List.find_opt (fun col -> lc col.Db.cname = lc c) t.Db.cols with
            | None -> Error (Printf.sprintf "colonne inconnue : « %s » dans « %s »" c t.Db.tname)
            | Some col ->
              if List.exists (fun x -> lc x = lc col.Db.cname) acc
              then Error (Printf.sprintf "colonne « %s » répétée" col.Db.cname)
              else resolve (col.Db.cname :: acc) rest)
       in
       (match resolve [] icols with
        | Error e -> Error e
        | Ok [] -> Error "au moins une colonne est attendue"
        | Ok cols ->
          if List.exists (fun (_, i) -> lc i.Db.iname = lc iname) (all_indexes db)
          then Error (Printf.sprintf "« %s » existe déjà (nom d'index déjà utilisé)" iname)
          else if List.exists (fun t -> lc t.Db.tname = lc iname) db.Db.tables
          then Error (Printf.sprintf "« %s » est déjà le nom d'une table" iname)
          else Ok (DdlCreate (t, { Db.iname; icols = cols; iunique; ienabled = true; iimplicit = false }))))
  | Ast.DropIndex name ->
    (match List.find_opt (fun (_, i) -> lc i.Db.iname = lc name) (all_indexes db) with
     | None -> Error (Printf.sprintf "index inconnu : « %s »" name)
     | Some (t, i) ->
       if i.Db.iimplicit
       then Error (Printf.sprintf "« %s » est l'index implicite de la clé primaire : décoche PK sur la colonne" name)
       else Ok (DdlDrop (t, i)))

let check (q : Ast.query) (db : Db.db) : tquery =
  let base = resolve_source db q.from q.from_alias in
  let jsrcs = List.map (fun (j : Ast.join_clause) -> (resolve_source db j.jtable j.jalias, j)) q.joins in
  (* environnement combiné : base + toutes les tables jointes *)
  let env = fields_of base @ List.concat_map (fun (s, _) -> fields_of s) jsrcs in
  let joins = List.map (fun (s, (j : Ast.join_clause)) ->
    { src = s; kind = j.jkind; on = Option.map (tc_cond env) j.jon }) jsrcs in
  let where = Option.map (tc_cond env) q.where in
  let select =
    match q.sel with
    | Ast.Star -> TStar
    | Ast.Items items ->
      let names = output_names items in
      TItems (List.map2 (fun n it -> (n, tc_expr env None it.Ast.e)) names items)
  in
  (* ORDER BY peut référencer les alias du SELECT (en plus des colonnes). *)
  let order_env =
    match select with
    | TStar -> env
    | TItems items ->
      (* on ajoute les alias du SELECT, mais seulement ceux qui n'ombrent pas une colonne
         existante (sinon « ORDER BY id » deviendrait ambigu entre la colonne et l'alias). *)
      let existing = List.map (fun f -> lc f.fname) env in
      let alias_fields =
        List.filter_map (fun (n, Pack (t, _)) ->
          if List.mem (lc n) existing then None else Some { fkey = n; falias = ""; fname = n; fty = AnyTy t }) items
      in
      env @ alias_fields
  in
  let order_by = List.map (fun (e, d) -> (tc_expr order_env None e, d)) q.order_by in
  { distinct = q.distinct; base; joins; where; select; order_by; limit = q.limit; offset = q.offset }
