(* physical.ml — Couche PHYSIQUE : chemins d'accès, index B-tree, modèle de coût, compteurs EXACTS.

   Un index ne change JAMAIS le résultat d'une requête : la sémantique (semantics.ml, Guagliardo &
   Libkin) reste l'autorité. Cette couche choisit seulement QUELLES lignes lui donner et dans QUEL
   ordre, et compte ce qu'elle a réellement lu. La propriété fondamentale, vérifiée à chaque exécution
   ([check]) et par QCheck : résultat(chemin physique) ≡ résultat(sémantique).

   SOURCES (rien d'ad hoc — cf. About) :
   - Selinger, Astrahan, Chamberlin, Lorie, Price, « Access Path Selection in a Relational Database
     Management System », SIGMOD 1979 : coût = accès pages + W·CPU ; facteurs de sélectivité
     (col = v → 1/ICARD si index, sinon 1/10 ; col > v → (max−v)/(max−min) si index, sinon 1/3 ;
     BETWEEN → 1/4 ; AND produit ; OR F1+F2−F1·F2 ; NOT 1−F) ; prédicats « sargables » ;
     un index qui fournit l'ordre demandé (« interesting order ») évite le tri.
   - Comer, « The Ubiquitous B-Tree », ACM Computing Surveys 11(2), 1979 : structure de l'arbre,
     descente racine → feuille, hauteur ≈ ⌈log_F n⌉.
   - PostgreSQL, doc. chap. 11 : 11.2 (B-tree : < <= = >= >, BETWEEN, IN, IS NULL ; LIKE seulement en
     préfixe/collation C), 11.3 (index multicolonne : règle du préfixe gauche), 11.4 (ordre fourni par
     un B-tree, parcours arrière pour DESC, ORDER BY … LIMIT s'arrête tôt), 11.9 (Index Only Scan si
     toutes les colonnes lues sont dans l'index) ; chap. 14.1 (noms des nœuds Seq Scan / Index Scan /
     Index Only Scan / Sort ; coût = pages × seq_page_cost + lignes × cpu_tuple_cost) ; chap. 19.7
     (constantes : seq_page_cost 1.0, random_page_cost 4.0, cpu_tuple_cost 0.01,
     cpu_index_tuple_cost 0.005, cpu_operator_cost 0.0025 ; enable_seqscan = « forcer »).
   - Sélectivité par défaut d'un LIKE : DEFAULT_MATCH_SEL = 0.005 (PostgreSQL, selfuncs.h).
   Le MODÈLE de coût (rows_per_page = 4, fanout = 4, forme simplifiée) est annoncé comme tel dans
   l'app ; les compteurs (lignes lues, pages, entrées d'index) sont EXACTS.
   NULL : en TÊTE de l'index (même convention que l'ORDER BY de semantics.ml — SQLite) ; une
   comparaison ne trouve jamais NULL (3VL), IS NULL cherche le bloc NULL (PG 11.2). *)

open Value
module T = Typecheck

(* ------------------------------------------------------------------ *)
(* Constantes et options du modèle                                     *)
(* ------------------------------------------------------------------ *)
type consts = {
  seq_page_cost : float; random_page_cost : float; cpu_tuple_cost : float;
  cpu_index_tuple_cost : float; cpu_operator_cost : float;
  rows_per_page : int;   (* lignes par page du tas (paramètre du modèle, visible dès 8 lignes) *)
  fanout : int;          (* fan-out du B-tree affiché *)
}

let default_consts = {
  seq_page_cost = 1.0; random_page_cost = 4.0; cpu_tuple_cost = 0.01;
  cpu_index_tuple_cost = 0.005; cpu_operator_cost = 0.0025; rows_per_page = 4; fanout = 4;
}

type options = {
  with_plan : bool;
  force : string option;      (* chemin forcé : "seq_scan" ou nom d'index (≈ enable_seqscan) *)
  consts : consts;
  scale : int option;         (* échelle SIMULÉE : « et si la table avait N lignes » ; la table réelle sert d'échantillon *)
}
let default_options = { with_plan = true; force = None; consts = default_consts; scale = None }

(* ------------------------------------------------------------------ *)
(* Clés : comparaison NULL d'abord, puis Value.compare_nonnull         *)
(* ------------------------------------------------------------------ *)
let compare_value a b =
  match a, b with
  | VNull, VNull -> 0 | VNull, _ -> -1 | _, VNull -> 1
  | _ -> compare_nonnull a b

let rec compare_key ka kb =
  match ka, kb with
  | [], [] -> 0 | [], _ -> -1 | _, [] -> 1
  | a :: ra, b :: rb -> let c = compare_value a b in if c <> 0 then c else compare_key ra rb

let lc = String.lowercase_ascii
let same_col a b = lc a = lc b
let col_of_key k =
  match String.rindex_opt k '.' with
  | Some i -> String.sub k (i + 1) (String.length k - i - 1)
  | None -> k

(* Valeur NORMALISÉE d'une colonne : passe par le même lecteur typé que le WHERE ([Typed.read_col]),
   donc l'index « voit » exactement ce que voit la condition (ex. une chaîne dans une colonne
   numérique devient NULL des deux côtés). Évite aussi les comparaisons entre types mixtes. *)
let norm_value (r : Db.row) (cname : string) (aty : Typed.any_ty) : value =
  match aty with Typed.AnyTy ty -> Typed.wrap ty (Typed.read_col r cname ty)

(* ------------------------------------------------------------------ *)
(* Index construit : entrées triées (clé, rowid)                        *)
(* ------------------------------------------------------------------ *)
type entry = { key : value list; rowid : int }   (* rowid = position dans table.rows *)

type built = {
  def : Db.index_def;
  cols : (string * Typed.any_ty) list;   (* noms canoniques + types *)
  entries : entry array;                  (* triées par (clé, rowid), NULL d'abord *)
  unique_violations : value list list;    (* clés (sans NULL) présentes plusieurs fois *)
}

let build_index (t : Db.table) (def : Db.index_def) : (built, string) result =
  let resolve c = List.find_opt (fun col -> same_col col.Db.cname c) t.Db.cols in
  match List.find_opt (fun c -> resolve c = None) def.Db.icols with
  | Some c -> Error (Printf.sprintf "index « %s » : colonne inconnue « %s »" def.Db.iname c)
  | None ->
    if def.Db.icols = [] then Error (Printf.sprintf "index « %s » : aucune colonne" def.Db.iname)
    else begin
      let cols = List.filter_map (fun c ->
        Option.map (fun col -> (col.Db.cname, T.ty_of_sqltype col.Db.cty)) (resolve c)) def.Db.icols in
      let key_of r = List.map (fun (cn, aty) -> norm_value r cn aty) cols in
      let entries = Array.of_list (List.mapi (fun i r -> { key = key_of r; rowid = i }) t.Db.rows) in
      Array.stable_sort (fun a b -> let c = compare_key a.key b.key in if c <> 0 then c else compare a.rowid b.rowid) entries;
      (* unicité : clés égales SANS composante NULL (NULL ≠ NULL, comme Postgres) *)
      let viol = ref [] in
      Array.iteri (fun i e ->
        if i > 0 && compare_key entries.(i - 1).key e.key = 0 && not (List.exists is_null e.key)
           && not (List.exists (fun k -> compare_key k e.key = 0) !viol)
        then viol := e.key :: !viol) entries;
      Ok { def; cols; entries; unique_violations = List.rev !viol }
    end

(* ------------------------------------------------------------------ *)
(* B-tree (Comer 1979) : feuilles = tranches de [fanout] entrées,       *)
(* nœuds internes = séparateurs (première clé de chaque enfant sauf 1). *)
(* ------------------------------------------------------------------ *)
type node = { id : int; level : int; first : int; last : int;   (* couvre les entrées [first, last) *)
              seps : value list list; children : int list }
type btree = { fanout : int; height : int; root : int; nodes : node array }

let rec chunk k = function
  | [] -> []
  | l -> let rec take i acc = function
           | x :: r when i < k -> take (i + 1) (x :: acc) r
           | rest -> (List.rev acc, rest) in
         let (g, rest) = take 0 [] l in g :: chunk k rest

let build_tree ~fanout (entries : entry array) : btree =
  let f = max 2 fanout and n = Array.length entries in
  let acc = ref [] and next = ref 0 in
  let mk level first last seps children =
    let id = !next in incr next;
    acc := { id; level; first; last; seps; children } :: !acc; id in
  let nleaves = max 1 ((n + f - 1) / f) in
  let leaves = List.init nleaves (fun i -> mk 0 (i * f) (min n ((i + 1) * f)) [] []) in
  let find id = List.find (fun nd -> nd.id = id) !acc in
  let rec up level ids =
    match ids with
    | [ root ] -> root
    | _ ->
      let parents = List.map (fun g ->
        let first = (find (List.hd g)).first and last = (find (List.nth g (List.length g - 1))).last in
        let seps = List.filteri (fun i _ -> i > 0) g
                   |> List.map (fun id -> let nd = find id in if nd.first < n then entries.(nd.first).key else []) in
        mk level first last seps g) (chunk f ids) in
      up (level + 1) parents
  in
  let root = up 1 leaves in
  let nodes = Array.of_list (List.rev !acc) in   (* ids séquentiels = positions *)
  { fanout = f; height = nodes.(root).level + 1; root; nodes }

(* Descente racine → feuille contenant la position [pos] (bornée). *)
let path_to (t : btree) (pos : int) : int list =
  let rec go nd acc =
    let acc = nd.id :: acc in
    match nd.children with
    | [] -> List.rev acc
    | ch ->
      let p = max 0 (min pos (nd.last - 1)) in
      let child = match List.find_opt (fun id -> let c = t.nodes.(id) in c.first <= p && p < c.last) ch with
        | Some id -> id | None -> List.nth ch (List.length ch - 1) in
      go t.nodes.(child) acc
  in
  go t.nodes.(t.root) []

(* ------------------------------------------------------------------ *)
(* Bornes et sondes                                                     *)
(* ------------------------------------------------------------------ *)
type bound = Incl of value | Excl of value | Unbounded
type probe = { prefix : value list; lo : bound; hi : bound }   (* égalités de tête + intervalle sur la colonne suivante *)

(* premier i tel que f a.(i) (f monotone faux → vrai) ; n si aucun *)
let bisect (a : 'a array) (f : 'a -> bool) : int =
  let lo = ref 0 and hi = ref (Array.length a) in
  while !lo < !hi do let mid = (!lo + !hi) / 2 in if f a.(mid) then hi := mid else lo := mid + 1 done;
  !lo

let cmp_prefix key prefix =
  let rec go k p = match k, p with
    | _, [] -> 0 | [], _ -> -1
    | a :: ka, b :: kb -> let c = compare_value a b in if c <> 0 then c else go ka kb in
  go key prefix

(* Intervalle [lo, hi) des positions d'entrées couvertes par une sonde (recherche binaire × 2). *)
let range_of (b : built) (p : probe) : int * int =
  let j = List.length p.prefix in
  let at_lo e =
    let c = cmp_prefix e.key p.prefix in
    c > 0 || (c = 0 && (match p.lo, List.nth_opt e.key j with
      | Unbounded, _ | _, None -> true
      | Incl v, Some x -> compare_value x v >= 0
      | Excl v, Some x -> compare_value x v > 0)) in
  let at_hi e =
    let c = cmp_prefix e.key p.prefix in
    c > 0 || (c = 0 && (match p.hi, List.nth_opt e.key j with
      | Unbounded, _ | _, None -> false
      | Incl v, Some x -> compare_value x v > 0
      | Excl v, Some x -> compare_value x v >= 0)) in
  let lo = bisect b.entries at_lo in
  let hi = bisect b.entries at_hi in
  (lo, max lo hi)

(* ------------------------------------------------------------------ *)
(* Classification des conjoints du WHERE (sargable ou non, et pourquoi) *)
(* ------------------------------------------------------------------ *)
type col_pred =
  | PEq of value | PIn of value list | PRange of bound * bound | PIsNull
  | PNever   (* comparaison avec NULL : jamais True → 0 candidat *)

type reason =
  | FuncOnCol | ArithOnCol | LikeLeadingWildcard | LikeCollation | NotEqual | Negation | NotIn
  | IsNotNull | OrAcrossCols | ColVsCol | NoColumn
  | NotIndexed                 (* colonne hors de cet index *)
  | NotLeading of string       (* colonne de l'index mais pas en tête de préfixe *)
  | AfterRange of string       (* colonne après la colonne d'intervalle *)

type classified = Sarg of string * col_pred | NotSarg of reason   (* string = nom de colonne *)

type conjunct = {
  text : string; cond : Typed.tcond; cls : classified;
  sel : float;   (* sélectivité ESTIMÉE (facteurs de Selinger) *)
  obs : float;   (* sélectivité OBSERVÉE sur l'échantillon (fraction exacte des lignes qui satisfont le conjoint) *)
}

let rec conjuncts = function Typed.TAnd (a, b) -> conjuncts a @ conjuncts b | c -> [ c ]

type side = SCol of string | SLit of value | SFunc | SArith

let rec has_col : type a. a Typed.texpr -> bool = fun e ->
  match e with
  | Typed.TCol _ -> true | Typed.TLit _ -> false
  | Typed.TNeg x -> has_col x | Typed.TAbs x -> has_col x | Typed.TLength x -> has_col x
  | Typed.TUpper x -> has_col x | Typed.TLower x -> has_col x | Typed.TTrim x -> has_col x
  | Typed.TRound (x, _) -> has_col x
  | Typed.TArith (_, a, b) -> has_col a || has_col b
  | Typed.TCoalesce (es, _) -> List.exists has_col es

let side : type a. a Typed.texpr -> side = fun e ->
  match e with
  | Typed.TCol (k, _) -> SCol (col_of_key k)
  | Typed.TLit (v, ty) -> SLit (Typed.wrap ty v)
  | Typed.TNeg _ -> if has_col e then SArith else SLit (Typed.wrap Typed.TNum (Typed.eval [] e))
  | Typed.TArith _ -> if has_col e then SArith else SLit (Typed.wrap Typed.TNum (Typed.eval [] e))
  | Typed.TLength _ -> if has_col e then SFunc else SLit (Typed.wrap Typed.TNum (Typed.eval [] e))
  | Typed.TAbs _ -> if has_col e then SFunc else SLit (Typed.wrap Typed.TNum (Typed.eval [] e))
  | Typed.TRound _ -> if has_col e then SFunc else SLit (Typed.wrap Typed.TNum (Typed.eval [] e))
  | Typed.TUpper _ -> if has_col e then SFunc else SLit (Typed.wrap Typed.TText (Typed.eval [] e))
  | Typed.TLower _ -> if has_col e then SFunc else SLit (Typed.wrap Typed.TText (Typed.eval [] e))
  | Typed.TTrim _ -> if has_col e then SFunc else SLit (Typed.wrap Typed.TText (Typed.eval [] e))
  | Typed.TCoalesce (_, ty) -> if has_col e then SFunc else SLit (Typed.wrap ty (Typed.eval [] e))

let reason_of_sides a b =
  match a, b with
  | SFunc, _ | _, SFunc -> FuncOnCol
  | SArith, _ | _, SArith -> ArithOnCol
  | SCol _, SCol _ -> ColVsCol
  | _ -> NoColumn

let flip = function Ast.Lt -> Ast.Gt | Ast.Gt -> Ast.Lt | Ast.Le -> Ast.Ge | Ast.Ge -> Ast.Le | op -> op

let cmp_pred col op v =
  if is_null v then Sarg (col, PNever)
  else match op with
    | Ast.Eq -> Sarg (col, PEq v)
    | Ast.Neq -> NotSarg NotEqual
    | Ast.Lt -> Sarg (col, PRange (Unbounded, Excl v))
    | Ast.Le -> Sarg (col, PRange (Unbounded, Incl v))
    | Ast.Gt -> Sarg (col, PRange (Excl v, Unbounded))
    | Ast.Ge -> Sarg (col, PRange (Incl v, Unbounded))

let rec or_leaves = function Typed.TOr (a, b) -> or_leaves a @ or_leaves b | c -> [ c ]

let eq_leaf = function
  | Typed.TCmp (Ast.Eq, l, r) ->
    (match side l, side r with SCol k, SLit v | SLit v, SCol k -> Some (k, v) | _ -> None)
  | _ -> None

let uniq_values vs = List.sort_uniq compare_value vs

let classify (c : Typed.tcond) : classified =
  match c with
  | Typed.TCmp (op, l, r) ->
    (match side l, side r with
     | SCol k, SLit v -> cmp_pred k op v
     | SLit v, SCol k -> cmp_pred k (flip op) v
     | a, b -> NotSarg (reason_of_sides a b))
  | Typed.TAnd _ -> NotSarg NoColumn   (* n'arrive pas : les conjoints sont aplatis avant *)
  | Typed.TOr _ ->
    (* a = 1 OR a = 2 (même colonne, égalités) ≡ a IN (1, 2) → sargable ; sinon OR non sargable *)
    (match List.map eq_leaf (or_leaves c) with
     | Some (k, _) :: _ as ls when List.for_all (function Some (k2, _) -> same_col k k2 | None -> false) ls ->
       let vs = List.filter_map (function Some (_, v) when not (is_null v) -> Some v | _ -> None) ls in
       Sarg (k, if vs = [] then PNever else PIn (uniq_values vs))
     | _ -> NotSarg OrAcrossCols)
  | Typed.TNot _ -> NotSarg Negation
  | Typed.TIsNull e -> (match side e with SCol k -> Sarg (k, PIsNull) | s -> NotSarg (reason_of_sides s (SLit VNull)))
  | Typed.TIsNotNull _ -> NotSarg IsNotNull
  | Typed.TIn (e, items, neg) ->
    if neg then NotSarg NotIn
    else (match side e with
      | SCol k ->
        let sides = List.map side items in
        (match List.find_opt (function SLit _ -> false | _ -> true) sides with
         | Some s -> NotSarg (reason_of_sides (SCol k) s)
         | None ->
           let vs = List.filter_map (function SLit v when not (is_null v) -> Some v | _ -> None) sides in
           Sarg (k, if vs = [] then PNever else PIn (uniq_values vs)))
      | s -> NotSarg (reason_of_sides s (SLit VNull)))
  | Typed.TLike (e, p, neg) ->
    if neg then NotSarg Negation
    else (match side e, side p with
      | SCol _, SLit (VStr pat) ->
        NotSarg (if pat <> "" && (pat.[0] = '%' || pat.[0] = '_') then LikeLeadingWildcard else LikeCollation)
      | SCol _, _ -> NotSarg NoColumn
      | s, _ -> NotSarg (reason_of_sides s (SLit VNull)))
  | Typed.TBetween (e, lo, hi, neg) ->
    if neg then NotSarg Negation
    else (match side e, side lo, side hi with
      | SCol k, SLit l, SLit h -> if is_null l || is_null h then Sarg (k, PNever) else Sarg (k, PRange (Incl l, Incl h))
      | SCol _, _, _ -> NotSarg NoColumn
      | s, _, _ -> NotSarg (reason_of_sides s (SLit VNull)))

(* ---- Impression (affichage des conjoints) ---- *)
let rec pe : type a. a Typed.texpr -> string = fun e ->
  match e with
  | Typed.TLit (None, _) -> "NULL"
  | Typed.TLit (Some v, ty) ->
    (match ty with
     | Typed.TNum -> to_display (Typed.wrap Typed.TNum (Some v))
     | Typed.TText -> "'" ^ v ^ "'"
     | Typed.TBool -> if v then "true" else "false")
  | Typed.TCol (k, _) -> col_of_key k
  | Typed.TNeg x -> "-" ^ pe x
  | Typed.TArith (op, a, b) ->
    let s = match op with Ast.Add -> "+" | Ast.Sub -> "-" | Ast.Mul -> "*" | Ast.Div -> "/" | Ast.Mod -> "%" in
    pe a ^ " " ^ s ^ " " ^ pe b
  | Typed.TUpper x -> "upper(" ^ pe x ^ ")"
  | Typed.TLower x -> "lower(" ^ pe x ^ ")"
  | Typed.TTrim x -> "trim(" ^ pe x ^ ")"
  | Typed.TLength x -> "length(" ^ pe x ^ ")"
  | Typed.TAbs x -> "abs(" ^ pe x ^ ")"
  | Typed.TRound (x, d) -> "round(" ^ pe x ^ (if d = 0 then "" else ", " ^ string_of_int d) ^ ")"
  | Typed.TCoalesce (es, _) -> "coalesce(" ^ String.concat ", " (List.map pe es) ^ ")"

let cmp_sym = function Ast.Eq -> "=" | Ast.Neq -> "<>" | Ast.Lt -> "<" | Ast.Le -> "<=" | Ast.Gt -> ">" | Ast.Ge -> ">="

let rec pc = function
  | Typed.TCmp (op, l, r) -> pe l ^ " " ^ cmp_sym op ^ " " ^ pe r
  | Typed.TAnd (a, b) -> pc a ^ " AND " ^ pc b
  | Typed.TOr (a, b) -> "(" ^ pc a ^ " OR " ^ pc b ^ ")"
  | Typed.TNot a -> "NOT " ^ pc a
  | Typed.TIsNull e -> pe e ^ " IS NULL"
  | Typed.TIsNotNull e -> pe e ^ " IS NOT NULL"
  | Typed.TIn (e, items, neg) -> pe e ^ (if neg then " NOT IN (" else " IN (") ^ String.concat ", " (List.map pe items) ^ ")"
  | Typed.TLike (e, p, neg) -> pe e ^ (if neg then " NOT LIKE " else " LIKE ") ^ pe p
  | Typed.TBetween (e, lo, hi, neg) -> pe e ^ (if neg then " NOT BETWEEN " else " BETWEEN ") ^ pe lo ^ " AND " ^ pe hi

(* ------------------------------------------------------------------ *)
(* Statistiques (NCARD, ICARD, min/max, NULL) calculées sur les données *)
(* ------------------------------------------------------------------ *)
type col_stat = { distinct : int; nulls : int; minmax : (float * float) option; indexed : bool }
type stats = { n : int; pages : int; col_stats : (string * col_stat) list }

let compute_stats (c : consts) (t : Db.table) (enabled : built list) : stats =
  let n = List.length t.Db.rows in
  let col_stats = List.map (fun col ->
    let aty = T.ty_of_sqltype col.Db.cty in
    let vals = List.map (fun r -> norm_value r col.Db.cname aty) t.Db.rows in
    let nonnull = List.filter (fun v -> not (is_null v)) vals in
    let distinct = List.length (uniq_values nonnull) in
    let nums = List.filter_map as_num nonnull in
    let minmax = match nums with [] -> None | x :: r -> Some (List.fold_left Float.min x r, List.fold_left Float.max x r) in
    let indexed = List.exists (fun b -> match b.def.Db.icols with c0 :: _ -> same_col c0 col.Db.cname | [] -> false) enabled in
    (col.Db.cname, { distinct; nulls = n - List.length nonnull; minmax; indexed })) t.Db.cols in
  { n; pages = (n + c.rows_per_page - 1) / c.rows_per_page; col_stats }

let stat_of (s : stats) col = List.find_opt (fun (c, _) -> same_col c col) s.col_stats |> Option.map snd

(* Facteurs de sélectivité (Selinger 1979 §4 ; « index » = ICARD / min / max connus). *)
let sel_of_pred (s : stats) col (p : col_pred) : float =
  let st = stat_of s col in
  let indexed, icard, minmax, nulls =
    match st with Some x -> (x.indexed, x.distinct, x.minmax, x.nulls) | None -> (false, 0, None, 0) in
  let eq = if indexed && icard > 0 then 1. /. float_of_int icard else 0.1 in
  let clamp f = Float.max 0. (Float.min 1. f) in
  match p with
  | PNever -> 0.
  | PEq _ -> eq
  | PIn vs -> Float.min 0.5 (float_of_int (List.length vs) *. eq)
  | PIsNull -> if s.n = 0 then 0. else float_of_int nulls /. float_of_int s.n
  | PRange (lo, hi) ->
    let num = function Incl v | Excl v -> as_num v | Unbounded -> None in
    (match indexed, minmax with
     | true, Some (mn, mx) when mx > mn ->
       let l = match num lo with Some x -> Float.max mn x | None -> mn in
       let h = match num hi with Some x -> Float.min mx x | None -> mx in
       clamp ((h -. l) /. (mx -. mn))
     | _ -> (match lo, hi with Unbounded, _ | _, Unbounded -> 1. /. 3. | _ -> 0.25))

let rec est_sel (s : stats) (c : Typed.tcond) : float =
  match c with
  | Typed.TAnd (a, b) -> est_sel s a *. est_sel s b
  | Typed.TOr (a, b) -> let fa = est_sel s a and fb = est_sel s b in fa +. fb -. fa *. fb
  | Typed.TNot a -> 1. -. est_sel s a
  | Typed.TCmp (Ast.Neq, l, r) -> 1. -. est_sel s (Typed.TCmp (Ast.Eq, l, r))
  | Typed.TIn (e, items, true) -> 1. -. est_sel s (Typed.TIn (e, items, false))
  | Typed.TBetween (e, lo, hi, true) -> 1. -. est_sel s (Typed.TBetween (e, lo, hi, false))
  | Typed.TLike (e, p, true) -> 1. -. est_sel s (Typed.TLike (e, p, false))
  | Typed.TLike _ -> 0.005
  | Typed.TIsNotNull e ->
    (match side e with
     | SCol k -> (match stat_of s k with Some st when s.n > 0 -> 1. -. float_of_int st.nulls /. float_of_int s.n | _ -> 1.)
     | _ -> 1.)
  | leaf ->
    (match classify leaf with
     | Sarg (k, p) -> sel_of_pred s k p
     | NotSarg _ -> (match leaf with Typed.TCmp (Ast.Eq, _, _) -> 0.1 | _ -> 1. /. 3.))

let est_rows (s : stats) (f : float) : float =
  if s.n = 0 then 0. else Float.max 1. (Float.round (float_of_int s.n *. f))

(* ------------------------------------------------------------------ *)
(* Appariement index ↔ conjoints : règle du préfixe gauche (PG 11.3)    *)
(* ------------------------------------------------------------------ *)
type matching = {
  probes : probe list;          (* sondes à effectuer (vide si PNever) *)
  has_cond : bool;              (* au moins un conjoint borne le parcours *)
  const_cols : string list;     (* colonnes fixées par une égalité simple (constantes dans le flux) *)
  index_cond : int list;        (* conjoints qui bornent le parcours *)
  index_check : int list;       (* conjoints vérifiables dans l'index mais qui ne bornent pas *)
  residual : int list;          (* conjoints vérifiés sur la ligne du tas *)
  not_applicable : (int * reason) list;   (* pour l'affichage : pourquoi tel conjoint ne borne pas *)
}

let tighter_lo a b =
  match a, b with
  | Unbounded, x | x, Unbounded -> x
  | (Incl va | Excl va), (Incl vb | Excl vb) ->
    let c = compare_value va vb in
    if c > 0 then a else if c < 0 then b else (match a with Excl _ -> a | _ -> b)

let tighter_hi a b =
  match a, b with
  | Unbounded, x | x, Unbounded -> x
  | (Incl va | Excl va), (Incl vb | Excl vb) ->
    let c = compare_value va vb in
    if c < 0 then a else if c > 0 then b else (match a with Excl _ -> a | _ -> b)

let rec cartesian = function
  | [] -> [ [] ]
  | vs :: rest -> let tails = cartesian rest in List.concat_map (fun v -> List.map (fun t -> v :: t) tails) vs

let match_index (conjs : conjunct array) (b : built) : matching =
  let n = Array.length conjs in
  let used = Array.make n false in
  let find_unused pred =
    let r = ref None in
    Array.iteri (fun i c ->
      if !r = None && not used.(i) then
        match c.cls with Sarg (k, p) when pred k p -> r := Some (i, p) | _ -> ()) conjs;
    !r in
  let is_eq = function PEq _ | PIn _ | PNever -> true | _ -> false in
  let is_rng = function PRange _ | PIsNull -> true | _ -> false in
  (* parcours des colonnes de l'index dans l'ordre *)
  let rec walk cols pos prefix_sets const_cols cond =
    match cols with
    | [] -> (prefix_sets, const_cols, cond, None, None)
    | col :: rest ->
      (match find_unused (fun k p -> same_col k col && is_eq p) with
       | Some (i, p) ->
         used.(i) <- true;
         let vals = match p with PEq v -> [ v ] | PIn vs -> vs | _ -> [] in
         let const_cols = match p with PEq _ -> col :: const_cols | _ -> const_cols in
         walk rest (pos + 1) (prefix_sets @ [ vals ]) const_cols (cond @ [ i ])
       | None ->
         (* intervalle(s) / IS NULL sur cette colonne : intersectés, puis on s'arrête *)
         let lo = ref Unbounded and hi = ref Unbounded and isnull = ref false and ids = ref [] in
         let rec take () =
           match find_unused (fun k p -> same_col k col && is_rng p) with
           | None -> ()
           | Some (i, p) ->
             used.(i) <- true; ids := !ids @ [ i ];
             (match p with PRange (l, h) -> lo := tighter_lo !lo l; hi := tighter_hi !hi h | _ -> isnull := true);
             take ()
         in
         take ();
         if !ids = [] then (prefix_sets, const_cols, cond, None, None)
         else begin
           let bounds =
             if !isnull && (!lo <> Unbounded || !hi <> Unbounded) then (Incl VNull, Excl VNull)  (* IS NULL ∧ intervalle : vide *)
             else if !isnull then (Incl VNull, Incl VNull)
             else ((match !lo with Unbounded -> Excl VNull | l -> l), !hi)   (* une comparaison ne trouve jamais NULL *)
           in
           (prefix_sets, const_cols, cond @ !ids, Some bounds, Some pos)
         end)
  in
  let prefix_sets, const_cols, index_cond, bounds, range_pos = walk b.def.Db.icols 0 [] [] [] in
  let probes =
    if List.exists (fun vs -> vs = []) prefix_sets then []
    else
      let lo, hi = match bounds with Some (l, h) -> (l, h) | None -> (Unbounded, Unbounded) in
      List.map (fun prefix -> { prefix; lo; hi }) (cartesian (List.map uniq_values prefix_sets))
  in
  let pos_of k = let rec go i = function [] -> None | c :: r -> if same_col c k then Some i else go (i + 1) r in go 0 b.def.Db.icols in
  let index_check = ref [] and residual = ref [] and na = ref [] in
  Array.iteri (fun i c ->
    if not used.(i) then
      match c.cls with
      | Sarg (k, _) when pos_of k <> None ->
        index_check := i :: !index_check;
        let r = match range_pos, pos_of k with Some rp, Some kp when kp > rp -> AfterRange k | _ -> NotLeading k in
        na := (i, r) :: !na
      | Sarg _ -> residual := i :: !residual; na := (i, NotIndexed) :: !na
      | NotSarg r -> residual := i :: !residual; na := (i, r) :: !na) conjs;
  { probes; has_cond = index_cond <> []; const_cols = List.rev const_cols; index_cond;
    index_check = List.rev !index_check; residual = List.rev !residual; not_applicable = List.rev !na }

(* Ordre fourni par l'index (PG 11.4 ; Selinger : interesting order) : les colonnes d'ORDER BY,
   moins celles rendues constantes par une égalité, forment un préfixe des colonnes restantes de
   l'index ; directions homogènes (tout DESC = parcours arrière). *)
let order_provided (tq : T.tquery) (b : built) ~(const_cols : string list) : [ `Forward | `Backward ] option =
  if tq.T.order_by = [] then None
  else begin
    let items = List.map (fun (p, dir) ->
      (match p with Typed.Pack (_, Typed.TCol (k, _)) -> Some (col_of_key k) | _ -> None), dir) tq.T.order_by in
    if List.exists (fun (c, _) -> c = None) items then None
    else begin
      let is_const c = List.exists (same_col c) const_cols in
      let items = List.filter_map (fun (c, d) -> match c with Some c when not (is_const c) -> Some (c, d) | _ -> None) items in
      let dirs = List.map snd items in
      let dir = if List.for_all (( = ) Ast.Asc) dirs then Some `Forward
        else if List.for_all (( = ) Ast.Desc) dirs then Some `Backward else None in
      match dir with
      | None -> None
      | Some d ->
        let remaining = List.filter (fun c -> not (is_const c)) b.def.Db.icols in
        let rec prefix its cols = match its, cols with
          | [], _ -> true
          | (c, _) :: ri, ic :: rc -> same_col c ic && prefix ri rc
          | _, [] -> false in
        if prefix items remaining then Some d else None
    end
  end

(* Colonnes référencées par la requête (SELECT ∪ WHERE ∪ ORDER BY) → Index Only Scan (PG 11.9). *)
let rec cols_of_texpr : type a. a Typed.texpr -> string list = fun e ->
  match e with
  | Typed.TCol (k, _) -> [ col_of_key k ] | Typed.TLit _ -> []
  | Typed.TNeg x -> cols_of_texpr x | Typed.TAbs x -> cols_of_texpr x | Typed.TLength x -> cols_of_texpr x
  | Typed.TUpper x -> cols_of_texpr x | Typed.TLower x -> cols_of_texpr x | Typed.TTrim x -> cols_of_texpr x
  | Typed.TRound (x, _) -> cols_of_texpr x
  | Typed.TArith (_, a, b) -> cols_of_texpr a @ cols_of_texpr b
  | Typed.TCoalesce (es, _) -> List.concat_map cols_of_texpr es

let rec cols_of_tcond = function
  | Typed.TCmp (_, l, r) -> cols_of_texpr l @ cols_of_texpr r
  | Typed.TAnd (a, b) | Typed.TOr (a, b) -> cols_of_tcond a @ cols_of_tcond b
  | Typed.TNot a -> cols_of_tcond a
  | Typed.TIsNull e -> cols_of_texpr e
  | Typed.TIsNotNull e -> cols_of_texpr e
  | Typed.TIn (e, items, _) -> cols_of_texpr e @ List.concat_map cols_of_texpr items
  | Typed.TLike (e, p, _) -> cols_of_texpr e @ cols_of_texpr p
  | Typed.TBetween (e, lo, hi, _) -> cols_of_texpr e @ cols_of_texpr lo @ cols_of_texpr hi

let referenced (tq : T.tquery) : string list =
  let sel = match tq.T.select with
    | T.TStar -> List.map (fun c -> c.Db.cname) tq.T.base.T.table.Db.cols
    | T.TItems items -> List.concat_map (fun (_, p) -> match p with Typed.Pack (_, e) -> cols_of_texpr e) items in
  let wh = match tq.T.where with None -> [] | Some c -> cols_of_tcond c in
  let ob = List.concat_map (fun (p, _) -> match p with Typed.Pack (_, e) -> cols_of_texpr e) tq.T.order_by in
  sel @ wh @ ob

let covering (tq : T.tquery) (b : built) : bool =
  List.for_all (fun c -> List.exists (same_col c) b.def.Db.icols) (referenced tq)

(* ------------------------------------------------------------------ *)
(* Chemins d'accès et coût                                              *)
(* ------------------------------------------------------------------ *)
type access = SeqScan | IndexScan of built | IndexOnlyScan of built

type est = { rows : float; access_cost : float; sort_cost : float; total : float; formula : string list }

type path = {
  access : access;
  m : matching;
  order : [ `Forward | `Backward ] option;   (* ordre d'ORDER BY fourni par le parcours *)
  sort_needed : bool;
  est : est;                (* coût à l'échelle RÉELLE de la table *)
  est_sim : est option;     (* coût à l'échelle SIMULÉE (options.scale), la table servant d'échantillon *)
}

let index_name = function SeqScan -> "seq_scan" | IndexScan b | IndexOnlyScan b -> b.def.Db.iname
let f2 = Printf.sprintf "%.2f"
let f4 = Printf.sprintf "%.4g"

(* ---- Échelle : réelle (stats de la table) ou simulée (N lignes, mêmes fractions qu'observées) ---- *)
type scale_ctx = { sn : float; spages : float; sheight : built -> int }

let real_ctx (c : consts) (s : stats) : scale_ctx =
  { sn = float_of_int s.n; spages = float_of_int s.pages;
    sheight = (fun b -> (build_tree ~fanout:c.fanout b.entries).height) }

(* hauteur d'un B-tree de fan-out F sur n entrées : 1 + ⌈log_F(feuilles)⌉ (Comer 1979) *)
let sim_height (c : consts) (n : int) : int =
  let leaves = max 1 ((n + c.fanout - 1) / c.fanout) in
  if leaves <= 1 then 1
  else 1 + int_of_float (Float.ceil (log (float_of_int leaves) /. log (float_of_int c.fanout) -. 1e-9))

let sim_ctx (c : consts) (n : int) : scale_ctx =
  { sn = float_of_int n; spages = float_of_int ((n + c.rows_per_page - 1) / c.rows_per_page); sheight = (fun _ -> sim_height c n) }

(* Colonne unique (sans NULL) dans l'échantillon : une clé, qui reste unique à grande échelle. *)
let unique_in_sample (s : stats) col =
  match stat_of s col with Some st -> s.n > 0 && st.nulls = 0 && st.distinct = s.n | None -> false

(* Sélectivité d'un conjoint à l'échelle simulée : fraction observée sur l'échantillon, sauf pour une
   égalité sur une clé (1/N par valeur cherchée) et une comparaison à NULL (0). *)
let sim_sel (s : stats) (n_sim : int) (cj : conjunct) : float =
  let nf = float_of_int (max 1 n_sim) in
  match cj.cls with
  | Sarg (k, PEq _) when unique_in_sample s k -> 1. /. nf
  | Sarg (k, PIn vs) when unique_in_sample s k -> Float.min 1. (float_of_int (List.length vs) /. nf)
  | Sarg (_, PNever) -> 0.
  | _ -> if s.n > 0 then cj.obs else cj.sel

(* Cœur du modèle de coût (forme System R, constantes PostgreSQL), à une échelle donnée.
   [fidx] = sélectivité de la condition d'index, [fwhere] = sélectivité du WHERE entier. *)
let cost_core (c : consts) (tq : T.tquery) (ctx : scale_ctx) ~(nq : int) ~(fidx : float) ~(fwhere : float)
    (access : access) (m : matching) (order : [ `Forward | `Backward ] option) : est * bool =
  let est_rows f = if ctx.sn <= 0. then 0. else Float.max 1. (Float.round (ctx.sn *. f)) in
  let r = est_rows fwhere in
  let sort_needed = tq.T.order_by <> [] && order = None in
  let ko = match tq.T.limit with Some l -> Some (l + (match tq.T.offset with Some o -> o | None -> 0)) | None -> None in
  let n = ctx.sn and pages = ctx.spages in
  let ni = int_of_float n and pi = int_of_float pages in
  let access_cost, lines =
    match access with
    | SeqScan ->
      let io = pages *. c.seq_page_cost and cpu = n *. c.cpu_tuple_cost and ops = n *. float_of_int nq *. c.cpu_operator_cost in
      (io +. cpu +. ops,
       [ Printf.sprintf "pages × seq_page_cost = %d × %s = %s" pi (f4 c.seq_page_cost) (f2 io);
         Printf.sprintf "lignes × cpu_tuple_cost = %d × %s = %s" ni (f4 c.cpu_tuple_cost) (f2 cpu);
         Printf.sprintf "lignes × prédicats × cpu_operator_cost = %d × %d × %s = %s" ni nq (f4 c.cpu_operator_cost) (f2 ops) ])
    | IndexScan b | IndexOnlyScan b ->
      let e = if m.has_cond then est_rows fidx else n in
      let height = ctx.sheight b in
      let ipages = float_of_int (height - 1) +. Float.ceil (e /. float_of_int c.fanout) in
      let nidx = List.length m.index_cond + List.length m.index_check and nres = List.length m.residual in
      let io_idx = ipages *. c.random_page_cost in
      let cpu_idx = e *. c.cpu_index_tuple_cost +. e *. float_of_int nidx *. c.cpu_operator_cost in
      let only = (match access with IndexOnlyScan _ -> true | _ -> false) in
      let hpages = Float.min pages e in
      let io_heap = if only then 0. else hpages *. c.random_page_cost in
      let cpu_heap = e *. c.cpu_tuple_cost +. e *. float_of_int nres *. c.cpu_operator_cost in
      (io_idx +. cpu_idx +. io_heap +. cpu_heap,
       [ Printf.sprintf "entrées estimées E = %s (sélectivité %s × %d lignes)" (f4 e) (f4 (if m.has_cond then fidx else 1.)) ni;
         Printf.sprintf "pages d'index (hauteur %d − 1 + ⌈E/fanout⌉) × random_page_cost = %s × %s = %s" height (f4 ipages) (f4 c.random_page_cost) (f2 io_idx);
         Printf.sprintf "E × cpu_index_tuple_cost + E × %d × cpu_operator_cost = %s" nidx (f2 cpu_idx) ]
       @ (if only then [ "Index Only Scan : aucune page du tas" ]
          else [ Printf.sprintf "pages du tas min(pages, E) × random_page_cost = %s × %s = %s" (f4 hpages) (f4 c.random_page_cost) (f2 io_heap) ])
       @ [ Printf.sprintf "E × cpu_tuple_cost + E × %d résiduel(s) × cpu_operator_cost = %s" nres (f2 cpu_heap) ])
  in
  let sort_cost, sort_lines =
    if not sort_needed then (0., [])
    else begin
      let base = Float.max 2. r in
      let l2 = Float.log2 (match ko with Some k when float_of_int k < r -> Float.max 2. (2. *. float_of_int k) | _ -> base) in
      let sc = 2. *. c.cpu_operator_cost *. r *. l2 in
      (sc, [ Printf.sprintf "tri : 2 × cpu_operator_cost × R × log₂ = 2 × %s × %s × %s = %s" (f4 c.cpu_operator_cost) (f4 r) (f2 l2) (f2 sc) ])
    end
  in
  let access', limit_lines =
    match ko with
    | Some k when not sort_needed && r > 0. && float_of_int k < r ->
      let frac = float_of_int k /. r in
      (access_cost *. frac, [ Printf.sprintf "LIMIT sans tri : × %s / %s (arrêt anticipé)" (string_of_int k) (f4 r) ])
    | _ -> (access_cost, [])
  in
  ({ rows = r; access_cost = access'; sort_cost; total = access' +. sort_cost;
     formula = lines @ limit_lines @ sort_lines @ [ Printf.sprintf "total = %s" (f2 (access' +. sort_cost)) ] },
   sort_needed)

let prod f l = List.fold_left (fun acc i -> acc *. f i) 1. l

(* coût à l'échelle réelle (sélectivités estimées de Selinger) *)
let cost_of (c : consts) (tq : T.tquery) (s : stats) (conjs : conjunct array) (access : access) (m : matching)
    (order : [ `Forward | `Backward ] option) : est * bool =
  let all = List.init (Array.length conjs) (fun i -> i) in
  let sel i = conjs.(i).sel in
  cost_core c tq (real_ctx c s) ~nq:(Array.length conjs) ~fidx:(prod sel m.index_cond) ~fwhere:(prod sel all) access m order

(* coût à l'échelle simulée (fractions observées sur l'échantillon, clés → 1/N) *)
let cost_sim (c : consts) (tq : T.tquery) (s : stats) (n_sim : int) (conjs : conjunct array) (access : access) (m : matching)
    (order : [ `Forward | `Backward ] option) : est =
  let all = List.init (Array.length conjs) (fun i -> i) in
  let sel i = sim_sel s n_sim conjs.(i) in
  fst (cost_core c tq (sim_ctx c n_sim) ~nq:(Array.length conjs) ~fidx:(prod sel m.index_cond) ~fwhere:(prod sel all) access m order)

let empty_matching (conjs : conjunct array) : matching =
  { probes = []; has_cond = false; const_cols = []; index_cond = []; index_check = [];
    residual = List.init (Array.length conjs) (fun i -> i); not_applicable = [] }

let enumerate_paths ?(scale : int option) (c : consts) (tq : T.tquery) (s : stats) (enabled : built list) (conjs : conjunct array) : path list =
  let sim access m order = Option.map (fun n -> cost_sim c tq s n conjs access m order) scale in
  let seq =
    let m = empty_matching conjs in
    let est, sort_needed = cost_of c tq s conjs SeqScan m None in
    { access = SeqScan; m; order = None; sort_needed; est; est_sim = sim SeqScan m None } in
  let idx = List.filter_map (fun b ->
    let m = match_index conjs b in
    let order = order_provided tq b ~const_cols:m.const_cols in
    if not m.has_cond && order = None then None
    else begin
      let access = if covering tq b then IndexOnlyScan b else IndexScan b in
      let est, sort_needed = cost_of c tq s conjs access m order in
      Some { access; m; order; sort_needed; est; est_sim = sim access m order }
    end) enabled in
  seq :: idx

(* Courbe de bascule : coût du Seq Scan et du chemin d'index [ip] selon la sélectivité s (échelle [ctx]).
   Points log-espacés de 10⁻⁴ à 1. *)
let curve (c : consts) (tq : T.tquery) (ctx : scale_ctx) (conjs : conjunct array) (ip : path) : (float * float * float) list =
  let nq = Array.length conjs in
  let pts = List.init 41 (fun i -> 10. ** (-4. +. 4. *. float_of_int i /. 40.)) in
  List.map (fun sel ->
    let seq, _ = cost_core c tq ctx ~nq ~fidx:sel ~fwhere:sel SeqScan (empty_matching conjs) None in
    let idx, _ = cost_core c tq ctx ~nq ~fidx:sel ~fwhere:sel ip.access ip.m ip.order in
    (sel, seq.total, idx.total)) pts

(* ------------------------------------------------------------------ *)
(* Exécution EXACTE du chemin choisi                                    *)
(* ------------------------------------------------------------------ *)
type descent = { probe : value list; path : int list; lo : int; hi : int }

type exec = {
  descents : descent list;   (* une par sonde *)
  entries_scanned : int;     (* entrées d'index examinées *)
  index_pages : int;         (* pages d'index lues (internes + feuilles) *)
  stream : int list;         (* rowids dans l'ordre de lecture *)
  touched : int list;        (* lignes du tas lues (vide pour Index Only Scan) *)
  heap_pages : int list;     (* pages du tas lues *)
  candidates : int;          (* = |stream| *)
  passed : int;              (* candidats satisfaisant le WHERE *)
  returned : int;            (* lignes finalement renvoyées *)
  early_stop : bool;         (* arrêt anticipé (LIMIT atteint) *)
}

let sort_uniq_ints l = List.sort_uniq compare l

let execute (c : consts) (tq : T.tquery) (t : Db.table) (p : path) : exec * Db.row list =
  let keyed = Array.of_list (List.map (Semantics.keyed_row tq.T.base) t.Db.rows) in
  let passes r = match tq.T.where with None -> true | Some cond -> Typed.eval_cond r cond = True in
  let early_ok = tq.T.limit <> None && not tq.T.distinct && (tq.T.order_by = [] || p.order <> None) in
  let need = match tq.T.limit with Some l -> l + (match tq.T.offset with Some o -> o | None -> 0) | None -> max_int in
  let stream = ref [] and passed = ref 0 and stopped = ref false in
  let visit rowid =
    stream := rowid :: !stream;
    if passes keyed.(rowid) then incr passed;
    if early_ok && !passed >= need then stopped := true in
  let descents = ref [] and entries_scanned = ref 0 and leaf_pages = ref [] and n_descents = ref 0 and height = ref 0 in
  let total_candidates = ref 0 in
  (match p.access with
   | SeqScan ->
     let n = Array.length keyed in
     total_candidates := n;
     let i = ref 0 in
     while !i < n && not !stopped do visit !i; incr i done
   | IndexScan b | IndexOnlyScan b ->
     let tree = build_tree ~fanout:c.fanout b.entries in
     height := tree.height;
     let backward = (p.order = Some `Backward) in
     let probes = if backward then List.rev p.m.probes else p.m.probes in
     let ranges = List.map (fun pr -> (pr, range_of b pr)) probes in
     total_candidates := List.fold_left (fun acc (_, (lo, hi)) -> acc + (hi - lo)) 0 ranges;
     let n = Array.length b.entries in
     List.iter (fun (pr, (lo, hi)) ->
       if not !stopped then begin
         incr n_descents;
         let shown = pr.prefix @ (match pr.lo with Incl v | Excl v -> [ v ] | Unbounded -> []) in
         descents := { probe = shown; path = path_to tree lo; lo; hi } :: !descents;
         if n > 0 then leaf_pages := (min lo (n - 1) / tree.fanout) :: !leaf_pages;
         let positions = if backward then List.init (hi - lo) (fun k -> hi - 1 - k) else List.init (hi - lo) (fun k -> lo + k) in
         List.iter (fun pos ->
           if not !stopped then begin
             incr entries_scanned;
             leaf_pages := (pos / tree.fanout) :: !leaf_pages;
             visit b.entries.(pos).rowid
           end) positions
       end) ranges);
  let stream = List.rev !stream in
  let only = (match p.access with IndexOnlyScan _ -> true | _ -> false) in
  let touched = if only then [] else stream in
  let heap_pages = sort_uniq_ints (List.map (fun rid -> rid / c.rows_per_page) touched) in
  let index_pages = (match p.access with SeqScan -> 0 | _ -> (!height - 1) * !n_descents + List.length (sort_uniq_ints !leaf_pages)) in
  ({ descents = List.rev !descents; entries_scanned = !entries_scanned; index_pages; stream; touched; heap_pages;
     candidates = List.length stream; passed = !passed; returned = 0;
     early_stop = !stopped && List.length stream < !total_candidates },
   List.map (fun rid -> keyed.(rid)) stream)

(* ------------------------------------------------------------------ *)
(* Vérification de correction : physique ≡ sémantique                    *)
(* ------------------------------------------------------------------ *)
type soundness = {
  bag_equal : bool;          (* mêmes lignes (sacs) *)
  order_keys_equal : bool;   (* mêmes clés d'ORDER BY (multiensemble) *)
  sorted_ok : bool;          (* la sortie physique respecte l'ORDER BY *)
  tie_ambiguity : bool;      (* LIMIT + ex æquo : lignes différentes mais résultat SQL valide *)
  sound : bool;
}

let bag_of recs = List.sort compare (List.map fst recs)

let rec is_sorted cmp = function
  | a :: (b :: _ as rest) -> cmp a b <= 0 && is_sorted cmp rest
  | _ -> true

(* a ⊆ b en multiensembles (listes triées) *)
let rec subbag a b =
  match a, b with
  | [], _ -> true | _, [] -> false
  | x :: ra, y :: rb -> let c = compare x y in if c = 0 then subbag ra rb else if c > 0 then subbag a rb else false

let check (tq : T.tquery) ~(sem : (value list * Db.row) list) ~(phys : (value list * Db.row) list) : soundness =
  let bag_equal = bag_of sem = bag_of phys in
  let has_order = tq.T.order_by <> [] in
  let sorted_ok = (not has_order) || is_sorted (Semantics.order_cmp tq) phys in
  let keys recs = List.sort compare (List.map (fun (_, env) -> List.map (fun (p, _) -> Typed.eval_to_value env p) tq.T.order_by) recs) in
  let order_keys_equal = (not has_order) || keys sem = keys phys in
  let limited = tq.T.limit <> None || tq.T.offset <> None in
  let tie_ambiguity =
    (not bag_equal) && limited && List.length sem = List.length phys && sorted_ok && order_keys_equal
    && (let full = fst (Semantics.run_records { tq with T.limit = None; T.offset = None } (Semantics.scan_all tq)) in
        subbag (bag_of phys) (bag_of full)) in
  { bag_equal; order_keys_equal; sorted_ok; tie_ambiguity; sound = (bag_equal && sorted_ok) || tie_ambiguity }

(* ------------------------------------------------------------------ *)
(* Plan complet                                                         *)
(* ------------------------------------------------------------------ *)
type plan_info = {
      table : Db.table;
      consts : consts;
      stats : stats;
      scale : int option;                    (* échelle simulée active *)
      sim_pages : int;                       (* pages à l'échelle simulée (0 si échelle réelle) *)
      conjuncts : conjunct array;
      obs_where : float;                     (* fraction observée des lignes satisfaisant le WHERE entier *)
      query_sel : float;                     (* sélectivité du WHERE à l'échelle ACTIVE (observée, ou extrapolée si simulée) *)
      indexes : built list;                  (* tous les index construits (actifs ou non) *)
      trees : (string * btree) list;
      reports : (string * matching) list;    (* appariement pour chaque index ACTIF (pédagogie « pourquoi pas ») *)
      paths : path list;
      chosen : int;
      forced : bool;
      curve : (float * float * float) list;  (* (sélectivité, coût seq, coût index) à l'échelle active *)
      curve_index : string option;           (* index de la courbe *)
      exec : exec;
      phys : Semantics.result;               (* résultat produit par le chemin physique *)
      sound : soundness;
      warnings : string list;
    }

type plan = Unavailable of string | Plan of plan_info

(* coût qui décide : simulé si une échelle est active, réel sinon *)
let deciding_total (o : options) (p : path) : float =
  match o.scale, p.est_sim with Some _, Some e -> e.total | _ -> p.est.total

let choose (o : options) (paths : path list) : int * bool =
  let best = ref 0 in
  List.iteri (fun i p -> if deciding_total o p < deciding_total o (List.nth paths !best) then best := i) paths;
  match o.force with
  | Some name ->
    (match List.find_opt (fun (_, p) -> same_col (index_name p.access) name) (List.mapi (fun i p -> (i, p)) paths) with
     | Some (i, _) -> (i, true)
     | None -> (!best, false))
  | None -> (!best, false)

let plan (o : options) (tq : T.tquery) ~(sem : (value list * Db.row) list) : plan =
  if tq.T.joins <> [] then Unavailable "jointures : incrément suivant"
  else begin
    let t = tq.T.base.T.table in
    let builts, warnings =
      List.fold_left (fun (bs, ws) def ->
        match build_index t def with
        | Ok b ->
          let ws = if def.Db.iunique && b.unique_violations <> [] then
              ws @ [ Printf.sprintf "index unique « %s » : %d clé(s) en double" def.Db.iname (List.length b.unique_violations) ] else ws in
          (bs @ [ b ], ws)
        | Error m -> (bs, ws @ [ m ])) ([], []) t.Db.indexes in
    let enabled = List.filter (fun b -> b.def.Db.ienabled) builts in
    let stats = compute_stats o.consts t enabled in
    (* sélectivités observées : fraction exacte des lignes de l'échantillon qui satisfont chaque conjoint *)
    let keyed = List.map (Semantics.keyed_row tq.T.base) t.Db.rows in
    let frac cnd =
      if keyed = [] then 0.
      else float_of_int (List.length (List.filter (fun r -> Typed.eval_cond r cnd = True) keyed)) /. float_of_int (List.length keyed) in
    let conjs = Array.of_list (List.map (fun cnd -> { text = pc cnd; cond = cnd; cls = classify cnd; sel = est_sel stats cnd; obs = frac cnd })
                                 (match tq.T.where with None -> [] | Some w -> conjuncts w)) in
    let obs_where = match tq.T.where with None -> 1. | Some w -> frac w in
    let reports = List.map (fun b -> (b.def.Db.iname, match_index conjs b)) enabled in
    let scale = match o.scale with Some n when n > 0 -> Some n | _ -> None in
    let paths = enumerate_paths ?scale o.consts tq stats enabled conjs in
    let chosen, forced = choose { o with scale } paths in
    let path = List.nth paths chosen in
    let exec, stream = execute o.consts tq t path in
    let recs, phys = Semantics.run_records ~sort:path.sort_needed tq stream in
    let exec = { exec with returned = List.length recs } in
    let sound = check tq ~sem ~phys:recs in
    let trees = List.map (fun b -> (b.def.Db.iname, build_tree ~fanout:o.consts.fanout b.entries)) builts in
    (* courbe de bascule : pour le chemin d'index choisi, sinon le meilleur chemin d'index disponible *)
    let ctx = match scale with Some n -> sim_ctx o.consts n | None -> real_ctx o.consts stats in
    let idx_paths = List.filter (fun p -> p.access <> SeqScan) paths in
    let curve_path =
      if path.access <> SeqScan then Some path
      else List.fold_left (fun best p -> match best with
          | None -> Some p
          | Some b -> if deciding_total { o with scale } p < deciding_total { o with scale } b then Some p else best) None idx_paths in
    let curve, curve_index = match curve_path with
      | Some ip -> (curve o.consts tq ctx conjs ip, Some (index_name ip.access))
      | None -> ([], None) in
    let sim_pages = match scale with Some n -> (n + o.consts.rows_per_page - 1) / o.consts.rows_per_page | None -> 0 in
    let query_sel = match scale with
      | None -> obs_where
      | Some n -> Array.fold_left (fun acc cj -> acc *. sim_sel stats n cj) 1. conjs in
    Plan { table = t; consts = o.consts; stats; scale; sim_pages; conjuncts = conjs; obs_where; query_sel; indexes = builts; trees; reports;
           paths; chosen; forced; curve; curve_index; exec; phys; sound; warnings }
  end
