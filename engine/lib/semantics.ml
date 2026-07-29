(* semantics.ml — Runner : exécute une requête TYPÉE et construit la trace pas-à-pas.
   Sémantique de sacs (Guagliardo & Libkin). Les jointures produisent une relation
   combinée dont les champs sont clés "alias.col" ; l'appariement se fait quand
   ON vaut True (3VL). Remplissage NULL des orphelines pour LEFT/RIGHT/FULL. *)

open Value
module T = Typecheck

type where_row = { idx : int; tv : tv; pass : bool }

type stage = {
  kind : string;
  label : string;
  columns : string list;
  rows : value list list;
  note : string;
  where_rows : where_row list option;
}

type result = {
  ok : bool;
  error : string option;
  columns : string list;
  out_rows : value list list;
  pipeline : stage list;
}

(* ---- Construction de la relation combinée ---- *)
let source_keys (s : T.tsource) = List.map (fun c -> T.key_of s.T.alias c.Db.cname) s.T.table.Db.cols

let scan_source (s : T.tsource) : Db.row list =
  List.map (fun r ->
    List.map (fun c ->
      (T.key_of s.T.alias c.Db.cname, match List.assoc_opt c.Db.cname r with Some v -> v | None -> VNull))
      s.T.table.Db.cols)
    s.T.table.Db.rows

let null_row keys : Db.row = List.map (fun k -> (k, VNull)) keys
let eval_on on row = match on with None -> true | Some c -> Typed.eval_cond row c = True

(* Sémantique des jointures — SOURCÉE (cf. About / feedback-every-construct-must-be-sourced) :
   - Produit + sélection (INNER = σ_ON(A×B)) : Guagliardo & Libkin, PVLDB 2017.
   - Jointures externes avec NULL (LEFT/RIGHT/FULL) : Ricciotti & Cheney,
     "A Formalization of SQL with Nulls", arXiv:2003.11331 (JAR 2022).
   Définition standard : A LEFT JOIN B ON θ = σ_θ(A×B) ∪ { a·NULL | a∈A, ∄ b. θ(a,b)=True }.
   Appariement = ON vaut True (3VL). Validée par différentiel vs SQLite (voir difftest/). *)

let combine kind on (left : Db.row list) left_keys (right : Db.row list) right_keys : Db.row list =
  match (kind : Ast.join_kind) with
  | Ast.Inner | Ast.Cross ->
    List.concat_map (fun l -> List.filter_map (fun r -> if eval_on on (l @ r) then Some (l @ r) else None) right) left
  | Ast.Left ->
    List.concat_map (fun l ->
      match List.filter (fun r -> eval_on on (l @ r)) right with
      | [] -> [ l @ null_row right_keys ]
      | ms -> List.map (fun r -> l @ r) ms) left
  | Ast.Right ->
    List.concat_map (fun r ->
      match List.filter (fun l -> eval_on on (l @ r)) left with
      | [] -> [ null_row left_keys @ r ]
      | ms -> List.map (fun l -> l @ r) ms) right
  | Ast.Full ->
    let left_part =
      List.concat_map (fun l ->
        match List.filter (fun r -> eval_on on (l @ r)) right with
        | [] -> [ l @ null_row right_keys ]
        | ms -> List.map (fun r -> l @ r) ms) left
    in
    let unmatched_right = List.filter (fun r -> not (List.exists (fun l -> eval_on on (l @ r)) left)) right in
    left_part @ List.map (fun r -> null_row left_keys @ r) unmatched_right

let to_pos keys (r : Db.row) = List.map (fun k -> match List.assoc_opt k r with Some v -> v | None -> VNull) keys

(* ---- Exécution ---- *)
let run (tq : T.tquery) : result =
  (* relation combinée *)
  let base_rows = scan_source tq.T.base in
  let base_keys = source_keys tq.T.base in
  let scan, all_keys =
    List.fold_left (fun (rows, keys) (j : T.tjoin) ->
      let right = scan_source j.T.src in
      let rk = source_keys j.T.src in
      (combine j.T.kind j.T.on rows keys right rk, keys @ rk))
      (base_rows, base_keys) tq.T.joins
  in
  let single = (tq.T.joins = []) in
  let disp_keys = all_keys in
  (* pour une seule table : afficher les noms de colonnes nus ; sinon "alias.col" *)
  let disp_cols =
    if single then List.map (fun c -> c.Db.cname) tq.T.base.T.table.Db.cols else all_keys
  in
  let pipeline = ref [] in
  let add s = pipeline := s :: !pipeline in

  let from_label =
    if single then "FROM " ^ tq.T.base.T.table.Db.tname
    else "FROM " ^ tq.T.base.T.alias ^ String.concat "" (List.map (fun (j : T.tjoin) -> " ⋈ " ^ j.T.src.T.alias) tq.T.joins)
  in
  add { kind = "from"; label = from_label; columns = disp_cols;
        rows = List.map (to_pos disp_keys) scan;
        note = Printf.sprintf "%d ligne(s)%s" (List.length scan) (if single then "" else " (relation jointe)");
        where_rows = None };

  (* WHERE *)
  let survivors =
    match tq.T.where with
    | None -> scan
    | Some cond ->
      let wr = ref [] and kept = ref [] in
      List.iteri (fun i r ->
        let t = Typed.eval_cond r cond in
        let pass = (t = True) in
        wr := { idx = i; tv = t; pass } :: !wr;
        if pass then kept := r :: !kept) scan;
      let kept = List.rev !kept in
      add { kind = "where"; label = "WHERE"; columns = disp_cols;
            rows = List.map (to_pos disp_keys) kept;
            note = Printf.sprintf "%d ligne(s) gardée(s) sur %d" (List.length kept) (List.length scan);
            where_rows = Some (List.rev !wr) };
      kept
  in

  (* SELECT : (projection, env) ; env = ligne combinée + alias, pour ORDER BY *)
  let out_cols, records =
    match tq.T.select with
    | T.TStar -> disp_cols, List.map (fun r -> (to_pos disp_keys r, r)) survivors
    | T.TItems items ->
      let names = List.map fst items in
      names,
      List.map (fun r ->
        let vals = List.map (fun (_, p) -> Typed.eval_to_value r p) items in
        let env = r @ List.map2 (fun (n, _) v -> (n, v)) items vals in
        (vals, env)) survivors
  in
  add { kind = "select"; label = "SELECT"; columns = out_cols; rows = List.map fst records;
        note = (match tq.T.select with T.TStar -> "toutes les colonnes" | T.TItems l -> Printf.sprintf "%d colonne(s) projetée(s)" (List.length l));
        where_rows = None };

  (* DISTINCT *)
  let records =
    if tq.T.distinct then begin
      let seen = Hashtbl.create 16 and out = ref [] in
      List.iter (fun (vals, env) ->
        let key = String.concat "\x00" (List.map to_display vals) in
        if not (Hashtbl.mem seen key) then (Hashtbl.replace seen key (); out := (vals, env) :: !out)) records;
      let recs = List.rev !out in
      add { kind = "distinct"; label = "DISTINCT"; columns = out_cols; rows = List.map fst recs;
            note = Printf.sprintf "%d ligne(s) unique(s)" (List.length recs); where_rows = None };
      recs
    end else records
  in

  (* ORDER BY *)
  let records =
    if tq.T.order_by = [] then records
    else begin
      let cmp (_, ea) (_, eb) =
        let rec go = function
          | [] -> 0
          | (p, dir) :: rest ->
            let va = Typed.eval_to_value ea p and vb = Typed.eval_to_value eb p in
            let c = if is_null va && is_null vb then 0 else if is_null va then -1 else if is_null vb then 1 else compare_nonnull va vb in
            let c = if dir = Ast.Desc then -c else c in
            if c <> 0 then c else go rest
        in
        go tq.T.order_by
      in
      let recs = List.stable_sort cmp records in
      add { kind = "order"; label = "ORDER BY"; columns = out_cols; rows = List.map fst recs; note = "tri appliqué"; where_rows = None };
      recs
    end
  in

  (* LIMIT / OFFSET *)
  let records =
    match tq.T.limit, tq.T.offset with
    | None, None -> records
    | _ ->
      let off = match tq.T.offset with Some o -> o | None -> 0 in
      let arr = Array.of_list records in
      let len = Array.length arr in
      let lim = match tq.T.limit with Some l -> l | None -> len in
      let stop = min len (off + lim) in
      let sub = if off >= len then [] else Array.to_list (Array.sub arr off (max 0 (stop - off))) in
      add { kind = "limit"; label = (if tq.T.offset <> None then "LIMIT / OFFSET" else "LIMIT");
            columns = out_cols; rows = List.map fst sub;
            note = Printf.sprintf "%d ligne(s) conservée(s)" (List.length sub); where_rows = None };
      sub
  in

  { ok = true; error = None; columns = out_cols; out_rows = List.map fst records; pipeline = List.rev !pipeline }
