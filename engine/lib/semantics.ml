(* semantics.ml — Runner : exécute une requête TYPÉE et construit la trace pas-à-pas.
   La sémantique (sacs, 3VL sur WHERE, projection) suit Guagliardo & Libkin (PVLDB 2017).
   L'évaluation des expressions/conditions est déléguée au cœur typé (Typed). *)

open Value

(* ---- Résultat + trace ---- *)
type where_row = { idx : int; tv : tv; pass : bool }

type stage = {
  kind : string;                 (* from | where | select | distinct | order | limit *)
  label : string;
  columns : string list;
  rows : value list list;        (* lignes positionnelles alignées sur [columns] *)
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

let run (tq : Typecheck.tquery) : result =
  let tbl = tq.Typecheck.from in
  let src_cols = List.map (fun c -> c.Db.cname) tbl.Db.cols in
  let pipeline = ref [] in
  let add s = pipeline := s :: !pipeline in

  (* FROM *)
  add { kind = "from"; label = "FROM " ^ tbl.Db.tname; columns = src_cols;
        rows = List.map (Db.to_positional src_cols) tbl.Db.rows;
        note = Printf.sprintf "%d ligne(s) lue(s) dans « %s »" (List.length tbl.Db.rows) tbl.Db.tname;
        where_rows = None };

  (* WHERE (3VL : on garde ssi True) *)
  let survivors =
    match tq.Typecheck.where with
    | None -> tbl.Db.rows
    | Some cond ->
      let wr = ref [] and kept = ref [] in
      List.iteri (fun i r ->
        let t = Typed.eval_cond r cond in
        let pass = (t = True) in
        wr := { idx = i; tv = t; pass } :: !wr;
        if pass then kept := r :: !kept) tbl.Db.rows;
      let kept = List.rev !kept in
      add { kind = "where"; label = "WHERE"; columns = src_cols;
            rows = List.map (Db.to_positional src_cols) kept;
            note = Printf.sprintf "%d ligne(s) gardée(s) sur %d" (List.length kept) (List.length tbl.Db.rows);
            where_rows = Some (List.rev !wr) };
      kept
  in

  (* SELECT : on calcule (projection, env) ; env = ligne source + alias, pour ORDER BY *)
  let out_cols, records =
    match tq.Typecheck.select with
    | Typecheck.TStar ->
      src_cols, List.map (fun r -> (Db.to_positional src_cols r, r)) survivors
    | Typecheck.TItems items ->
      let names = List.map fst items in
      names,
      List.map (fun r ->
        let vals = List.map (fun (_, p) -> Typed.eval_to_value r p) items in
        let env = r @ List.map2 (fun (n, _) v -> (n, v)) items vals in
        (vals, env)) survivors
  in
  add { kind = "select"; label = "SELECT"; columns = out_cols;
        rows = List.map fst records;
        note = (match tq.Typecheck.select with Typecheck.TStar -> "toutes les colonnes"
                | Typecheck.TItems l -> Printf.sprintf "%d colonne(s) projetée(s)" (List.length l));
        where_rows = None };

  (* DISTINCT *)
  let records =
    if tq.Typecheck.distinct then begin
      let seen = Hashtbl.create 16 and out = ref [] in
      List.iter (fun (vals, env) ->
        let key = String.concat "\x00" (List.map to_display vals) in
        if not (Hashtbl.mem seen key) then (Hashtbl.replace seen key (); out := (vals, env) :: !out)) records;
      let recs = List.rev !out in
      add { kind = "distinct"; label = "DISTINCT"; columns = out_cols;
            rows = List.map fst recs; note = Printf.sprintf "%d ligne(s) unique(s)" (List.length recs);
            where_rows = None };
      recs
    end else records
  in

  (* ORDER BY (NULLS FIRST en ASC, convention SQLite) *)
  let records =
    if tq.Typecheck.order_by = [] then records
    else begin
      let cmp (_, env_a) (_, env_b) =
        let rec go = function
          | [] -> 0
          | (p, dir) :: rest ->
            let va = Typed.eval_to_value env_a p and vb = Typed.eval_to_value env_b p in
            let c =
              if is_null va && is_null vb then 0
              else if is_null va then -1
              else if is_null vb then 1
              else compare_nonnull va vb
            in
            let c = if dir = Ast.Desc then -c else c in
            if c <> 0 then c else go rest
        in
        go tq.Typecheck.order_by
      in
      let recs = List.stable_sort cmp records in
      add { kind = "order"; label = "ORDER BY"; columns = out_cols;
            rows = List.map fst recs; note = "tri appliqué"; where_rows = None };
      recs
    end
  in

  (* LIMIT / OFFSET *)
  let records =
    match tq.Typecheck.limit, tq.Typecheck.offset with
    | None, None -> records
    | _ ->
      let off = match tq.Typecheck.offset with Some o -> o | None -> 0 in
      let arr = Array.of_list records in
      let len = Array.length arr in
      let lim = match tq.Typecheck.limit with Some l -> l | None -> len in
      let stop = min len (off + lim) in
      let sub = if off >= len then [] else Array.to_list (Array.sub arr off (max 0 (stop - off))) in
      add { kind = "limit"; label = (if tq.Typecheck.offset <> None then "LIMIT / OFFSET" else "LIMIT");
            columns = out_cols; rows = List.map fst sub;
            note = Printf.sprintf "%d ligne(s) conservée(s)" (List.length sub); where_rows = None };
      sub
  in

  { ok = true; error = None; columns = out_cols;
    out_rows = List.map fst records; pipeline = List.rev !pipeline }
