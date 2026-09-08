(* json_io.ml — Frontière JSON (yojson) entre le moteur OCaml et le monde JS.
   Décode la base de données entrante, encode le résultat + la trace. *)

open Value
module J = Yojson.Safe

exception Json_error of string

let value_of_json (j : J.t) : value =
  match j with
  | `Int i -> VInt i
  | `Intlit s -> (try VInt (int_of_string s) with _ -> VStr s)
  | `Float f -> VFloat f
  | `String s -> VStr s
  | `Bool b -> VBool b
  | `Null -> VNull
  | _ -> raise (Json_error "valeur de cellule non supportée")

let json_of_value (v : value) : J.t =
  match v with
  | VInt i -> `Int i
  | VFloat f -> `Float f
  | VStr s -> `String s
  | VBool b -> `Bool b
  | VNull -> `Null

let member name obj = match obj with
  | `Assoc l -> (try List.assoc name l with Not_found -> `Null)
  | _ -> `Null

let to_list = function `List l -> l | _ -> raise (Json_error "liste attendue")
let to_str = function `String s -> s | _ -> raise (Json_error "chaîne attendue")

let to_bool ~default = function `Bool b -> b | _ -> default
let to_float ~default = function `Float f -> f | `Int i -> float_of_int i | _ -> default
let to_int ~default = function `Int i -> i | `Float f -> int_of_float f | _ -> default

(* Index déclaré : { "name", "columns": [..], "unique"?, "enabled"?, "implicit"? } *)
let index_of_json (j : J.t) : Db.index_def =
  { Db.iname = to_str (member "name" j);
    icols = to_list (member "columns" j) |> List.map to_str;
    iunique = to_bool ~default:false (member "unique" j);
    ienabled = to_bool ~default:true (member "enabled" j);
    iimplicit = to_bool ~default:false (member "implicit" j) }

(* Options de la couche physique : { "plan"?, "force"?, "consts"?: {...} } — toutes facultatives. *)
let options_of_json (j : J.t) : Physical.options =
  let d = Physical.default_options in
  let c = member "consts" j in
  let dc = d.Physical.consts in
  { Physical.with_plan = to_bool ~default:d.Physical.with_plan (member "plan" j);
    force = (match member "force" j with `String s -> Some s | _ -> None);
    scale = (match member "scale" j with `Int n when n > 0 -> Some n | `Float f when f > 0. -> Some (int_of_float f) | _ -> None);
    consts =
      { Physical.seq_page_cost = to_float ~default:dc.Physical.seq_page_cost (member "seq_page_cost" c);
        random_page_cost = to_float ~default:dc.Physical.random_page_cost (member "random_page_cost" c);
        cpu_tuple_cost = to_float ~default:dc.Physical.cpu_tuple_cost (member "cpu_tuple_cost" c);
        cpu_index_tuple_cost = to_float ~default:dc.Physical.cpu_index_tuple_cost (member "cpu_index_tuple_cost" c);
        cpu_operator_cost = to_float ~default:dc.Physical.cpu_operator_cost (member "cpu_operator_cost" c);
        rows_per_page = max 1 (to_int ~default:dc.Physical.rows_per_page (member "rows_per_page" c));
        fanout = max 2 (to_int ~default:dc.Physical.fanout (member "fanout" c)) } }

(* Base entrante :
   { "tables": [ { "name": "...", "columns": [ {"name","type"} ], "rows": [ {col: val} ],
                  "indexes"?: [ {"name","columns","unique","enabled"} ] } ],
     "options"?: { "plan", "force", "consts" } } *)
let input_of_json (s : string) : (Db.db * Physical.options, string) result =
  try
    let j = J.from_string s in
    let tables = to_list (member "tables" j) |> List.map (fun t ->
      let name = to_str (member "name" t) in
      let cols = to_list (member "columns" t) |> List.map (fun c ->
        { Db.cname = to_str (member "name" c);
          Db.cty = (match member "type" c with `String s -> s | _ -> "") }) in
      let rows = to_list (member "rows" t) |> List.map (fun r ->
        match r with
        | `Assoc pairs -> List.map (fun (k, v) -> (k, value_of_json v)) pairs
        | _ -> raise (Json_error "ligne attendue sous forme d'objet")) in
      let indexes = match member "indexes" t with `List l -> List.map index_of_json l | _ -> [] in
      { Db.tname = name; cols; rows; indexes }) in
    Ok ({ Db.tables }, options_of_json (member "options" j))
  with
  | Json_error m -> Error ("JSON invalide : " ^ m)
  | Yojson.Json_error m -> Error ("JSON mal formé : " ^ m)
  | e -> Error ("erreur de lecture de la base : " ^ Printexc.to_string e)

let db_of_json (s : string) : (Db.db, string) result = Result.map fst (input_of_json s)

let json_of_tv = function
  | True -> `String "True" | False -> `String "False" | Unknown -> `String "Unknown"

let json_of_where_row (w : Semantics.where_row) : J.t =
  `Assoc [ ("i", `Int w.idx); ("tv", json_of_tv w.tv); ("pass", `Bool w.pass) ]

let json_of_stage (s : Semantics.stage) : J.t =
  `Assoc [
    ("kind", `String s.kind);
    ("label", `String s.label);
    ("columns", `List (List.map (fun c -> `String c) s.columns));
    ("rows", `List (List.map (fun r -> `List (List.map json_of_value r)) s.rows));
    ("note", `String s.note);
    ("whereRows", match s.where_rows with
      | None -> `Null
      | Some ws -> `List (List.map json_of_where_row ws));
  ]

(* ---- Couche physique → JSON ---- *)
module P = Physical

let strs l = `List (List.map (fun s -> `String s) l)
let ints l = `List (List.map (fun i -> `Int i) l)
let vals l = `List (List.map json_of_value l)

let json_of_index_def (d : Db.index_def) : (string * J.t) list =
  [ ("name", `String d.Db.iname); ("columns", strs d.Db.icols); ("unique", `Bool d.Db.iunique);
    ("enabled", `Bool d.Db.ienabled); ("implicit", `Bool d.Db.iimplicit) ]

let reason_code = function
  | P.FuncOnCol -> "func_on_col" | P.ArithOnCol -> "arith_on_col" | P.LikeLeadingWildcard -> "like_leading_wildcard"
  | P.LikeCollation -> "like_collation" | P.NotEqual -> "not_equal" | P.Negation -> "negation" | P.NotIn -> "not_in"
  | P.IsNotNull -> "is_not_null" | P.OrAcrossCols -> "or_across_cols" | P.ColVsCol -> "col_vs_col" | P.NoColumn -> "no_column"
  | P.NotIndexed -> "not_indexed" | P.NotLeading _ -> "not_leading" | P.AfterRange _ -> "after_range"

let json_of_reason r =
  let col = match r with P.NotLeading c | P.AfterRange c -> `String c | _ -> `Null in
  `Assoc [ ("kind", `String (reason_code r)); ("column", col) ]

let json_of_bound = function
  | P.Unbounded -> `Null
  | P.Incl v -> `Assoc [ ("incl", `Bool true); ("v", json_of_value v) ]
  | P.Excl v -> `Assoc [ ("incl", `Bool false); ("v", json_of_value v) ]

let json_of_conjunct (c : P.conjunct) : J.t =
  let base = [ ("text", `String c.P.text); ("sel", `Float c.P.sel); ("obs", `Float c.P.obs) ] in
  match c.P.cls with
  | P.Sarg (col, p) ->
    let pred, extra = match p with
      | P.PEq v -> ("eq", [ ("value", json_of_value v) ])
      | P.PIn vs -> ("in", [ ("values", vals vs) ])
      | P.PRange (lo, hi) -> ("range", [ ("lo", json_of_bound lo); ("hi", json_of_bound hi) ])
      | P.PIsNull -> ("is_null", [])
      | P.PNever -> ("never", []) in
    `Assoc (base @ [ ("sargable", `Bool true); ("column", `String col); ("pred", `String pred) ] @ extra)
  | P.NotSarg r -> `Assoc (base @ [ ("sargable", `Bool false); ("reason", json_of_reason r) ])

let json_of_matching (m : P.matching) : (string * J.t) list =
  [ ("hasCond", `Bool m.P.has_cond);
    ("probes", `List (List.map (fun (p : P.probe) ->
       `Assoc [ ("prefix", vals p.P.prefix); ("lo", json_of_bound p.P.lo); ("hi", json_of_bound p.P.hi) ]) m.P.probes));
    ("constCols", strs m.P.const_cols);
    ("indexCond", ints m.P.index_cond); ("indexCheck", ints m.P.index_check); ("residual", ints m.P.residual);
    ("notApplicable", `List (List.map (fun (i, r) -> `Assoc [ ("conjunct", `Int i); ("reason", json_of_reason r) ]) m.P.not_applicable)) ]

let json_of_est (e : P.est) : J.t =
  `Assoc [ ("rows", `Float e.P.rows); ("accessCost", `Float e.P.access_cost); ("sortCost", `Float e.P.sort_cost);
           ("total", `Float e.P.total); ("formula", strs e.P.formula) ]

let json_of_path (i : int) (chosen : int) (p : P.path) : J.t =
  let kind, idx, only = match p.P.access with
    | P.SeqScan -> ("seq_scan", `Null, false)
    | P.IndexScan b -> ("index_scan", `String b.P.def.Db.iname, false)
    | P.IndexOnlyScan b -> ("index_only_scan", `String b.P.def.Db.iname, true) in
  `Assoc ([ ("kind", `String kind); ("index", idx); ("indexOnly", `Bool only);
            ("orderProvided", match p.P.order with None -> `Null | Some `Forward -> `String "forward" | Some `Backward -> `String "backward");
            ("sortNeeded", `Bool p.P.sort_needed); ("chosen", `Bool (i = chosen));
            ("est", json_of_est p.P.est);
            ("estSim", match p.P.est_sim with None -> `Null | Some e -> json_of_est e) ]
          @ json_of_matching p.P.m)

let json_of_tree (t : P.btree) : J.t =
  `Assoc [ ("fanout", `Int t.P.fanout); ("height", `Int t.P.height); ("root", `Int t.P.root);
           ("nodes", `List (Array.to_list (Array.map (fun (n : P.node) ->
              `Assoc [ ("id", `Int n.P.id); ("level", `Int n.P.level); ("first", `Int n.P.first); ("last", `Int n.P.last);
                       ("seps", `List (List.map vals n.P.seps)); ("children", ints n.P.children) ]) t.P.nodes))) ]

let json_of_built ?(scale : int option) (c : P.consts) (trees : (string * P.btree) list) (b : P.built) : J.t =
  `Assoc (json_of_index_def b.P.def
          @ [ ("heightSim", match scale with Some n -> `Int (P.sim_height c n) | None -> `Null);
              ("entries", `List (Array.to_list (Array.map (fun (e : P.entry) -> `Assoc [ ("key", vals e.P.key); ("rowid", `Int e.P.rowid) ]) b.P.entries)));
              ("uniqueViolations", `List (List.map vals b.P.unique_violations));
              ("tree", match List.assoc_opt b.P.def.Db.iname trees with Some t -> json_of_tree t | None -> `Null) ])

let json_of_consts (c : P.consts) : J.t =
  `Assoc [ ("seq_page_cost", `Float c.P.seq_page_cost); ("random_page_cost", `Float c.P.random_page_cost);
           ("cpu_tuple_cost", `Float c.P.cpu_tuple_cost); ("cpu_index_tuple_cost", `Float c.P.cpu_index_tuple_cost);
           ("cpu_operator_cost", `Float c.P.cpu_operator_cost); ("rows_per_page", `Int c.P.rows_per_page); ("fanout", `Int c.P.fanout) ]

let json_of_stats (s : P.stats) : J.t =
  `Assoc [ ("rows", `Int s.P.n); ("pages", `Int s.P.pages);
           ("columns", `List (List.map (fun (c, (st : P.col_stat)) ->
              `Assoc [ ("name", `String c); ("distinct", `Int st.P.distinct); ("nulls", `Int st.P.nulls); ("indexed", `Bool st.P.indexed);
                       ("min", match st.P.minmax with Some (mn, _) -> `Float mn | None -> `Null);
                       ("max", match st.P.minmax with Some (_, mx) -> `Float mx | None -> `Null) ]) s.P.col_stats)) ]

let plan_to_json (p : P.plan) : J.t =
  match p with
  | P.Unavailable reason -> `Assoc [ ("available", `Bool false); ("reason", `String reason) ]
  | P.Plan pl ->
    let ex = pl.P.exec and sd = pl.P.sound in
    let chosen_path = List.nth pl.P.paths pl.P.chosen in
    `Assoc [
      ("available", `Bool true); ("reason", `Null);
      ("table", `String pl.P.table.Db.tname);
      ("consts", json_of_consts pl.P.consts);
      ("stats", json_of_stats pl.P.stats);
      ("scale", match pl.P.scale with Some n -> `Int n | None -> `Null);
      ("simPages", `Int pl.P.sim_pages);
      ("obsWhere", `Float pl.P.obs_where);
      ("querySel", `Float pl.P.query_sel);
      ("conjuncts", `List (Array.to_list (Array.map json_of_conjunct pl.P.conjuncts)));
      ("indexes", `List (List.map (json_of_built ?scale:pl.P.scale pl.P.consts pl.P.trees) pl.P.indexes));
      ("curve", `List (List.map (fun (s, a, b) -> `Assoc [ ("sel", `Float s); ("seq", `Float a); ("idx", `Float b) ]) pl.P.curve));
      ("curveIndex", match pl.P.curve_index with Some n -> `String n | None -> `Null);
      ("reports", `List (List.map (fun (name, m) -> `Assoc (("index", `String name) :: json_of_matching m)) pl.P.reports));
      ("paths", `List (List.mapi (fun i p -> json_of_path i pl.P.chosen p) pl.P.paths));
      ("chosen", `Int pl.P.chosen); ("forced", `Bool pl.P.forced);
      ("exec", `Assoc [
         ("kind", `String (match chosen_path.P.access with P.SeqScan -> "seq_scan" | P.IndexScan _ -> "index_scan" | P.IndexOnlyScan _ -> "index_only_scan"));
         ("index", match chosen_path.P.access with P.SeqScan -> `Null | P.IndexScan b | P.IndexOnlyScan b -> `String b.P.def.Db.iname);
         ("descents", `List (List.map (fun (d : P.descent) ->
            `Assoc [ ("probe", vals d.P.probe); ("path", ints d.P.path); ("lo", `Int d.P.lo); ("hi", `Int d.P.hi) ]) ex.P.descents));
         ("entriesScanned", `Int ex.P.entries_scanned); ("indexPages", `Int ex.P.index_pages);
         ("stream", ints ex.P.stream); ("touched", ints ex.P.touched); ("heapPages", ints ex.P.heap_pages);
         ("candidates", `Int ex.P.candidates); ("passed", `Int ex.P.passed); ("returned", `Int ex.P.returned);
         ("earlyStop", `Bool ex.P.early_stop) ]);
      ("physRows", `List (List.map (fun row -> `List (List.map json_of_value row)) pl.P.phys.Semantics.out_rows));
      ("physPipeline", `List (List.map json_of_stage pl.P.phys.Semantics.pipeline));
      ("sound", `Assoc [ ("bagEqual", `Bool sd.P.bag_equal); ("orderKeysEqual", `Bool sd.P.order_keys_equal);
                         ("sortedOk", `Bool sd.P.sorted_ok); ("tieAmbiguity", `Bool sd.P.tie_ambiguity); ("sound", `Bool sd.P.sound) ]);
      ("warnings", strs pl.P.warnings);
    ]

let result_to_json ?(plan : P.plan option) (r : Semantics.result) : string =
  `Assoc [
    ("ok", `Bool r.ok);
    ("kind", `String "select");
    ("error", match r.error with None -> `Null | Some e -> `String e);
    ("columns", `List (List.map (fun c -> `String c) r.columns));
    ("rows", `List (List.map (fun row -> `List (List.map json_of_value row)) r.out_rows));
    ("pipeline", `List (List.map json_of_stage r.pipeline));
    ("plan", match plan with None -> `Null | Some p -> plan_to_json p);
    ("ddl", `Null);
  ] |> J.to_string

(* Résultat d'un DDL : le moteur valide et normalise ; c'est l'app qui applique (sur clic). *)
let ddl_to_json (d : Typecheck.ddl_result) (warnings : string list) : string =
  let op, table, index, summary =
    match d with
    | Typecheck.DdlCreate (t, i) ->
      ("create_index", t.Db.tname, i,
       Printf.sprintf "CREATE %sINDEX %s ON %s (%s)" (if i.Db.iunique then "UNIQUE " else "") i.Db.iname t.Db.tname (String.concat ", " i.Db.icols))
    | Typecheck.DdlDrop (t, i) -> ("drop_index", t.Db.tname, i, Printf.sprintf "DROP INDEX %s" i.Db.iname)
  in
  `Assoc [
    ("ok", `Bool true); ("kind", `String "ddl"); ("error", `Null);
    ("columns", `List []); ("rows", `List []); ("pipeline", `List []); ("plan", `Null);
    ("ddl", `Assoc [ ("op", `String op); ("table", `String table); ("index", `Assoc (json_of_index_def index));
                     ("summary", `String summary); ("warnings", strs warnings) ]);
  ] |> J.to_string

let error_json ?(pos = -1) (msg : string) : string =
  `Assoc [
    ("ok", `Bool false);
    ("kind", `String "error");
    ("error", `String msg);
    ("errorPos", `Int pos);
    ("columns", `List []);
    ("rows", `List []);
    ("pipeline", `List []);
    ("plan", `Null);
    ("ddl", `Null);
  ] |> J.to_string
