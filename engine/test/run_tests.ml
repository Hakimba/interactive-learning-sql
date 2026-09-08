(* run_tests.ml — Tests du moteur :
   1) tests "golden" (exemples précis, sémantique attendue)
   2) tests de propriétés (QCheck) : lois algébriques et 3VL
   Le différentiel vs SQLite se fait côté Node (voir ../../app + tools). *)

open Sqlengine
open Value

let failures = ref 0
let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (incr failures; Printf.printf "FAIL   %s\n" name)

let col n t = { Db.cname = n; cty = t }
let mkdb tables = { Db.tables }
let tbl name cols rows = { Db.tname = name; cols; rows; indexes = [] }

let clients =
  tbl "clients"
    [ col "id" "INTEGER"; col "nom" "VARCHAR"; col "ville" "VARCHAR"; col "age" "INTEGER"; col "actif" "BOOLEAN" ]
    [ [ ("id", VInt 1); ("nom", VStr "Alice"); ("ville", VStr "Paris"); ("age", VInt 30); ("actif", VBool true) ];
      [ ("id", VInt 2); ("nom", VStr "Bob");   ("ville", VStr "Lyon");  ("age", VInt 45); ("actif", VBool false) ];
      [ ("id", VInt 3); ("nom", VStr "Chloé"); ("ville", VStr "Paris"); ("age", VInt 25); ("actif", VBool true) ];
      [ ("id", VInt 4); ("nom", VStr "David"); ("ville", VStr "Nice");  ("age", VNull);   ("actif", VBool true) ];
      [ ("id", VInt 5); ("nom", VStr "Emma");  ("ville", VStr "Lyon");  ("age", VInt 52); ("actif", VBool false) ] ]

let db = mkdb [ clients ]

(* Exécute une requête, renvoie le résultat ou signale un échec. *)
let run_ok sql =
  match Engine.run sql db with
  | Ok r -> r
  | Error (m, _) -> incr failures; Printf.printf "FAIL (erreur inattendue) %s -> %s\n" sql m;
    { Semantics.ok = false; error = Some m; columns = []; out_rows = []; pipeline = [] }

let nrows sql = List.length (run_ok sql).Semantics.out_rows
let first_cell sql = match (run_ok sql).Semantics.out_rows with (v :: _) :: _ -> Some v | _ -> None

(* ---------------- 1) Tests golden ---------------- *)
let golden () =
  Printf.printf "\n== Golden ==\n";
  check "SELECT *" ((run_ok "SELECT * FROM clients").columns = [ "id"; "nom"; "ville"; "age"; "actif" ] && nrows "SELECT * FROM clients" = 5);
  check "alias" ((run_ok "SELECT nom AS n, age FROM clients").columns = [ "n"; "age" ]);
  check "WHERE =" (nrows "SELECT nom FROM clients WHERE ville = 'Paris'" = 2);
  check "WHERE AND" (nrows "SELECT nom FROM clients WHERE ville = 'Lyon' AND actif = false" = 2);
  check "WHERE OR" (nrows "SELECT nom FROM clients WHERE ville = 'Nice' OR age > 50" = 2);
  check "WHERE NOT (bool)" (nrows "SELECT nom FROM clients WHERE NOT actif" = 2);
  check "IN" (nrows "SELECT nom FROM clients WHERE ville IN ('Paris','Nice')" = 3);
  check "NOT IN" (nrows "SELECT nom FROM clients WHERE ville NOT IN ('Paris')" = 3);
  check "LIKE" (nrows "SELECT nom FROM clients WHERE nom LIKE 'A%'" = 1);
  check "IS NULL" (first_cell "SELECT nom FROM clients WHERE age IS NULL" = Some (VStr "David"));
  check "IS NOT NULL" (nrows "SELECT nom FROM clients WHERE age IS NOT NULL" = 4);
  check "BETWEEN" (nrows "SELECT nom FROM clients WHERE age BETWEEN 26 AND 46" = 2);
  (* 3VL : NULL n'est jamais > 10, donc David (age NULL) est exclu *)
  check "NULL en comparaison exclut" (nrows "SELECT nom FROM clients WHERE age > 10" = 4);
  check "NOT IN avec NULL -> Unknown exclut" (nrows "SELECT id FROM clients WHERE age NOT IN (30, 45)" = 2);
  check "arithmétique" (first_cell "SELECT age * 2 AS d FROM clients WHERE id = 1" = Some (VInt 60));
  check "fonction upper" (first_cell "SELECT upper(ville) FROM clients WHERE id = 1" = Some (VStr "PARIS"));
  check "coalesce sur NULL" (first_cell "SELECT coalesce(age, -1) AS a FROM clients WHERE id = 4" = Some (VInt (-1)));
  check "ORDER BY DESC" (first_cell "SELECT nom FROM clients WHERE age IS NOT NULL ORDER BY age DESC" = Some (VStr "Emma"));
  check "ORDER BY NULLS FIRST" (first_cell "SELECT nom FROM clients ORDER BY age" = Some (VStr "David"));
  check "LIMIT/OFFSET" (nrows "SELECT id FROM clients ORDER BY id LIMIT 2 OFFSET 1" = 2 && first_cell "SELECT id FROM clients ORDER BY id LIMIT 2 OFFSET 1" = Some (VInt 2));
  check "DISTINCT" (nrows "SELECT DISTINCT ville FROM clients" = 3);
  (* trace : le WHERE évalue bien les 5 lignes et en garde 2 *)
  (let r = run_ok "SELECT nom FROM clients WHERE ville = 'Paris'" in
   let w = List.find_opt (fun s -> s.Semantics.kind = "where") r.pipeline in
   check "trace WHERE"
     (match w with Some s -> (match s.where_rows with Some ws -> List.length ws = 5 && List.length (List.filter (fun x -> x.Semantics.pass) ws) = 2 | None -> false) | None -> false));
  (* erreurs (parse + typage) *)
  check "erreur table" (match Engine.run "SELECT * FROM inconnue" db with Error _ -> true | _ -> false);
  check "erreur colonne" (match Engine.run "SELECT xyz FROM clients" db with Error _ -> true | _ -> false);
  check "erreur syntaxe" (match Engine.run "SELECT FROM clients" db with Error _ -> true | _ -> false);
  check "erreur sans SELECT" (match Engine.run "FROM clients" db with Error (m, _) -> String.length m > 0 | _ -> false);
  (* erreurs de TYPE (nouveauté du cœur typé) *)
  check "type: age (num) = texte" (match Engine.run "SELECT * FROM clients WHERE age = 'Paris'" db with Error _ -> true | _ -> false);
  check "type: nom (texte) + 1" (match Engine.run "SELECT nom + 1 FROM clients" db with Error _ -> true | _ -> false);
  check "type: NOT sur un nombre est ok via = " (match Engine.run "SELECT * FROM clients WHERE actif" db with Ok _ -> true | _ -> false);
  check "type: LIKE sur un nombre rejeté" (match Engine.run "SELECT * FROM clients WHERE age LIKE '3%'" db with Error _ -> true | _ -> false)

(* ---------------- 2) Propriétés QCheck ---------------- *)
let cols3 = [ col "a" "INTEGER"; col "b" "INTEGER"; col "s" "VARCHAR" ]
let db_of_rows rows = mkdb [ tbl "t" cols3 rows ]

let gen_row =
  let open QCheck.Gen in
  let gen_a = oneof_weighted [ (1, return VNull); (4, map (fun i -> VInt i) (int_range 0 5)) ] in
  map3 (fun a b s -> [ ("a", a); ("b", VInt b); ("s", VStr s) ])
    gen_a (int_range 0 5) (oneof_list [ "x"; "y"; "z" ])

let gen_case =
  let open QCheck.Gen in
  triple (list_size (int_range 0 12) gen_row) (int_range 0 5) (int_range 1 6)

let arb_case = QCheck.make gen_case

let qcount sql db =
  match Engine.run sql db with
  | Ok r -> List.length r.Semantics.out_rows
  | Error _ -> -1

let run_prop name t =
  match QCheck.Test.check_exn t with
  | () -> Printf.printf "  ok   %s\n" name
  | exception e -> incr failures; Printf.printf "FAIL   %s : %s\n" name (Printexc.to_string e)

let props () =
  Printf.printf "\n== Propriétés (QCheck) ==\n";
  (* P1 : ajouter une conjonction ne peut que réduire (monotonie de σ) *)
  run_prop "P1 monotonie WHERE (AND réduit)"
    (QCheck.Test.make ~count:300 ~name:"mono" arb_case (fun (rows, k, _) ->
       let db = db_of_rows rows in
       let c1 = qcount (Printf.sprintf "SELECT * FROM t WHERE b > %d AND b < %d" k (k + 1)) db in
       let c2 = qcount (Printf.sprintf "SELECT * FROM t WHERE b > %d" k) db in
       c1 >= 0 && c2 >= 0 && c1 <= c2));
  (* P2 : DISTINCT ne peut qu'égaler ou réduire le nombre de lignes *)
  run_prop "P2 |DISTINCT b| <= |b|"
    (QCheck.Test.make ~count:300 ~name:"distinct" arb_case (fun (rows, _, _) ->
       let db = db_of_rows rows in
       let d = qcount "SELECT DISTINCT b FROM t" db in
       let a = qcount "SELECT b FROM t" db in
       d >= 0 && a >= 0 && d <= a && a = List.length rows));
  (* P3 : LIMIT n renvoie au plus n lignes *)
  run_prop "P3 LIMIT borne"
    (QCheck.Test.make ~count:300 ~name:"limit" arb_case (fun (rows, _, n) ->
       let db = db_of_rows rows in
       qcount (Printf.sprintf "SELECT * FROM t LIMIT %d" n) db <= n));
  (* P4 : NULL exclu par l'égalité (3VL) — count(a = k) = #lignes avec a = VInt k *)
  run_prop "P4 NULL exclu par ="
    (QCheck.Test.make ~count:400 ~name:"null-eq" arb_case (fun (rows, k, _) ->
       let db = db_of_rows rows in
       let got = qcount (Printf.sprintf "SELECT * FROM t WHERE a = %d" k) db in
       let expected = List.length (List.filter (fun r -> List.assoc "a" r = VInt k) rows) in
       got = expected));
  (* P5 : sur une colonne non-nulle, p et NOT p partitionnent (somme = total) *)
  run_prop "P5 complément (b non-null partitionne)"
    (QCheck.Test.make ~count:400 ~name:"complement" arb_case (fun (rows, k, _) ->
       let db = db_of_rows rows in
       let p = qcount (Printf.sprintf "SELECT * FROM t WHERE b > %d" k) db in
       let np = qcount (Printf.sprintf "SELECT * FROM t WHERE NOT b > %d" k) db in
       p >= 0 && np >= 0 && p + np = List.length rows))

(* ---------------- Tests JOIN ---------------- *)
let clients2 =
  tbl "clients" [ col "id" "INTEGER"; col "nom" "VARCHAR" ]
    [ [ ("id", VInt 1); ("nom", VStr "Alice") ];
      [ ("id", VInt 2); ("nom", VStr "Bob") ];
      [ ("id", VInt 3); ("nom", VStr "Chloé") ] ]   (* Chloé n'a pas de commande *)

let commandes2 =
  tbl "commandes" [ col "id" "INTEGER"; col "client_id" "INTEGER"; col "montant" "INTEGER" ]
    [ [ ("id", VInt 10); ("client_id", VInt 1); ("montant", VInt 100) ];  (* Alice *)
      [ ("id", VInt 11); ("client_id", VInt 1); ("montant", VInt 50) ];   (* Alice (fan-out) *)
      [ ("id", VInt 12); ("client_id", VInt 2); ("montant", VInt 75) ];   (* Bob *)
      [ ("id", VInt 13); ("client_id", VInt 99); ("montant", VInt 20) ] ] (* client inexistant *)

let jdb = mkdb [ clients2; commandes2 ]
let jn sql =
  match Engine.run sql jdb with
  | Ok r -> List.length r.Semantics.out_rows
  | Error (m, _) -> incr failures; Printf.printf "JOIN FAIL %s -> %s\n" sql m; -1
let jerr sql = match Engine.run sql jdb with Error _ -> true | _ -> false

let golden_joins () =
  Printf.printf "\n== Golden JOIN ==\n";
  let on = "ON clients.id = commandes.client_id" in
  check "INNER JOIN (fan-out)" (jn (Printf.sprintf "SELECT nom, montant FROM clients JOIN commandes %s" on) = 3);
  check "LEFT JOIN (orphelin gardé)" (jn (Printf.sprintf "SELECT nom, montant FROM clients LEFT JOIN commandes %s" on) = 4);
  check "RIGHT JOIN" (jn (Printf.sprintf "SELECT nom, montant FROM clients RIGHT JOIN commandes %s" on) = 4);
  check "FULL JOIN" (jn (Printf.sprintf "SELECT nom, montant FROM clients FULL JOIN commandes %s" on) = 5);
  check "CROSS JOIN" (jn "SELECT nom, montant FROM clients CROSS JOIN commandes" = 12);
  check "virgule = CROSS" (jn "SELECT nom, montant FROM clients, commandes" = 12);
  check "alias de table" (jn "SELECT c.nom, o.montant FROM clients c JOIN commandes o ON c.id = o.client_id" = 3);
  check "colonne ambiguë -> erreur" (jerr (Printf.sprintf "SELECT id FROM clients JOIN commandes %s" on));
  check "colonne qualifiée ok" (jn (Printf.sprintf "SELECT clients.id FROM clients JOIN commandes %s" on) = 3);
  check "WHERE après JOIN" (jn (Printf.sprintf "SELECT nom FROM clients JOIN commandes %s WHERE montant > 60" on) = 2);
  (* LEFT JOIN : Chloé apparaît avec un montant NULL *)
  let left = match Engine.run (Printf.sprintf "SELECT nom, montant FROM clients LEFT JOIN commandes %s" on) jdb with
    | Ok r -> r.Semantics.out_rows | _ -> [] in
  check "LEFT JOIN remplit NULL" (List.exists (function [ VStr "Chloé"; VNull ] -> true | _ -> false) left)

(* ---------------- Tests couche PHYSIQUE (index) ---------------- *)
module P = Physical

let idx ?(unique = false) ?(enabled = true) ?(implicit = false) name cols =
  { Db.iname = name; icols = cols; iunique = unique; ienabled = enabled; iimplicit = implicit }
let tbl_i name cols rows indexes = { Db.tname = name; cols; rows; indexes }

let prow a b s = [ ("a", a); ("b", VInt b); ("s", VStr s) ]
let prows = [ prow VNull 1 "x"; prow (VInt 1) 2 "y"; prow (VInt 2) 3 "x"; prow (VInt 2) 1 "z"; prow (VInt 3) 2 "x";
              prow (VInt 3) 5 "y"; prow (VInt 3) 0 "z"; prow (VInt 4) 4 "x"; prow (VInt 5) 1 "y"; prow VNull 2 "z" ]
let ptbl = tbl_i "t" cols3 prows [ idx "i_a" [ "a" ]; idx "i_ab" [ "a"; "b" ]; idx "i_s" [ "s" ] ]
let pdb = mkdb [ ptbl ]

let plan_of ?force ?(db = pdb) sql =
  match Engine.run_plan { P.default_options with P.force } sql db with
  | Ok (_, P.Plan p) -> Some p
  | Ok (_, P.Unavailable m) -> incr failures; Printf.printf "FAIL (plan indisponible) %s -> %s\n" sql m; None
  | Error (m, _) -> incr failures; Printf.printf "FAIL (erreur) %s -> %s\n" sql m; None

let chosen p = List.nth p.P.paths p.P.chosen
let kind_of p =
  match (chosen p).P.access with
  | P.SeqScan -> "seq" | P.IndexScan b -> "idx:" ^ b.P.def.Db.iname | P.IndexOnlyScan b -> "only:" ^ b.P.def.Db.iname
let path_named p name = List.find_opt (fun x -> P.index_name x.P.access = name) p.P.paths
let report p name = List.assoc_opt name p.P.reports
let cls p i = if i < Array.length p.P.conjuncts then Some p.P.conjuncts.(i).P.cls else None
let with_plan sql f = match plan_of sql with Some p -> f p | None -> false
let with_forced name sql f = match plan_of ~force:name sql with Some p -> f p | None -> false
let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  m = 0 || go 0

let golden_physical () =
  Printf.printf "\n== Golden INDEX (couche physique) ==\n";
  (* construction de l'index *)
  (match P.build_index ptbl (idx "i_a" [ "a" ]) with
   | Ok b ->
     check "build : NULL en tête, clés croissantes, ex æquo par rowid"
       (Array.to_list (Array.map (fun e -> (e.P.key, e.P.rowid)) b.P.entries)
        = [ ([ VNull ], 0); ([ VNull ], 9); ([ VInt 1 ], 1); ([ VInt 2 ], 2); ([ VInt 2 ], 3); ([ VInt 3 ], 4);
            ([ VInt 3 ], 5); ([ VInt 3 ], 6); ([ VInt 4 ], 7); ([ VInt 5 ], 8) ]);
     check "build : violations d'unicité détectées sans exception (2 et 3)" (List.length b.P.unique_violations = 2);
     let t = P.build_tree ~fanout:4 b.P.entries in
     check "tree : 10 entrées, fanout 4 → 3 feuilles + racine, hauteur 2"
       (Array.length t.P.nodes = 4 && t.P.height = 2 && List.length t.P.nodes.(t.P.root).P.seps = 2);
     check "tree : descente vers la position 6 = racine → 2e feuille" (P.path_to t 6 = [ t.P.root; 1 ])
   | Error m -> incr failures; Printf.printf "FAIL build_index : %s\n" m);
  check "build : colonne inconnue → erreur propre" (match P.build_index ptbl (idx "bad" [ "zz" ]) with Error _ -> true | Ok _ -> false);
  (* chaîne dans une colonne numérique : normalisée comme le WHERE (→ NULL) *)
  (let t2 = tbl_i "t" cols3 [ prow (VStr "abc") 1 "x"; prow (VInt 1) 1 "x" ] [] in
   match P.build_index t2 (idx "i" [ "a" ]) with
   | Ok b -> check "build : chaîne dans colonne numérique → bloc NULL" (b.P.entries.(0).P.key = [ VNull ] && b.P.entries.(0).P.rowid = 0)
   | Error _ -> check "build : chaîne dans colonne numérique" false);
  (* classification des conjoints *)
  let is_eq3 = function Some (P.Sarg ("a", P.PEq (VInt 3))) -> true | _ -> false in
  check "classify : a = 3" (with_plan "SELECT * FROM t WHERE a = 3" (fun p -> is_eq3 (cls p 0)));
  check "classify : 3 = a (retourné)" (with_plan "SELECT * FROM t WHERE 3 = a" (fun p -> is_eq3 (cls p 0)));
  check "classify : a > 2 → intervalle ouvert"
    (with_plan "SELECT * FROM t WHERE a > 2" (fun p -> match cls p 0 with Some (P.Sarg ("a", P.PRange (P.Excl (VInt 2), P.Unbounded))) -> true | _ -> false));
  check "classify : 2 >= a ≡ a <= 2"
    (with_plan "SELECT * FROM t WHERE 2 >= a" (fun p -> match cls p 0 with Some (P.Sarg ("a", P.PRange (P.Unbounded, P.Incl (VInt 2)))) -> true | _ -> false));
  check "classify : BETWEEN → intervalle fermé"
    (with_plan "SELECT * FROM t WHERE a BETWEEN 1 AND 3" (fun p -> match cls p 0 with Some (P.Sarg ("a", P.PRange (P.Incl (VInt 1), P.Incl (VInt 3)))) -> true | _ -> false));
  check "classify : IN (NULL retiré, trié)"
    (with_plan "SELECT * FROM t WHERE a IN (2, NULL, 1)" (fun p -> match cls p 0 with Some (P.Sarg ("a", P.PIn [ VInt 1; VInt 2 ])) -> true | _ -> false));
  check "classify : a = NULL → jamais vrai, 0 candidat, sound"
    (with_forced "i_a" "SELECT * FROM t WHERE a = NULL"
       (fun p -> (match cls p 0 with Some (P.Sarg ("a", P.PNever)) -> true | _ -> false) && p.P.exec.P.candidates = 0 && p.P.sound.P.sound));
  check "classify : a = 1 OR a = 2 ≡ IN"
    (with_plan "SELECT * FROM t WHERE a = 1 OR a = 2" (fun p -> match cls p 0 with Some (P.Sarg ("a", P.PIn [ VInt 1; VInt 2 ])) -> true | _ -> false));
  check "classify : a = 1 OR b = 2 → OR multi-colonnes" (with_plan "SELECT * FROM t WHERE a = 1 OR b = 2" (fun p -> cls p 0 = Some (P.NotSarg P.OrAcrossCols)));
  check "classify : <>" (with_plan "SELECT * FROM t WHERE a <> 2" (fun p -> cls p 0 = Some (P.NotSarg P.NotEqual)));
  check "classify : NOT" (with_plan "SELECT * FROM t WHERE NOT a = 2" (fun p -> cls p 0 = Some (P.NotSarg P.Negation)));
  check "classify : NOT IN" (with_plan "SELECT * FROM t WHERE a NOT IN (1)" (fun p -> cls p 0 = Some (P.NotSarg P.NotIn)));
  check "classify : IS NOT NULL" (with_plan "SELECT * FROM t WHERE a IS NOT NULL" (fun p -> cls p 0 = Some (P.NotSarg P.IsNotNull)));
  check "classify : IS NULL sargable → bloc NULL (rowids 0 et 9)"
    (with_forced "i_a" "SELECT * FROM t WHERE a IS NULL"
       (fun p -> cls p 0 = Some (P.Sarg ("a", P.PIsNull)) && p.P.exec.P.stream = [ 0; 9 ] && p.P.sound.P.sound));
  check "classify : LIKE 'x%' → collation" (with_plan "SELECT * FROM t WHERE s LIKE 'x%'" (fun p -> cls p 0 = Some (P.NotSarg P.LikeCollation)));
  check "classify : LIKE '%x' → joker en tête" (with_plan "SELECT * FROM t WHERE s LIKE '%x'" (fun p -> cls p 0 = Some (P.NotSarg P.LikeLeadingWildcard)));
  check "classify : upper(s) = 'X' → fonction sur colonne" (with_plan "SELECT * FROM t WHERE upper(s) = 'X'" (fun p -> cls p 0 = Some (P.NotSarg P.FuncOnCol)));
  check "classify : a + 1 = 3 → arithmétique" (with_plan "SELECT * FROM t WHERE a + 1 = 3" (fun p -> cls p 0 = Some (P.NotSarg P.ArithOnCol)));
  check "classify : a = b → colonne vs colonne" (with_plan "SELECT * FROM t WHERE a = b" (fun p -> cls p 0 = Some (P.NotSarg P.ColVsCol)));
  check "classify : a = 1 + 1 → constante repliée" (with_plan "SELECT * FROM t WHERE a = 1 + 1" (fun p -> match cls p 0 with Some (P.Sarg ("a", P.PEq (VInt 2))) -> true | _ -> false));
  (* préfixe gauche (PG 11.3) sur l'index (a, b) *)
  check "préfixe : b = 1 AND a = 2 → les deux bornent"
    (with_plan "SELECT * FROM t WHERE b = 1 AND a = 2" (fun p -> match report p "i_ab" with Some m -> List.sort compare m.P.index_cond = [ 0; 1 ] && m.P.index_check = [] | None -> false));
  check "préfixe : a > 1 AND b = 2 → a borne, b vérifié dans l'index (après l'intervalle)"
    (with_plan "SELECT * FROM t WHERE a > 1 AND b = 2"
       (fun p -> match report p "i_ab" with Some m -> m.P.index_cond = [ 0 ] && m.P.index_check = [ 1 ] && List.mem (1, P.AfterRange "b") m.P.not_applicable | None -> false));
  check "préfixe : b = 2 seul → non applicable (pas en tête)"
    (with_plan "SELECT * FROM t WHERE b = 2" (fun p -> match report p "i_ab" with Some m -> (not m.P.has_cond) && List.mem (0, P.NotLeading "b") m.P.not_applicable | None -> false));
  check "préfixe : a = 2 AND b > 1 AND b < 3 → 3 conjoints bornent"
    (with_plan "SELECT * FROM t WHERE a = 2 AND b > 1 AND b < 3" (fun p -> match report p "i_ab" with Some m -> List.length m.P.index_cond = 3 | None -> false));
  check "préfixe : colonne hors index → résiduel"
    (with_plan "SELECT * FROM t WHERE a = 2 AND s = 'x'"
       (fun p -> match report p "i_ab" with Some m -> m.P.index_cond = [ 0 ] && m.P.residual = [ 1 ] && List.mem (1, P.NotIndexed) m.P.not_applicable | None -> false));
  (* index couvrant (PG 11.9) *)
  check "couvrant : SELECT a, b WHERE a = 2 → Index Only Scan"
    (with_plan "SELECT a, b FROM t WHERE a = 2" (fun p -> match path_named p "i_ab" with Some { P.access = P.IndexOnlyScan _; _ } -> true | _ -> false));
  check "couvrant : SELECT * → Index Scan"
    (with_plan "SELECT * FROM t WHERE a = 2" (fun p -> match path_named p "i_ab" with Some { P.access = P.IndexScan _; _ } -> true | _ -> false));
  (* ordre fourni par l'index (PG 11.4) *)
  let order_of p name = Option.map (fun x -> x.P.order) (path_named p name) in
  check "ordre : ORDER BY a → parcours avant" (with_plan "SELECT * FROM t ORDER BY a" (fun p -> order_of p "i_a" = Some (Some `Forward)));
  check "ordre : ORDER BY a DESC → parcours arrière" (with_plan "SELECT * FROM t ORDER BY a DESC" (fun p -> order_of p "i_a" = Some (Some `Backward)));
  check "ordre : a = 2 ORDER BY b → (a, b) fournit l'ordre" (with_plan "SELECT * FROM t WHERE a = 2 ORDER BY b" (fun p -> order_of p "i_ab" = Some (Some `Forward)));
  check "ordre : a > 1 ORDER BY b → tri nécessaire"
    (with_plan "SELECT * FROM t WHERE a > 1 ORDER BY b" (fun p -> match path_named p "i_ab" with Some x -> x.P.order = None && x.P.sort_needed | None -> true));
  check "ordre : ORDER BY a, b DESC → tri (directions mixtes)"
    (with_plan "SELECT * FROM t ORDER BY a, b DESC" (fun p -> match path_named p "i_ab" with Some x -> x.P.sort_needed | None -> true));
  check "ordre : ORDER BY alias → tri" (with_plan "SELECT a AS x FROM t ORDER BY x" (fun p -> match path_named p "i_a" with Some x -> x.P.sort_needed | None -> true));
  check "ordre : parcours arrière = sortie DESC (NULL en fin), sans tri, sound"
    (with_forced "i_a" "SELECT a FROM t ORDER BY a DESC"
       (fun p -> p.P.sound.P.sound && not (chosen p).P.sort_needed
                 && p.P.phys.Semantics.out_rows = [ [ VInt 5 ]; [ VInt 4 ]; [ VInt 3 ]; [ VInt 3 ]; [ VInt 3 ]; [ VInt 2 ]; [ VInt 2 ]; [ VInt 1 ]; [ VNull ]; [ VNull ] ]));
  (* exécution exacte *)
  check "exec : a = 3 via i_a → rowids 4,5,6 ; 3 entrées ; page 1 du tas"
    (with_forced "i_a" "SELECT * FROM t WHERE a = 3"
       (fun p -> let e = p.P.exec in e.P.candidates = 3 && e.P.stream = [ 4; 5; 6 ] && e.P.touched = [ 4; 5; 6 ] && e.P.entries_scanned = 3
                 && e.P.passed = 3 && e.P.returned = 3 && e.P.heap_pages = [ 1 ] && List.length e.P.descents = 1 && p.P.sound.P.sound));
  check "exec : Seq Scan touche tout (pages 0,1,2)" (with_forced "seq_scan" "SELECT * FROM t WHERE a = 3" (fun p -> p.P.exec.P.candidates = 10 && p.P.exec.P.heap_pages = [ 0; 1; 2 ]));
  check "exec : Index Only Scan ne touche pas le tas"
    (with_forced "i_ab" "SELECT a, b FROM t WHERE a = 3" (fun p -> p.P.exec.P.touched = [] && p.P.exec.P.heap_pages = [] && p.P.exec.P.candidates = 3 && p.P.sound.P.sound));
  check "exec : IN → une descente par valeur" (with_forced "i_a" "SELECT * FROM t WHERE a IN (1, 4)" (fun p -> List.length p.P.exec.P.descents = 2 && p.P.exec.P.stream = [ 1; 7 ] && p.P.sound.P.sound));
  check "exec : arrêt anticipé ORDER BY a LIMIT 2 via i_a"
    (with_forced "i_a" "SELECT * FROM t WHERE a >= 1 ORDER BY a LIMIT 2" (fun p -> p.P.exec.P.early_stop && p.P.exec.P.candidates = 2 && p.P.exec.P.returned = 2 && p.P.sound.P.sound));
  check "exec : arrêt anticipé Seq Scan LIMIT 2 sans ORDER BY" (with_forced "seq_scan" "SELECT * FROM t LIMIT 2" (fun p -> p.P.exec.P.early_stop && p.P.exec.P.candidates = 2));
  check "exec : DISTINCT → pas d'arrêt anticipé" (with_forced "seq_scan" "SELECT DISTINCT a FROM t LIMIT 2" (fun p -> (not p.P.exec.P.early_stop) && p.P.exec.P.candidates = 10));
  check "exec : tri nécessaire → pas d'arrêt anticipé"
    (with_forced "i_a" "SELECT * FROM t WHERE a >= 1 ORDER BY b LIMIT 2" (fun p -> (not p.P.exec.P.early_stop) && (chosen p).P.sort_needed && p.P.sound.P.sound));
  (* coût *)
  check "coût : Seq Scan 10 lignes = 3×1.0 + 10×0.01 + 10×1×0.0025 = 3.125"
    (with_plan "SELECT * FROM t WHERE a = 3" (fun p -> match path_named p "seq_scan" with Some x -> Float.abs (x.P.est.P.total -. 3.125) < 1e-9 | None -> false));
  check "coût : 10 lignes → Seq Scan choisi" (with_plan "SELECT * FROM t WHERE a = 3" (fun p -> kind_of p = "seq" && not p.P.forced));
  check "coût : forcer → index choisi, forced" (with_forced "i_a" "SELECT * FROM t WHERE a = 3" (fun p -> kind_of p = "idx:i_a" && p.P.forced));
  (let big = List.init 120 (fun i -> [ ("a", VInt i); ("b", VInt (i mod 7)); ("s", VStr "x") ]) in
   let bdb = mkdb [ tbl_i "t" cols3 big [ idx ~unique:true ~implicit:true "t_pkey" [ "a" ] ] ] in
   check "coût : 120 lignes, clé unique → Index Scan choisi, 1 candidat"
     (match plan_of ~db:bdb "SELECT * FROM t WHERE a = 42" with Some p -> kind_of p = "idx:t_pkey" && p.P.exec.P.candidates = 1 && p.P.sound.P.sound | None -> false);
   check "coût : 120 lignes → 30 pages, arbre de hauteur 4"
     (match plan_of ~db:bdb "SELECT * FROM t WHERE a = 42" with Some p -> p.P.stats.P.pages = 30 && (List.assoc "t_pkey" p.P.trees).P.height = 4 | None -> false));
  (* ex æquo *)
  check "sound : LIMIT + ex æquo → lignes différentes mais valides (tie_ambiguity)"
    (with_forced "i_ab" "SELECT s FROM t WHERE a = 3 ORDER BY a LIMIT 1" (fun p -> p.P.sound.P.tie_ambiguity && p.P.sound.P.sound));
  check "index désactivé → construit mais aucun chemin"
    (let db = mkdb [ tbl_i "t" cols3 prows [ idx ~enabled:false "i_a" [ "a" ] ] ] in
     match plan_of ~db "SELECT * FROM t WHERE a = 3" with Some p -> List.length p.P.paths = 1 && p.P.reports = [] && List.length p.P.indexes = 1 | None -> false);
  check "jointure → plan indisponible"
    (match Engine.run_plan P.default_options "SELECT * FROM clients c JOIN commandes o ON c.id = o.client_id" jdb with Ok (_, P.Unavailable _) -> true | _ -> false);
  check "unique violé → avertissement (pas d'exception)"
    (let db = mkdb [ tbl_i "t" cols3 prows [ idx ~unique:true "u_a" [ "a" ] ] ] in match plan_of ~db "SELECT * FROM t" with Some p -> p.P.warnings <> [] | None -> false);
  (* ---- échelle simulée : la table sert d'échantillon ---- *)
  let plan_scale n sql = match Engine.run_plan { P.default_options with P.scale = Some n } sql pdb with Ok (_, P.Plan p) -> Some p | _ -> None in
  check "obs : sélectivité observée exacte (a = 3 → 3/10)" (with_plan "SELECT * FROM t WHERE a = 3" (fun p -> Float.abs (p.P.conjuncts.(0).P.obs -. 0.3) < 1e-9 && Float.abs (p.P.obs_where -. 0.3) < 1e-9));
  check "échelle : hauteur simulée (120 → 4, 10 → 2, 4 → 1, 16 → 2, 100 000 → 9)"
    (P.sim_height P.default_consts 120 = 4 && P.sim_height P.default_consts 10 = 2 && P.sim_height P.default_consts 4 = 1 && P.sim_height P.default_consts 16 = 2
     && P.sim_height P.default_consts 100000 = 9);
  check "échelle : sans scale, pas d'est_sim ; avec scale, est_sim partout"
    (with_plan "SELECT * FROM t WHERE a = 3" (fun p -> List.for_all (fun x -> x.P.est_sim = None) p.P.paths)
     && (match plan_scale 100000 "SELECT * FROM t WHERE a = 3" with Some p -> p.P.scale = Some 100000 && List.for_all (fun x -> x.P.est_sim <> None) p.P.paths | None -> false));
  check "échelle : a = 3 (30 % observé) à 100 000 lignes → Seq Scan reste choisi (peu sélectif)"
    (match plan_scale 100000 "SELECT * FROM t WHERE a = 3" with Some p -> kind_of p = "seq" && not p.P.forced | None -> false);
  check "échelle : b = 5 (10 % observé) à 100 000 lignes → est_sim rows = 10 000"
    (match plan_scale 100000 "SELECT * FROM t WHERE b = 5" with
     | Some p -> (match path_named p "seq_scan" with Some x -> (match x.P.est_sim with Some e -> Float.abs (e.P.rows -. 10000.) < 1. | None -> false) | None -> false)
     | None -> false);
  (let udb = mkdb [ tbl_i "t" cols3 (List.init 10 (fun i -> prow (VInt i) i "x")) [ idx ~unique:true "i_a" [ "a" ] ] ] in
   let ps n sql = match Engine.run_plan { P.default_options with P.scale = Some n } sql udb with Ok (_, P.Plan p) -> Some p | _ -> None in
   check "échelle : clé unique dans l'échantillon → 1/N à l'échelle, Index Scan choisi à 100 000 lignes (Seq Scan sur les 10 réelles)"
     (match ps 100000 "SELECT * FROM t WHERE a = 3", plan_of ~db:udb "SELECT * FROM t WHERE a = 3" with
      | Some p, Some q -> kind_of p = "idx:i_a" && kind_of q = "seq"
                          && (match path_named p "i_a" with Some x -> (match x.P.est_sim with Some e -> e.P.rows = 1. | None -> false) | None -> false)
                          && p.P.exec.P.candidates = 1 && p.P.sound.P.sound
      | _ -> false));
  check "échelle : courbe de bascule (41 points, coûts croissants avec la sélectivité, index moins cher aux faibles sélectivités)"
    (match plan_scale 100000 "SELECT * FROM t WHERE a = 3" with
     | Some p ->
       let c = p.P.curve in
       List.length c = 41 && p.P.curve_index = Some "i_a"
       && (let (_, seq0, idx0) = List.hd c in idx0 < seq0)
       && (let (_, seqN, idxN) = List.nth c 40 in idxN > seqN)
       && (let rec mono = function (_, _, a) :: ((_, _, b) :: _ as r) -> a <= b +. 1e-9 && mono r | _ -> true in mono c)
     | None -> false);
  check "échelle : le chemin forcé prime sur l'échelle" (match Engine.run_plan { P.default_options with P.scale = Some 100000; P.force = Some "i_a" } "SELECT * FROM t WHERE a = 3" pdb with Ok (_, P.Plan p) -> p.P.forced && kind_of p = "idx:i_a" | _ -> false)

(* ---------------- Tests DDL ---------------- *)
let golden_ddl () =
  Printf.printf "\n== Golden DDL (CREATE / DROP INDEX) ==\n";
  let u = tbl_i "u" [ col "id" "INTEGER"; col "v" "VARCHAR" ]
            [ [ ("id", VInt 1); ("v", VStr "a") ]; [ ("id", VInt 2); ("v", VStr "a") ] ]
            [ idx ~unique:true ~implicit:true "u_pkey" [ "id" ]; idx "u_v" [ "v" ] ] in
  let w = tbl_i "w" [ col "index" "INTEGER" ] [ [ ("index", VInt 7) ] ] [] in
  let ddb = mkdb [ u; w ] in
  let stmt sql = Engine.run_statement P.default_options sql ddb in
  let is_create sql pred = match stmt sql with Ok (Engine.Ddl_ok (Typecheck.DdlCreate (t, i), _)) -> pred t i | _ -> false in
  let is_err sql frag = match stmt sql with Error (m, _) -> contains m frag | _ -> false in
  check "ddl : CREATE INDEX i ON u (id, v)"
    (is_create "CREATE INDEX i ON u (id, v)" (fun t i -> t.Db.tname = "u" && i.Db.iname = "i" && i.Db.icols = [ "id"; "v" ] && (not i.Db.iunique) && i.Db.ienabled && not i.Db.iimplicit));
  check "ddl : casse libre + UNIQUE + noms canoniques" (is_create "create unique index I2 on U(ID, V)" (fun _ i -> i.Db.iunique && i.Db.icols = [ "id"; "v" ] && i.Db.iname = "I2"));
  check "ddl : DROP INDEX ok" (match stmt "DROP INDEX u_v" with Ok (Engine.Ddl_ok (Typecheck.DdlDrop (_, i), _)) -> i.Db.iname = "u_v" | _ -> false);
  check "ddl : DROP INDEX inconnu → erreur" (is_err "DROP INDEX nope" "index inconnu");
  check "ddl : DROP de l'index implicite → refus" (is_err "DROP INDEX u_pkey" "implicite");
  check "ddl : table inconnue" (is_err "CREATE INDEX i ON zz (id)" "table inconnue");
  check "ddl : colonne inconnue" (is_err "CREATE INDEX i ON u (zz)" "colonne inconnue");
  check "ddl : colonne répétée" (is_err "CREATE INDEX i ON u (id, id)" "répétée");
  check "ddl : nom déjà pris par un index" (is_err "CREATE INDEX u_pkey ON u (v)" "existe déjà");
  check "ddl : nom déjà pris par une table" (is_err "CREATE INDEX w ON u (v)" "nom d'une table");
  check "ddl : UNIQUE violé → refus avec la clé en double" (is_err "CREATE UNIQUE INDEX uv ON u (v)" "en double");
  check "ddl : syntaxe incomplète" (is_err "CREATE INDEX ON u (id)" "attendu");
  check "ddl : colonne nommée « index » reste utilisable en SELECT"
    (match Engine.run "SELECT index FROM w WHERE index = 7" ddb with Ok r -> List.length r.Semantics.out_rows = 1 | Error _ -> false);
  check "ddl : Engine.run (SELECT seul) refuse un DDL" (match Engine.run "CREATE INDEX i ON u (id)" ddb with Error _ -> true | Ok _ -> false);
  (* frontière JSON *)
  let j = Engine.run_json "SELECT * FROM t WHERE a = 3" {|{"tables":[{"name":"t","columns":[{"name":"a","type":"INTEGER"}],"rows":[{"a":3},{"a":4}]}]}|} in
  check "json : sans indexes → plan disponible, Seq Scan" (contains j "\"available\":true" && contains j "\"kind\":\"seq_scan\"" && contains j "\"kind\":\"select\"");
  let j2 = Engine.run_json "SELECT * FROM t WHERE a = 3"
      {|{"tables":[{"name":"t","columns":[{"name":"a","type":"INTEGER"}],"rows":[{"a":3},{"a":4}],"indexes":[{"name":"i","columns":["a"],"enabled":false}]}]}|} in
  check "json : index désactivé → un seul chemin, index listé" (contains j2 "\"paths\":[{\"kind\":\"seq_scan\"" && (not (contains j2 "index_scan")) && contains j2 "\"name\":\"i\"");
  let j3 = Engine.run_json "CREATE INDEX i ON t (a)" {|{"tables":[{"name":"t","columns":[{"name":"a","type":"INTEGER"}],"rows":[]}]}|} in
  check "json : DDL → kind ddl + summary" (contains j3 "\"kind\":\"ddl\"" && contains j3 "CREATE INDEX i ON t (a)");
  let j4 = Engine.run_json "SELECT * FROM t" {|{"tables":[{"name":"t","columns":[{"name":"a","type":"INTEGER"}],"rows":[]}],"options":{"plan":false}}|} in
  check "json : options.plan = false → plan indisponible" (contains j4 "\"available\":false");
  let j5 = Engine.run_json "SELECT * FROM t WHERE a = 3"
      {|{"tables":[{"name":"t","columns":[{"name":"a","type":"INTEGER"}],"rows":[{"a":3},{"a":4}],"indexes":[{"name":"i","columns":["a"]}]}],"options":{"force":"i"}}|} in
  check "json : options.force → chemin forcé (couvrant : la table n'a qu'une colonne)" (contains j5 "\"forced\":true" && contains j5 "\"kind\":\"index_only_scan\"")

(* ---------------- Propriétés QCheck de la couche physique ---------------- *)
let gen_phys =
  let open QCheck.Gen in
  let idx_choices = [ [ "a" ]; [ "b" ]; [ "s" ]; [ "a"; "b" ]; [ "b"; "a" ]; [ "a"; "s" ]; [ "s"; "a"; "b" ] ] in
  let gen_indexes = list_size (int_range 0 3) (pair (oneof_list idx_choices) bool) in
  let wheres k =
    [ ""; Printf.sprintf " WHERE a = %d" k; Printf.sprintf " WHERE b > %d" k; Printf.sprintf " WHERE a = %d AND b < %d" k (k + 2);
      " WHERE s = 'x'"; Printf.sprintf " WHERE a IN (%d, %d)" k (k + 1); " WHERE a IS NULL"; Printf.sprintf " WHERE b BETWEEN %d AND %d" k (k + 2);
      Printf.sprintf " WHERE NOT a = %d" k; Printf.sprintf " WHERE a = %d OR b = %d" k k; " WHERE upper(s) = 'X'"; Printf.sprintf " WHERE b = %d AND a = %d" k k;
      Printf.sprintf " WHERE a = %d OR a = %d" k (k + 2); " WHERE a IS NOT NULL"; " WHERE s LIKE 'x%'"; Printf.sprintf " WHERE a = %d AND s = 'y'" k;
      Printf.sprintf " WHERE a > %d AND a < %d" (k - 1) (k + 2); Printf.sprintf " WHERE s = 'x' AND a = %d AND b = %d" k k; " WHERE a = NULL";
      Printf.sprintf " WHERE a <> %d" k; Printf.sprintf " WHERE a >= %d AND b IS NULL" k ] in
  let orders = [ ""; " ORDER BY a"; " ORDER BY b"; " ORDER BY a DESC"; " ORDER BY a, b"; " ORDER BY s"; " ORDER BY b DESC, a DESC"; " ORDER BY s, a, b"; " ORDER BY a, b DESC" ] in
  let limits = [ ""; " LIMIT 1"; " LIMIT 3"; " LIMIT 2 OFFSET 1"; " OFFSET 2" ] in
  let selects = [ "*"; "a"; "a, b"; "s"; "b, a"; "DISTINCT a"; "DISTINCT s, a"; "a AS x, b" ] in
  let gen_sql =
    int_range 0 5 >>= fun k ->
    map (fun (sel, w, o, l) -> "SELECT " ^ sel ^ " FROM t" ^ w ^ o ^ l)
      (quad (oneof_list selects) (oneof_list (wheres k)) (oneof_list orders) (oneof_list limits)) in
  triple (list_size (int_range 0 30) gen_row) gen_indexes gen_sql

let db_phys ?(enabled_override : bool option) rows idxs =
  let seen = Hashtbl.create 4 in
  let indexes = List.filter_map (fun (cols, en) ->
    let name = "i_" ^ String.concat "" cols in
    if Hashtbl.mem seen name then None
    else (Hashtbl.add seen name (); Some (idx ~enabled:(match enabled_override with Some e -> e | None -> en) name cols))) idxs in
  mkdb [ tbl_i "t" cols3 rows indexes ]

let props_physical () =
  Printf.printf "\n== Propriétés INDEX (QCheck) ==\n";
  let arb = QCheck.make ~print:(fun (rows, idxs, sql) ->
      Printf.sprintf "%d lignes, index %s, %s" (List.length rows)
        (String.concat "/" (List.map (fun (c, e) -> String.concat "" c ^ (if e then "" else "(off)")) idxs)) sql) gen_phys in
  (* P6 : TOUT chemin physique (forcé un par un) produit un résultat SQL valide identique au sémantique.
     L'échelle simulée (une fois sur deux) ne doit rien changer à la correction : elle n'influe que sur le CHOIX. *)
  run_prop "P6 tout chemin physique ≡ sémantique (sound, sacs / ordre / ex æquo), avec ou sans échelle simulée"
    (QCheck.Test.make ~count:600 ~name:"sound" arb (fun (rows, idxs, sql) ->
       let db = db_phys rows idxs in
       let scale = if List.length rows mod 2 = 0 then Some 100000 else None in
       match Engine.run_plan { P.default_options with P.scale = scale } sql db with
       | Error (m, _) -> QCheck.Test.fail_reportf "erreur : %s" m
       | Ok (_, P.Unavailable m) -> QCheck.Test.fail_reportf "plan indisponible : %s" m
       | Ok (_, P.Plan p) ->
         List.for_all (fun path ->
           let name = P.index_name path.P.access in
           match Engine.run_plan { P.default_options with P.force = Some name; P.scale = scale } sql db with
           | Ok (_, P.Plan q) ->
             (q.P.forced && q.P.sound.P.sound)
             || QCheck.Test.fail_reportf "non sound via %s : bag=%b keys=%b sorted=%b tie=%b" name
                  q.P.sound.P.bag_equal q.P.sound.P.order_keys_equal q.P.sound.P.sorted_ok q.P.sound.P.tie_ambiguity
           | _ -> QCheck.Test.fail_reportf "chemin forcé %s introuvable" name) p.P.paths));
  (* P8 : compteurs cohérents *)
  run_prop "P8 compteurs exacts cohérents (touched ⊆ lignes, passed ≤ candidats, returned = |résultat|)"
    (QCheck.Test.make ~count:300 ~name:"counters" arb (fun (rows, idxs, sql) ->
       let db = db_phys rows idxs in
       match Engine.run_plan P.default_options sql db with
       | Ok (r, P.Plan p) ->
         let e = p.P.exec and n = List.length rows in
         List.for_all (fun rid -> rid >= 0 && rid < n) e.P.touched && List.for_all (fun rid -> rid >= 0 && rid < n) e.P.stream
         && e.P.passed <= e.P.candidates && e.P.candidates = List.length e.P.stream
         && e.P.returned = List.length p.P.phys.Semantics.out_rows && e.P.returned = List.length r.Semantics.out_rows
         && (not e.P.early_stop || contains sql "LIMIT")
         && (match (chosen p).P.access with P.SeqScan -> e.P.touched = e.P.stream | P.IndexOnlyScan _ -> e.P.touched = [] | P.IndexScan _ -> e.P.touched = e.P.stream)
       | _ -> false));
  (* P9 : tous les index désactivés ⇒ Seq Scan, et sans LIMIT/OFFSET les sacs sont égaux *)
  run_prop "P9 index désactivés ⇒ Seq Scan ; sans LIMIT sacs égaux"
    (QCheck.Test.make ~count:200 ~name:"disabled" arb (fun (rows, idxs, sql) ->
       let db = db_phys ~enabled_override:false rows idxs in
       match Engine.run_plan P.default_options sql db with
       | Ok (_, P.Plan p) -> kind_of p = "seq" && List.length p.P.paths = 1 && (contains sql "LIMIT" || contains sql "OFFSET" || p.P.sound.P.bag_equal)
       | _ -> false))

let () =
  golden ();
  golden_joins ();
  props ();
  golden_physical ();
  golden_ddl ();
  props_physical ();
  Printf.printf "\n%s : %d échec(s)\n" (if !failures = 0 then "SUCCÈS" else "ÉCHEC") !failures;
  if !failures > 0 then exit 1
