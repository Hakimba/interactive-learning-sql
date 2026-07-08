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
let tbl name cols rows = { Db.tname = name; cols; rows }

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

let props () =
  Printf.printf "\n== Propriétés (QCheck) ==\n";
  let run_prop name t =
    match QCheck.Test.check_exn t with
    | () -> Printf.printf "  ok   %s\n" name
    | exception e -> incr failures; Printf.printf "FAIL   %s : %s\n" name (Printexc.to_string e)
  in
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

let () =
  golden ();
  golden_joins ();
  props ();
  Printf.printf "\n%s : %d échec(s)\n" (if !failures = 0 then "SUCCÈS" else "ÉCHEC") !failures;
  if !failures > 0 then exit 1
