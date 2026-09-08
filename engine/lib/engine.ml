(* engine.ml — API stable : parse -> typecheck -> run (+ couche physique, + DDL). C'est ce que l'app interroge. *)

(* Version native (tests) : requête SELECT -> résultat sémantique, ou erreur (message, position). *)
let run (sql : string) (db : Db.db) : (Semantics.result, string * int) result =
  match Parser.parse sql with
  | Error (msg, pos) -> Error (msg, pos)
  | Ok q ->
    (try Ok (Semantics.run (Typecheck.check q db))
     with Typecheck.Type_error m -> Error (m, -1))

(* Une instruction exécutée : requête (résultat + plan physique) ou DDL validé (+ avertissements). *)
type outcome =
  | Query of Semantics.result * Physical.plan
  | Ddl_ok of Typecheck.ddl_result * string list

let run_statement (o : Physical.options) (sql : string) (db : Db.db) : (outcome, string * int) result =
  match Parser.parse_statement sql with
  | Error e -> Error e
  | Ok (Ast.Ddl d) ->
    (match Typecheck.check_ddl d db with
     | Error m -> Error (m, -1)
     | Ok (Typecheck.DdlCreate (t, i) as r) ->
       (* un index UNIQUE ne peut pas être créé si des clés sont en double (message Postgres-like) *)
       (match Physical.build_index t i with
        | Ok b when i.Db.iunique && b.Physical.unique_violations <> [] ->
          let k = List.hd b.Physical.unique_violations in
          Error (Printf.sprintf "impossible de créer l'index unique « %s » : la clé (%s)=(%s) est en double"
                   i.Db.iname (String.concat ", " i.Db.icols) (String.concat ", " (List.map Value.to_display k)), -1)
        | Ok _ -> Ok (Ddl_ok (r, []))
        | Error m -> Error (m, -1))
     | Ok r -> Ok (Ddl_ok (r, [])))
  | Ok (Ast.Select q) ->
    (try
       let tq = Typecheck.check q db in
       let records, res = Semantics.run_records tq (Semantics.scan_all tq) in
       let plan =
         if not o.Physical.with_plan then Physical.Unavailable "plan désactivé"
         else (try Physical.plan o tq ~sem:records
               with e -> Physical.Unavailable ("erreur interne (plan) : " ^ Printexc.to_string e)) in
       Ok (Query (res, plan))
     with Typecheck.Type_error m -> Error (m, -1))

(* Version native (tests de la couche physique). *)
let run_plan (o : Physical.options) (sql : string) (db : Db.db) : (Semantics.result * Physical.plan, string * int) result =
  match run_statement o sql db with
  | Ok (Query (r, p)) -> Ok (r, p)
  | Ok (Ddl_ok _) -> Error ("instruction DDL : pas une requête SELECT", 0)
  | Error e -> Error e

(* API JSON (frontière avec JS) : SQL + base JSON (+ options) -> résultat JSON. *)
let run_json (sql : string) (db_json : string) : string =
  match Json_io.input_of_json db_json with
  | Error msg -> Json_io.error_json msg
  | Ok (db, opts) ->
    (try
       (match run_statement opts sql db with
        | Error (msg, pos) -> Json_io.error_json ~pos msg
        | Ok (Query (r, plan)) -> Json_io.result_to_json ~plan r
        | Ok (Ddl_ok (d, w)) -> Json_io.ddl_to_json d w)
     with e -> Json_io.error_json ("erreur interne : " ^ Printexc.to_string e))
