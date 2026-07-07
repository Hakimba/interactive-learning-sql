(* engine.ml — API stable : parse -> typecheck -> run. C'est ce que l'app interroge. *)

(* Version native (tests) : renvoie le résultat ou une erreur (message, position). *)
let run (sql : string) (db : Db.db) : (Semantics.result, string * int) result =
  match Parser.parse sql with
  | Error (msg, pos) -> Error (msg, pos)
  | Ok q ->
    (try Ok (Semantics.run (Typecheck.check q db))
     with Typecheck.Type_error m -> Error (m, -1))

(* API JSON (frontière avec JS) : SQL + base JSON -> résultat JSON. *)
let run_json (sql : string) (db_json : string) : string =
  match Json_io.db_of_json db_json with
  | Error msg -> Json_io.error_json msg
  | Ok db ->
    (match Parser.parse sql with
     | Error (msg, pos) -> Json_io.error_json ~pos msg
     | Ok q ->
       (try Json_io.result_to_json (Semantics.run (Typecheck.check q db))
        with
        | Typecheck.Type_error m -> Json_io.error_json m
        | e -> Json_io.error_json ("erreur interne : " ^ Printexc.to_string e)))
