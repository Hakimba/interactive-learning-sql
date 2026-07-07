(* main.ml — Point d'entrée js_of_ocaml.
   Expose globalThis.SqlEngine.run(sqlText, dbJson) -> resultJson.
   C'est LA frontière entre le moteur OCaml (sûr) et l'application réactive. *)

open Js_of_ocaml

let () =
  Js.export "SqlEngine"
    (object%js
       method run (sql : Js.js_string Js.t) (db : Js.js_string Js.t) : Js.js_string Js.t =
         Js.string (Sqlengine.Engine.run_json (Js.to_string sql) (Js.to_string db))
     end)
