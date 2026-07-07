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

(* Base entrante :
   { "tables": [ { "name": "...", "columns": [ {"name","type"} ], "rows": [ {col: val} ] } ] } *)
let db_of_json (s : string) : (Db.db, string) result =
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
      { Db.tname = name; cols; rows }) in
    Ok { Db.tables }
  with
  | Json_error m -> Error ("JSON invalide : " ^ m)
  | Yojson.Json_error m -> Error ("JSON mal formé : " ^ m)
  | e -> Error ("erreur de lecture de la base : " ^ Printexc.to_string e)

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

let result_to_json (r : Semantics.result) : string =
  `Assoc [
    ("ok", `Bool r.ok);
    ("error", match r.error with None -> `Null | Some e -> `String e);
    ("columns", `List (List.map (fun c -> `String c) r.columns));
    ("rows", `List (List.map (fun row -> `List (List.map json_of_value row)) r.out_rows));
    ("pipeline", `List (List.map json_of_stage r.pipeline));
  ] |> J.to_string

let error_json ?(pos = -1) (msg : string) : string =
  `Assoc [
    ("ok", `Bool false);
    ("error", `String msg);
    ("errorPos", `Int pos);
    ("columns", `List []);
    ("rows", `List []);
    ("pipeline", `List []);
  ] |> J.to_string
