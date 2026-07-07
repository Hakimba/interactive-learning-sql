(* db.ml — Modèle de données (schéma + n-uplets).
   Séparé pour éviter tout cycle entre typecheck (a besoin du schéma) et le runner. *)

open Value

type row = (string * value) list          (* n-uplet nommé *)
type column = { cname : string; cty : string }
type table = { tname : string; cols : column list; rows : row list }
type db = { tables : table list }

let find_table (db : db) name =
  let low = String.lowercase_ascii name in
  List.find_opt (fun t -> String.lowercase_ascii t.tname = low) db.tables

(* Cherche une colonne par nom (exact puis insensible à la casse). *)
let find_col (cols : column list) name =
  match List.find_opt (fun c -> c.cname = name) cols with
  | Some c -> Some c
  | None ->
    let low = String.lowercase_ascii name in
    List.find_opt (fun c -> String.lowercase_ascii c.cname = low) cols

let to_positional cols (r : row) =
  List.map (fun c -> match List.assoc_opt c r with Some v -> v | None -> VNull) cols
