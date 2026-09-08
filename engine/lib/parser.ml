(* parser.ml — Lexer + parseur par descente récursive (écrit à la main).
   Produit un Ast.query. Erreurs en français avec position. *)

exception Parse_error of string * int

(* ------------------------------------------------------------------ *)
(* Lexer                                                               *)
(* ------------------------------------------------------------------ *)
type tok =
  | TNum of float
  | TStr of string
  | TIdent of string
  | TKw of string
  | TOp of string
  | TLParen
  | TRParen
  | TComma
  | TDot
  | TEof

let keywords =
  [ "select"; "distinct"; "from"; "where"; "as"; "and"; "or"; "not";
    "in"; "like"; "is"; "null"; "between"; "order"; "by"; "asc"; "desc";
    "limit"; "offset"; "true"; "false";
    "join"; "inner"; "left"; "right"; "full"; "outer"; "cross"; "on"; "using" ]

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_digit c = c >= '0' && c <= '9'
let is_alnum c = is_alpha c || is_digit c

let tokenize (input : string) : (tok * int) list =
  let n = String.length input in
  let toks = ref [] in
  let i = ref 0 in
  let emit t p = toks := (t, p) :: !toks in
  while !i < n do
    let c = input.[!i] in
    if c = ' ' || c = '\t' || c = '\n' || c = '\r' then incr i
    else if c = '-' && !i + 1 < n && input.[!i + 1] = '-' then
      (* commentaire jusqu'à la fin de ligne *)
      (while !i < n && input.[!i] <> '\n' do incr i done)
    else begin
      let start = !i in
      if is_digit c then begin
        let b = Buffer.create 8 in
        while !i < n && is_digit input.[!i] do Buffer.add_char b input.[!i]; incr i done;
        if !i < n && input.[!i] = '.' then begin
          Buffer.add_char b '.'; incr i;
          while !i < n && is_digit input.[!i] do Buffer.add_char b input.[!i]; incr i done
        end;
        emit (TNum (float_of_string (Buffer.contents b))) start
      end
      else if c = '\'' then begin
        incr i;
        let b = Buffer.create 8 in
        let closed = ref false in
        while !i < n && not !closed do
          if input.[!i] = '\'' then begin
            if !i + 1 < n && input.[!i + 1] = '\'' then (Buffer.add_char b '\''; i := !i + 2)
            else (incr i; closed := true)
          end else (Buffer.add_char b input.[!i]; incr i)
        done;
        if not !closed then raise (Parse_error ("chaîne non terminée (guillemet simple manquant)", start));
        emit (TStr (Buffer.contents b)) start
      end
      else if c = '"' then begin
        incr i;
        let b = Buffer.create 8 in
        while !i < n && input.[!i] <> '"' do Buffer.add_char b input.[!i]; incr i done;
        if !i >= n then raise (Parse_error ("identifiant entre guillemets non terminé", start));
        incr i;
        emit (TIdent (Buffer.contents b)) start
      end
      else if is_alpha c then begin
        let b = Buffer.create 8 in
        while !i < n && is_alnum input.[!i] do Buffer.add_char b input.[!i]; incr i done;
        let w = Buffer.contents b in
        let lw = String.lowercase_ascii w in
        if List.mem lw keywords then emit (TKw lw) start else emit (TIdent w) start
      end
      else begin
        let two = if !i + 1 < n then String.sub input !i 2 else "" in
        if two = "<=" || two = ">=" || two = "<>" || two = "!=" then
          (emit (TOp (if two = "!=" then "<>" else two)) start; i := !i + 2)
        else begin
          (match c with
           | '=' | '<' | '>' | '+' | '-' | '*' | '/' | '%' -> emit (TOp (String.make 1 c)) start
           | '(' -> emit TLParen start
           | ')' -> emit TRParen start
           | ',' -> emit TComma start
           | '.' -> emit TDot start
           | _ -> raise (Parse_error (Printf.sprintf "caractère inattendu « %c »" c, start)));
          incr i
        end
      end
    end
  done;
  emit TEof n;
  List.rev !toks

(* ------------------------------------------------------------------ *)
(* Parseur                                                             *)
(* ------------------------------------------------------------------ *)
type state = { toks : (tok * int) array; mutable p : int }

let mk input = { toks = Array.of_list (tokenize input); p = 0 }
let peek st = fst st.toks.(st.p)
let pos st = snd st.toks.(st.p)
let advance st = let t = st.toks.(st.p) in st.p <- st.p + 1; fst t

let describe = function
  | TEof -> "la fin de la requête"
  | TNum f -> Printf.sprintf "« %g »" f
  | TStr s -> Printf.sprintf "« '%s' »" s
  | TIdent s | TKw s | TOp s -> Printf.sprintf "« %s »" s
  | TLParen -> "« ( »" | TRParen -> "« ) »" | TComma -> "« , »" | TDot -> "« . »"

let err st msg = raise (Parse_error (Printf.sprintf "%s (trouvé : %s)" msg (describe (peek st)), pos st))

let is_kw st k = match peek st with TKw x -> x = k | _ -> false
let is_op st o = match peek st with TOp x -> x = o | _ -> false
let eat_kw st k = if is_kw st k then ignore (advance st) else err st (Printf.sprintf "« %s » attendu" (String.uppercase_ascii k))
let eat_lp st = match peek st with TLParen -> ignore (advance st) | _ -> err st "« ( » attendu"
let eat_rp st = match peek st with TRParen -> ignore (advance st) | _ -> err st "« ) » attendu"

let ident_name st =
  match peek st with
  | TIdent s -> ignore (advance st); s
  | _ -> err st "identifiant attendu"

(* --- expressions --- *)
let rec parse_expr st = parse_add st

and parse_add st =
  let left = ref (parse_mul st) in
  let continue = ref true in
  while !continue do
    if is_op st "+" then (ignore (advance st); left := Ast.Arith (Ast.Add, !left, parse_mul st))
    else if is_op st "-" then (ignore (advance st); left := Ast.Arith (Ast.Sub, !left, parse_mul st))
    else continue := false
  done;
  !left

and parse_mul st =
  let left = ref (parse_unary st) in
  let continue = ref true in
  while !continue do
    if is_op st "*" then (ignore (advance st); left := Ast.Arith (Ast.Mul, !left, parse_unary st))
    else if is_op st "/" then (ignore (advance st); left := Ast.Arith (Ast.Div, !left, parse_unary st))
    else if is_op st "%" then (ignore (advance st); left := Ast.Arith (Ast.Mod, !left, parse_unary st))
    else continue := false
  done;
  !left

and parse_unary st =
  if is_op st "-" then (ignore (advance st); Ast.Neg (parse_unary st))
  else if is_op st "+" then (ignore (advance st); parse_unary st)
  else parse_primary st

and parse_primary st =
  match peek st with
  | TNum f ->
    ignore (advance st);
    (* entier si pas de partie fractionnaire *)
    if Float.is_integer f && Float.abs f < 1e15 then Ast.Lit (Value.VInt (int_of_float f))
    else Ast.Lit (Value.VFloat f)
  | TStr s -> ignore (advance st); Ast.Lit (Value.VStr s)
  | TKw "true" -> ignore (advance st); Ast.Lit (Value.VBool true)
  | TKw "false" -> ignore (advance st); Ast.Lit (Value.VBool false)
  | TKw "null" -> ignore (advance st); Ast.Lit Value.VNull
  | TLParen -> ignore (advance st); let e = parse_expr st in eat_rp st; e
  | TIdent name ->
    ignore (advance st);
    (match peek st with
     | TLParen ->
       ignore (advance st);
       let args = ref [] in
       (if peek st <> TRParen then begin
          args := [ parse_expr st ];
          while peek st = TComma do ignore (advance st); args := parse_expr st :: !args done
        end);
       eat_rp st;
       Ast.Func (String.lowercase_ascii name, List.rev !args)
     | TDot ->
       ignore (advance st);
       let col = ident_name st in
       Ast.Col (Some name, col)
     | _ -> Ast.Col (None, name))
  | _ -> err st "expression attendue"

(* --- conditions --- *)
and parse_cond st = parse_or st

and parse_or st =
  let left = ref (parse_and st) in
  while is_kw st "or" do ignore (advance st); left := Ast.Or (!left, parse_and st) done;
  !left

and parse_and st =
  let left = ref (parse_not st) in
  while is_kw st "and" do ignore (advance st); left := Ast.And (!left, parse_and st) done;
  !left

and parse_not st =
  if is_kw st "not" then (ignore (advance st); Ast.Not (parse_not st))
  else parse_cmp st

and parse_cmp st =
  let left = parse_expr st in
  (* NOT optionnel avant IN / LIKE / BETWEEN (ex : x NOT IN (...)) *)
  let neg =
    match peek st with
    | TKw "not" ->
      (match (if st.p + 1 < Array.length st.toks then fst st.toks.(st.p + 1) else TEof) with
       | TKw ("in" | "like" | "between") -> ignore (advance st); true
       | _ -> false)
    | _ -> false
  in
  match peek st with
  | TKw "in" ->
    ignore (advance st); eat_lp st;
    let items = ref [] in
    (if peek st <> TRParen then begin
       items := [ parse_expr st ];
       while peek st = TComma do ignore (advance st); items := parse_expr st :: !items done
     end);
    eat_rp st;
    Ast.In (left, List.rev !items, neg)
  | TKw "like" -> ignore (advance st); Ast.Like (left, parse_expr st, neg)
  | TKw "between" ->
    ignore (advance st);
    let lo = parse_add st in
    eat_kw st "and";
    let hi = parse_add st in
    Ast.Between (left, lo, hi, neg)
  | TKw "is" ->
    ignore (advance st);
    if is_kw st "not" then (ignore (advance st); eat_kw st "null"; Ast.IsNotNull left)
    else (eat_kw st "null"; Ast.IsNull left)
  | TOp ("=" | "<>" | "<" | "<=" | ">" | ">=" as o) ->
    ignore (advance st);
    let r = parse_expr st in
    let c = match o with
      | "=" -> Ast.Eq | "<>" -> Ast.Neq | "<" -> Ast.Lt
      | "<=" -> Ast.Le | ">" -> Ast.Gt | _ -> Ast.Ge in
    Ast.Cmp (c, left, r)
  | _ ->
    (* une expression seule utilisée comme condition (ex: colonne booléenne) : x  ~  x = true *)
    Ast.Cmp (Ast.Eq, left, Ast.Lit (Value.VBool true))

(* --- requête --- *)
let parse_select_list st =
  if is_op st "*" then (ignore (advance st); Ast.Star)
  else begin
    let items = ref [] in
    let one () =
      let e = parse_expr st in
      let alias =
        if is_kw st "as" then (ignore (advance st);
          match peek st with TIdent s -> ignore (advance st); Some s | _ -> err st "alias attendu")
        else match peek st with
          | TIdent s -> ignore (advance st); Some s   (* alias implicite *)
          | _ -> None
      in
      { Ast.e; Ast.alias }
    in
    items := [ one () ];
    while peek st = TComma do ignore (advance st); items := one () :: !items done;
    Ast.Items (List.rev !items)
  end

let parse_order_by st =
  let keys = ref [] in
  let one () =
    let e = parse_expr st in
    let d =
      if is_kw st "asc" then (ignore (advance st); Ast.Asc)
      else if is_kw st "desc" then (ignore (advance st); Ast.Desc)
      else Ast.Asc
    in (e, d)
  in
  keys := [ one () ];
  while peek st = TComma do ignore (advance st); keys := one () :: !keys done;
  List.rev !keys

let parse_int st what =
  match peek st with
  | TNum f -> ignore (advance st); int_of_float f
  | _ -> err st (Printf.sprintf "un nombre est attendu après %s" what)

(* Référence de table : nom + alias optionnel (AS a | a). *)
let parse_table_ref st =
  let name = ident_name st in
  let alias =
    if is_kw st "as" then (ignore (advance st); Some (ident_name st))
    else match peek st with TIdent a -> ignore (advance st); Some a | _ -> None
  in
  (name, alias)

(* Liste de jointures : [INNER|LEFT|RIGHT|FULL [OUTER]|CROSS] JOIN t [alias] [ON cond] ; « , » = CROSS. *)
let rec parse_joins st =
  let comma = (peek st = TComma) in
  let kind =
    if comma then (ignore (advance st); Some Ast.Cross)
    else if is_kw st "cross" then (ignore (advance st); Some Ast.Cross)
    else if is_kw st "inner" then (ignore (advance st); Some Ast.Inner)
    else if is_kw st "left" then (ignore (advance st); (if is_kw st "outer" then ignore (advance st)); Some Ast.Left)
    else if is_kw st "right" then (ignore (advance st); (if is_kw st "outer" then ignore (advance st)); Some Ast.Right)
    else if is_kw st "full" then (ignore (advance st); (if is_kw st "outer" then ignore (advance st)); Some Ast.Full)
    else if is_kw st "join" then Some Ast.Inner
    else None
  in
  match kind with
  | None -> []
  | Some k ->
    if not comma then eat_kw st "join";
    let (t, a) = parse_table_ref st in
    let on =
      if is_kw st "on" then (ignore (advance st); Some (parse_cond st))
      else if is_kw st "using" then err st "USING n'est pas encore supporté — écris ON a.x = b.y"
      else None
    in
    { Ast.jtable = t; jalias = a; jkind = k; jon = on } :: parse_joins st

let parse_query_st st =
  if not (is_kw st "select") then err st "une requête doit commencer par SELECT";
  eat_kw st "select";
  let distinct = if is_kw st "distinct" then (ignore (advance st); true) else false in
  let sel = parse_select_list st in
  eat_kw st "from";
  let (from, from_alias) = parse_table_ref st in
  let joins = parse_joins st in
  let where = if is_kw st "where" then (ignore (advance st); Some (parse_cond st)) else None in
  let order_by =
    if is_kw st "order" then (ignore (advance st); eat_kw st "by"; parse_order_by st) else []
  in
  let limit = if is_kw st "limit" then (ignore (advance st); Some (parse_int st "LIMIT")) else None in
  let offset = if is_kw st "offset" then (ignore (advance st); Some (parse_int st "OFFSET")) else None in
  (match peek st with TEof -> () | _ -> err st "fin de requête attendue");
  { Ast.distinct; sel; from; from_alias; joins; where; order_by; limit; offset }

(* --- DDL des index ---
   « create », « unique », « index », « drop » ne sont PAS des mots-clés du lexer : une colonne peut
   s'appeler « index ». On les reconnaît comme identifiants (insensibles à la casse) en tête d'instruction. *)
let is_word st w = match peek st with TIdent s -> String.lowercase_ascii s = w | _ -> false
let eat_word st w =
  if is_word st w then ignore (advance st) else err st (Printf.sprintf "« %s » attendu" (String.uppercase_ascii w))

let parse_create st =
  eat_word st "create";
  let unique = if is_word st "unique" then (ignore (advance st); true) else false in
  eat_word st "index";
  let name = ident_name st in
  eat_kw st "on";
  let table = ident_name st in
  eat_lp st;
  let cols = ref [ ident_name st ] in
  while peek st = TComma do ignore (advance st); cols := ident_name st :: !cols done;
  eat_rp st;
  (match peek st with TEof -> () | _ -> err st "fin d'instruction attendue");
  Ast.CreateIndex { iname = name; itable = table; icols = List.rev !cols; iunique = unique }

let parse_drop st =
  eat_word st "drop";
  eat_word st "index";
  let name = ident_name st in
  (match peek st with TEof -> () | _ -> err st "fin d'instruction attendue");
  Ast.DropIndex name

let parse_statement_st st =
  if is_word st "create" then Ast.Ddl (parse_create st)
  else if is_word st "drop" then Ast.Ddl (parse_drop st)
  else Ast.Select (parse_query_st st)

(* Point d'entrée général : SELECT ou DDL. *)
let parse_statement (input : string) : (Ast.statement, string * int) result =
  try Ok (parse_statement_st (mk input))
  with Parse_error (msg, p) -> Error (msg, p)

(* Point d'entrée « requête » : renvoie Ok query | Error (message, position). *)
let parse (input : string) : (Ast.query, string * int) result =
  match parse_statement input with
  | Ok (Ast.Select q) -> Ok q
  | Ok (Ast.Ddl _) -> Error ("instruction DDL : pas une requête SELECT", 0)
  | Error e -> Error e
