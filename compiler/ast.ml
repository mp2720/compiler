type loc = { lnum : int; cnum : int }

type loc2 = loc * loc
(** Start and end loc. *)

type stmt =
  | AutoDefs of loc2 * autodef list
  | Labeled of loc2 * string * stmt
  | RExpr of loc2 * rexpr
  | Block of loc2 * stmt list
  | Goto of loc2 * string
  | If of loc2 * rexpr * stmt * stmt option
  | While of loc2 * rexpr * stmt

and autodef = AutoDef of loc2 * string * rexpr option

and rexpr =
  | Literal of loc2 * int
  | SizeOf of loc2 * string
  | AddrOf of loc2 * lexpr
  | LExpr of loc2 * lexpr
  | BinOp of loc2 * rexpr * binop * rexpr
  | RelOp of loc2 * rexpr * relop * rexpr
  | UnOp of loc2 * unop * rexpr
  | Assign of loc2 * lexpr * rexpr
  | AssignOp of loc2 * lexpr * binop * rexpr
  | Cond of loc2 * rexpr * rexpr * rexpr
  | PreInc of loc2 * lexpr
  | PreDec of loc2 * lexpr
  | PostInc of loc2 * lexpr
  | PostDec of loc2 * lexpr

and binop = Add | Sub | BitOr | BitAnd

and relop =
  | Eq
  | NEq
  (* signed *)
  | LEq
  | Lt
  | GEq
  | Gt
  (* unsigned *)
  | ULEq
  | ULt
  | UGEq
  | UGt

and unop = BitNot | Neg

and lexpr =
  | Var of loc2 * string
  | Deref of loc2 * rexpr
  | Subscript of loc2 * rexpr * rexpr

(* TODO: perhaps use Format? *)

let rec dump_funcbody body = String.concat "\n" (List.map dump_stmt body)

and dump_stmt = function
  | AutoDefs (_, defs) ->
      "auto " ^ String.concat ", " (List.map dump_autodef defs) ^ ";\n"
  | Labeled (_, label, stmt) -> Printf.sprintf "%s:\n%s" label (dump_stmt stmt)
  | RExpr (_, e) -> dump_rexpr e ^ ";"
  | Block (_, stmts) ->
      "{ " ^ String.concat "\n" (List.map dump_stmt stmts) ^ " }"
  | Goto (_, label) -> "goto " ^ label ^ ";"
  | If (_, cond, then_, else_) ->
      let else_branch =
        match else_ with
        | Some stmt -> Printf.sprintf "else\n%s" (dump_stmt stmt)
        | None -> ""
      in
      Printf.sprintf "if (%s)\n%s\n%s" (dump_rexpr cond) (dump_stmt then_)
        else_branch
  | While (_, cond, body) ->
      Printf.sprintf "while (%s)\n%s" (dump_rexpr cond) (dump_stmt body)

and dump_autodef (AutoDef (_, ident, expr)) =
  match expr with None -> ident | Some expr -> ident ^ " = " ^ dump_rexpr expr

and dump_rexpr = function
  | Literal (_, lit) -> Printf.sprintf "%d" lit
  | LExpr (_, le) -> dump_lexpr le
  | SizeOf (_, ident) -> Printf.sprintf "sizeof %s" ident
  | AddrOf (_, l) -> "&" ^ dump_lexpr l
  | BinOp (_, r, op, l) ->
      Printf.sprintf "(%s %s %s)" (dump_rexpr r) (dump_binop op) (dump_rexpr l)
  | RelOp (_, r, op, l) ->
      Printf.sprintf "(%s %s %s)" (dump_rexpr r) (dump_relop op) (dump_rexpr l)
  | UnOp (_, op, opnd) -> dump_unop op ^ dump_rexpr opnd
  | Assign (_, l, r) -> Printf.sprintf "(%s = %s)" (dump_lexpr l) (dump_rexpr r)
  | AssignOp (_, l, op, r) ->
      Printf.sprintf "(%s %s= %s)" (dump_lexpr l) (dump_binop op) (dump_rexpr r)
  | Cond (_, cond, then_, else_) ->
      Printf.sprintf "%s ? %s : %s" (dump_rexpr cond) (dump_rexpr then_)
        (dump_rexpr else_)
  | PreInc (_, l) -> "++" ^ dump_lexpr l
  | PreDec (_, l) -> "--" ^ dump_lexpr l
  | PostInc (_, l) -> dump_lexpr l ^ "++"
  | PostDec (_, l) -> dump_lexpr l ^ "--"

and dump_binop = function
  | Add -> "+"
  | Sub -> "-"
  | BitOr -> "|"
  | BitAnd -> "&"

and dump_relop = function
  | Eq -> "=="
  | NEq -> "!="
  | LEq -> ">="
  | Lt -> "<"
  | GEq -> ">="
  | Gt -> ">"
  | ULEq -> "^<="
  | ULt -> "^<"
  | UGEq -> "^>="
  | UGt -> "^>"

and dump_unop = function BitNot -> "~" | Neg -> "-"

and dump_lexpr = function
  | Var (_, ident) -> ident
  | Deref (_, re) -> "*" ^ dump_rexpr re
  | Subscript (_, sub, idx) ->
      Printf.sprintf "%s[%s]" (dump_rexpr sub) (dump_rexpr idx)
