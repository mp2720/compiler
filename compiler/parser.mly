%{

open Ast
open Lexing

let mk_loc (s, e) = (
  { lnum = s.pos_lnum; cnum = s.pos_cnum },
  { lnum = e.pos_lnum; cnum = e.pos_cnum }
)

%}

%token <string> IDENT
%token <int>    LITERAL

%token IF     (* if *)
%token ELSE   (* else *)
%token WHILE  (* while *)
%token AUTO   (* auto *)
%token GOTO   (* goto *)
%token SIZEOF (* sizeof *)

%token LBRACE        (* {   *)
%token RBRACE        (* }   *)
%token LBRACKET      (* [   *)
%token RBRACKET      (* ]   *)
%token LPAREN        (* (   *)
%token RPAREN        (* )   *)
%token COMMA         (* ,   *)
%token SEMICOLON     (* ;   *)
%token COLON         (* :   *)
%token MINUS         (* -   *)
%token PLUS          (* +   *)
%token STAR          (* *   *)
%token BITNOT        (* ~   *)
%token BITOR         (* |   *)
%token AMPERSAND     (* &   *)
%token EQ            (* ==  *)
%token NEQ           (* !=  *)
%token LE            (* <=  *)
%token LT            (* <   *)
%token GE            (* >=  *)
%token GT            (* >   *)
%token UGE           (* ^>= *)
%token UGT           (* ^>  *)
%token ULE           (* ^<= *)
%token ULT           (* ^<  *)
%token ASSIGN        (* =   *)
%token ASSIGN_PLUS   (* +=  *)
%token ASSIGN_MINUS  (* -=  *)
%token ASSIGN_BITOR  (* |=  *)
%token ASSIGN_BITAND (* &=  *)
%token QUEST         (* ?   *)
%token INC           (* ++  *)
%token DEC           (* --  *)
%token EOF

%start funcbody

%right ASSIGN ASSIGN_PLUS ASSIGN_MINUS ASSIGN_BITOR ASSIGN_BITAND
%right QUEST COLON
%left EQ NEQ LE LT GE GT UGE UGT ULE ULT
%left PLUS MINUS BITOR AMPERSAND

%type <Ast.stmt list>    funcbody
%type <Ast.stmt>         stmt
%type <Ast.autodef list> separated_nonempty_list(COMMA,autodef)
%type <Ast.autodef>      autodef
%type <Ast.stmt>         open_stmt
%type <Ast.stmt>         closed_stmt
%type <Ast.stmt>         simple_stmt
%type <Ast.stmt list>    list(stmt)
%type <Ast.rexpr>        rexpr
%type <Ast.rexpr>        atom
%type <Ast.lexpr>        lexpr
%type <Ast.rexpr>        subscript

%%

funcbody:
  | list(stmt) EOF
    { $1 }

stmt:
  | AUTO separated_nonempty_list(COMMA, autodef) SEMICOLON;
    { AutoDefs (mk_loc $sloc, $2) }
  | IDENT COLON stmt
    { Labeled (mk_loc $sloc, $1, $3) }
  | open_stmt
    { $1 }
  | closed_stmt
    { $1 }
  | WHILE LPAREN rexpr RPAREN stmt
    { While (mk_loc $sloc, $3, $5) }

autodef:
  | IDENT
    { AutoDef (mk_loc $sloc, $1, None) }
  | IDENT ASSIGN rexpr
    { AutoDef (mk_loc $sloc, $1, Some ($3)) }

open_stmt:
  | IF LPAREN rexpr RPAREN stmt
    { If (mk_loc $sloc, $3, $5, None) }
  | IF LPAREN rexpr RPAREN closed_stmt ELSE open_stmt
    { If (mk_loc $sloc, $3, $5, Some ($7)) }

closed_stmt:
  | simple_stmt
    { $1 }
  | IF LPAREN rexpr RPAREN closed_stmt ELSE closed_stmt
    { If (mk_loc $sloc, $3, $5, Some ($7)) }

simple_stmt:
  | rexpr SEMICOLON
    { RExpr (mk_loc $sloc, $1) }
  | LBRACE stmt* RBRACE
    { Block (mk_loc $sloc, $2) }
  | GOTO IDENT SEMICOLON
    { Goto (mk_loc $sloc, $2) }

rexpr:
  | atom
    { $1 }
  | rexpr binop rexpr
    { BinOp (mk_loc $sloc, $1, $2, $3) }
  | rexpr relop rexpr
    { RelOp (mk_loc $sloc, $1, $2, $3) }
  | lexpr ASSIGN rexpr
    { Assign (mk_loc $sloc, $1, $3) }
  | lexpr assign_op rexpr
    { AssignOp (mk_loc $sloc, $1, $2, $3) }

  (* TODO: AFAIK C standard treats ternary if specially. Investigate if my approach is correct. *)
  | rexpr QUEST rexpr COLON rexpr
    { Cond (mk_loc $sloc, $1, $3, $5) }

  (*
    NOTE: Unlike in C, atom or subscript could not be an inc/dec (without use of parenthesis).
    So, expressions like `*++p` are not valid.
  *)
  | inc_dec lexpr
    {
      if $1
      then PreInc (mk_loc $sloc, $2)
      else PreDec (mk_loc $sloc, $2)
    }
  | lexpr inc_dec
    {
      if $2
      then PostInc (mk_loc $sloc, $1)
      else PostDec (mk_loc $sloc, $1)
    }

(* %inline preserves the precedence *)

%inline binop:
  | PLUS      { Add }
  | MINUS     { Sub }
  | BITOR     { BitOr }
  | AMPERSAND { BitAnd }

%inline relop:
  | EQ  { Eq }
  | NEQ { NEq }
  | LE  { LEq }
  | LT  { Lt }
  | GE  { GEq }
  | GT  { Gt }
  | UGE { UGEq }
  | UGT { UGt }
  | ULE { ULEq }
  | ULT { ULt }

%inline assign_op:
  | ASSIGN_PLUS   { Add }
  | ASSIGN_MINUS  { Sub }
  | ASSIGN_BITOR  { BitOr }
  | ASSIGN_BITAND { BitAnd }

%inline inc_dec:
  | INC { true  }
  | DEC { false }

atom:
  | LPAREN rexpr RPAREN
    { $2 }
  | LITERAL
    { Literal (mk_loc $sloc, $1) }
  | SIZEOF IDENT
    { SizeOf (mk_loc $sloc, $2)}
  | AMPERSAND lexpr
    { AddrOf (mk_loc $sloc, $2) }
  | lexpr
    { LExpr (mk_loc $sloc, $1) }
  | MINUS atom
    { UnOp (mk_loc $sloc, Neg, $2) }
  | BITNOT atom
    { UnOp (mk_loc $sloc, BitNot, $2) }

lexpr:
  | IDENT
    { Var (mk_loc $sloc, $1) }
  | STAR atom
    { Deref (mk_loc $sloc, $2) }
  | subscript LBRACKET rexpr RBRACKET
    { Subscript (mk_loc $sloc, $1, $3) }

subscript:
  | IDENT
    { LExpr (mk_loc $sloc, Var (mk_loc $sloc, $1)) }
  | LPAREN rexpr RPAREN
    { $2 }
  | LITERAL
    { Literal (mk_loc $sloc, $1) }
