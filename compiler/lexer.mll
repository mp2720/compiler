{
open Parser

let parse_int base =
  let parse_dig c = match c with
	| '0'..'9' -> Char.code c - Char.code '0'
	| d        -> 10 + Char.code (Char.lowercase_ascii d) - Char.code 'a'
  in
  String.fold_left (fun acc c ->
    let d = parse_dig c in
    acc * base + d
  ) 0

}

let ident_start_char = ['a'-'z' 'A'-'Z' '_']
let ident_char = ident_start_char | ['0'-'9']

let bin_dig = ['0' '1']
let oct_dig = ['0'-'7']
let dec_dig = ['0'-'9']
let hex_dig = ['0'-'9' 'a'-'f' 'A'-'F']

rule token = parse
  | [' ' '\t' '\n'] | "/*" _+ "*/" | "//" [^'\n']+ { token lexbuf }
  | ident_start_char ident_char*
    { match Lexing.lexeme lexbuf with
      | "if"     -> IF
	  | "else"   -> ELSE
      | "while"  -> WHILE
	  | "auto"   -> AUTO
	  | "goto"   -> GOTO
	  | "sizeof" -> SIZEOF
      | ident    -> IDENT ident }
  | "0b" (bin_dig+ as num) { LITERAL (parse_int 2 num) }
  | "0o" (oct_dig+ as num) { LITERAL (parse_int 8 num) }
  | "0x" (hex_dig+ as num) { LITERAL (parse_int 16 num) }
  | (dec_dig+ as num)      { LITERAL (parse_int 10 num) }

  (* Punctuation *)
  | '{'   { LBRACE }
  | '}'   { RBRACE }
  | '['   { LBRACKET }
  | ']'   { RBRACKET }
  | '('   { LPAREN }
  | ')'   { RPAREN }
  | ','   { COMMA }
  | ';'   { SEMICOLON }
  | ':'   { COLON }
  | '-'   { MINUS }
  | '+'   { PLUS }
  | '*'   { STAR }
  | '~'   { BITNOT }
  | '|'   { BITOR }
  | '&'   { AMPERSAND }
  | "=="  { EQ }
  | "!="  { NEQ }
  | "<="  { LE }
  | "<"   { LT }
  | ">="  { GE }
  | ">"   { GT }
  | "^>=" { UGE }
  | "^>"  { UGT }
  | "^<=" { ULE }
  | "^<"  { ULT }
  | "="   { ASSIGN }
  | "+="  { ASSIGN_PLUS }
  | "-="  { ASSIGN_MINUS }
  | "|="  { ASSIGN_BITOR }
  | "&="  { ASSIGN_BITAND }
  | "?"   { QUEST }
  | "++"  { INC }
  | "--"  { DEC }
  | eof   { EOF }
