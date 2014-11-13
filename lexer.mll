{
  open Grammar
}

rule token = parse
  | ['\t''\n'' ''\r']+ { token lexbuf }
  | "fun" { FUN }
  | "proc" { PROC }
  | "while" { WHILE }
  | "if" { IF }
  | "else" { ELSE}
  | "print" { PRINT }
  | "var" { VAR }
  | "int" { INT }
  | "real" { REAL }
  | "bool" { BOOL }
  | "true"|"false" as v { BOOLLIT v }
  | ['0'-'9']+'.'['0'-'9']+ as v { REALLIT v }
  | ['0'-'9']+ as v { INTLIT v }
  | ['_''A'-'Z''a'-'z']['0'-'9''_''A'-'Z''a'-'z']* as v { ID v }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | '*' { MULT }
  | '/' { DIV }
  | '%' { MOD }
  | '+' { PLUS }
  | '-' { MINUS }
  | "&&" { AND }
  | "||" { OR }
  | "<=" { LTE }
  | '<' { LT }
  | ">=" { GTE }
  | '>' { GT }
  | "==" { EQ }
  | "!=" { NEQ }
  | '=' { ASSIGN }
  | '!' { NOT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '^' { RET }
  | eof { EOF }
