%{
  open Tacl
%}

%token <string> BOOLLIT INTLIT REALLIT ID
%token FUN PROC WHILE IF ELSE PRINT INT REAL BOOL COMMA SEMICOLON MULT DIV MOD
%token PLUS MINUS AND OR EQ NEQ GT GTE LT LTE NOT ASSIGN LPAREN RPAREN LBRACKET
%token RBRACKET RET VAR EOF

%nonassoc ELSE
%left OR
%left AND
%left EQ NEQ LT LTE GT GTE
%left PLUS MINUS
%left MULT DIV MOD
%nonassoc UMINUS UPLUS
%nonassoc NOT

%start program
%type <Tacl.decl list> program

%%

program: global_declarations EOF { $1 };

global_declarations:
  | global_declaration { [$1] }
  | global_declaration global_declarations { $1::$2 }
;

global_declaration:
  | global_var_declaration { $1 }
  | fun_def { $1 }
  | proc_def { $1 }
;

global_var_declaration:
  VAR vtype ID opt_init SEMICOLON { GlobalVarDecl($2, $3, $4) }
;

fun_def:
  | FUN vtype ID LPAREN formal_args RPAREN fun_body {
    let (ds, ss, r) = $7
    in FunDecl {
      fun_name = $3;
      formal_args = $5;
      rtype = $2;
      body = ss;
      ldecls = ds;
      ret_expr = r;
    }
  }
  | FUN vtype ID LPAREN formal_args RPAREN ASSIGN expr SEMICOLON {
    FunDecl {
      fun_name = $3;
      formal_args = $5;
      rtype = $2;
      body = [];
      ldecls = [];
      ret_expr = $8;
    }
  }
;

proc_def: PROC ID LPAREN formal_args RPAREN proc_body {
  let (ds, ss) = $6
  in ProcDecl {
    proc_name = $2;
    formal_args = $4;
    ldecls = ds;
    body = ss;
  }
}
;

opt_init:
  | ASSIGN expr { Some $2 }
  | { None }
;

formal_args:
  | farg more_fargs { $1::$2 }
  | { [] }
;

farg: vtype ID { ($1, $2) }
;

more_fargs:
  | COMMA farg more_fargs { $2::$3 }
  | { [] }
;

vtype:
  | INT { Int }
  | REAL { Real }
  | BOOL { Bool }
;

expr:
  | LPAREN expr RPAREN { $2 }
  | literal { $1 }
  | ID { var_expr $1 }
  | fun_call { $1 }
  | binary_expr { $1 }
  | MINUS expr %prec UMINUS { op_expr Neg [$2] }
  | PLUS expr %prec UPLUS { $2 }
  | NOT expr { op_expr Not [$2] }
  | if_expr { $1 }
;

binary_expr:
  | expr PLUS expr { op_expr Sum [$1; $3] }
  | expr MINUS expr { op_expr Sub [$1; $3] }
  | expr MULT expr { op_expr Mult [$1; $3] }
  | expr DIV expr { op_expr Div [$1; $3] }
  | expr MOD expr { op_expr Mod [$1; $3] }
  | expr AND expr { op_expr And [$1; $3] }
  | expr OR expr { op_expr Or [$1; $3] }
  | expr EQ expr { op_expr Eq [$1; $3] }
  | expr LT expr { op_expr Lt [$1; $3] }
  | expr LTE expr { op_expr Lte [$1; $3] }
  | expr GT expr { op_expr Gt [$1; $3] }
  | expr GTE expr { op_expr Gte [$1; $3] }
  | expr NEQ expr { op_expr Neq [$1; $3] }
;

if_expr: IF LPAREN expr RPAREN expr ELSE expr { op_expr IfExpr [$3; $5; $7] }
;

literal:
  | INTLIT { int_lit $1 }
  | REALLIT { real_lit $1 }
  | BOOLLIT { bool_lit $1 }
;

fun_call: ID LPAREN call_args RPAREN { fun_call $1 $3 }
;

call_args:
  | expr more_cargs { $1::$2 }
  | { [] }
;

more_cargs:
  | COMMA expr more_cargs { $2::$3 }
  | { [] }
;

fun_body: LBRACKET local_decls stmts pseudo_instruction RBRACKET {($2, $3, $4)}
;

proc_body: LBRACKET local_decls stmts RBRACKET { ($2, $3) }
;

local_decls:
  | local_decl local_decls { $1::$2 }
  | {[]}
;

local_decl: VAR vtype ID opt_init SEMICOLON { ($2, $3, $4) }
;

stmts:
  | stmt stmts { $1::$2 }
  | {[]}
;

stmt:
  | matched_stmt  { $1 }
  | unmatched_stmt { $1 }
;

matched_stmt:
  | ID ASSIGN expr SEMICOLON { assign_stmt $1 $3 }
  | IF LPAREN expr RPAREN matched_stmt ELSE matched_stmt {
    if_stmt $3 $5 (Some $7)
  }
  | WHILE LPAREN expr RPAREN  matched_stmt { while_stmt $3 $5 }
  | PRINT expr SEMICOLON { print_stmt $2 }
  | ID LPAREN call_args RPAREN SEMICOLON { proc_call $1 $3 }
  | LBRACKET stmts RBRACKET { block_stmt $2 }
;

unmatched_stmt:
  | IF LPAREN expr RPAREN stmt { if_stmt $3 $5 None }
  | IF LPAREN expr RPAREN matched_stmt ELSE unmatched_stmt {
    if_stmt $3 $5 (Some $7)
  }
  | WHILE LPAREN expr RPAREN unmatched_stmt { while_stmt $3 $5 }
;

pseudo_instruction
  : RET expr { $2 }
;
