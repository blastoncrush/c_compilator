/* Analyseur syntaxique pour Arith */

%{
  open Ast
%}

%token <int> CST_INT
%token <string> CST_CHAR
%token <string> CST_STRING
%token <string> IDENT
%token EOF
%token LP RP LB RB LC RC
%token PLUS MINUS TIMES DIV MOD
%token AND OR NOT
%token EQ EQQ
%token SEMICOLON COMMA RETURN INT CHAR
%token LEQ GEQ LE GE NEQ
%token IF ELSE WHILE
%token BREAK CONTINUE 
%token ADDR MALLOC SIZEOF


/* D�finitions des priorit�s et associativit�s des tokens */
%left OR
%left AND
%nonassoc EQQ NEQ
%nonassoc LE LEQ GE GEQ
%left PLUS MINUS 
%left TIMES DIV MOD
%nonassoc NOT uminus 


/* Point d'entr�e de la grammaire */
%start prog

/* Type des valeurs retourn�es par l'analyseur syntaxique */
%type <Ast.program> prog

%%

prog:
| p = list(def) EOF                                                         { p }
;

def:
| t = c_type id = IDENT dims = array_dims SEMICOLON                         { Gvar(id, t, dims, $loc) }
| t = c_type id = IDENT dims = array_dims SEMICOLON                         { Gvar(id, t, dims, $loc) }
| t = c_type id = IDENT SEMICOLON                                           { Gvar(id, t, [], $loc) }
| t = c_type id = IDENT SEMICOLON                                           { Gvar(id, t, [], $loc) }
| t = c_type id = IDENT LP argu = def_arg RP LB body = list(stmt) RB        { Function(id,argu,body,$loc) }
;

c_type:
| INT                                                                       { Int(0, $loc) }
| CHAR                                                                      { Char(0, $loc) }
| INT pointer = pointer_type                                                { Int(pointer, $loc) }
| CHAR pointer = pointer_type                                               { Char(pointer, $loc) }
;

pointer_type:
|                                                                           { 0 }
| TIMES p = pointer_type                                                    { p + 1 }
;

array_dims:
| LC e = expr RC                                                            { [e] }
| LC e = expr RC dims = array_dims                                          { e :: dims }
;

def_arg:
|                                                                           { [] }
| t = c_type id = IDENT COMMA reste = def_arg                               { Lvar(id, t, $loc)::reste }
| t = c_type id = IDENT                                                     { [Lvar(id, t, $loc)] }
;

stmt:
| t = c_type id = IDENT SEMICOLON                                           { Lvar(id, t, $loc) }
| t = c_type id = IDENT dims = array_dims SEMICOLON                         { Tab_int(id, t, dims, $loc) }
| id = IDENT EQ e = expr SEMICOLON                                          { Set(id, 0, e, $loc) }
| pointer = pointer_type id = IDENT EQ e = expr SEMICOLON                   { Set(id, pointer, e, $loc) }
| id = IDENT dims = array_dims EQ e = expr SEMICOLON                        { Set_tab(id,0, dims,e,$loc) }
| pointer = pointer_type id = IDENT dims = array_dims EQ e = expr SEMICOLON { Set_tab(id,pointer, dims,e,$loc) }
| id = IDENT EQ MALLOC LP e = expr RP SEMICOLON                             { Malloc(id,e,$loc) }
| RETURN e = expr SEMICOLON                                                 { Return(e,$loc) }
| IF LP e = expr RP LB body = list(stmt) RB                                 { If(e, body, $loc) }
| IF LP e = expr RP body = stmt                                             { If(e, [body], $loc) }
| IF LP e = expr RP LB body1 = list(stmt) RB ELSE LB body2 = list(stmt) RB  { IfElse(e, body1, body2, $loc) }
| WHILE LP e = expr RP LB body = list(stmt) RB                              { While(e, body, $loc) }
| WHILE LP e = expr RP body = stmt                                          { While(e, [body], $loc) }
| BREAK SEMICOLON                                                           { Break($loc) }
| CONTINUE SEMICOLON                                                        { Continue($loc) }
| e = expr SEMICOLON                                                        { Expr(e, $loc) }
| SEMICOLON                                                                 { Empty($loc) }
;

call_arg:
|                                                                           { [] }
| e = expr COMMA reste = call_arg                                           { e::reste }
| e = expr                                                                  { [e] }
;

// Pour OR et AND on cast les int en bool
expr:
| c = CST_INT                                                               { Cst_int(c, $loc) }
| c = CST_CHAR                                                              { Cst_char(c, $loc) }
| c = CST_STRING                                                            { Cst_string(c, $loc) }
| fct = IDENT LP argu = call_arg RP                                         { Call(fct,argu,$loc) }
| id = IDENT                                                                { Var(id,0,[],$loc) }
| pointer = pointer_type id = IDENT                                         { Var(id,pointer,[],$loc) }
| id = IDENT dims = array_dims                                              { Var(id,0,dims,$loc) }
| pointer = pointer_type id = IDENT dims = array_dims                       { Var(id,pointer,dims,$loc) }
| NOT e = expr                                                              { Binop (Eq, Cst_int(0,$loc), e, $loc) } 
| e1 = expr AND e2 = expr                                                   { Binop (And, Binop (Neq, Cst_int(0,$loc), e1, $loc), Binop (Neq, Cst_int(0,$loc), e2, $loc), $loc) }
| e1 = expr OR e2 = expr                                                    { Binop (Or, Binop (Neq, Cst_int(0,$loc), e1, $loc), Binop (Neq, Cst_int(0,$loc), e2, $loc), $loc) }
| e1 = expr o = op e2 = expr                                                { Binop (o, e1, e2, $loc) }
| MINUS e = expr %prec uminus                                               { Binop (Sub, Cst_int(0,$loc), e, $loc) }
| ADDR id = IDENT                                                           { Address(id,$loc) }
| LP e = expr RP                                                            { e }
| SIZEOF LP t = c_type RP                                                   { Sizeof(t, $loc) }
;

%inline op:
| PLUS                                                                      { Add }
| MINUS                                                                     { Sub }
| TIMES                                                                     { Mul }
| DIV                                                                       { Div }
| MOD                                                                       { Mod }
| LEQ                                                                       { Leq }
| GEQ                                                                       { Geq }
| GE                                                                        { Ge  }
| LE                                                                        { Le  }
| NEQ                                                                       { Neq }
| EQQ                                                                       { Eq  }
;