(* Syntaxe abstraite pour notre langage *)

type ppos = Lexing.position * Lexing.position

type program = gdef list 

and gdef =
  | Function of string*stmt list*stmt list*ppos   
  | Gvar of string*c_type*expr list*ppos

and stmt = 
  | Empty of ppos
  | Expr of expr*ppos
  | Lvar of string*c_type*ppos
  | Tab_int of string*c_type*expr list*ppos
  | Return of expr*ppos
  | Set of string*int*expr*ppos
  | Set_tab of string*int*expr list*expr*ppos
  | Malloc of string*expr*ppos
  | If of expr*stmt list*ppos
  | IfElse of expr*stmt list*stmt list*ppos
  | While of expr*stmt list*ppos
  | Break of ppos
  | Continue of ppos

and expr =
  | Cst_int of int*ppos
  | Cst_char of string*ppos
  | Cst_string of string*ppos
  | Var of string*int*expr list*ppos
  | Address of string*ppos
  | Binop of binop * expr * expr*ppos
  | Call of string * expr list*ppos
  | Sizeof of c_type*ppos

and c_type =
  | Int of int*ppos
  | Char of int*ppos

and binop = Add | Sub | Mul | Div | Mod | Leq | Le | Geq | Ge | Neq | Eq | Or | And

let binopname = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Leq -> "<="
  | Le  -> "<"
  | Geq -> ">="
  | Ge  -> ">"
  | Neq -> "!="
  | Eq  -> "=="
  | Or  -> "||"
  | And -> "&&"

let rec pos ((s,e):ppos) =
  [ "start_line", `Int s.pos_lnum;
    "start_char", `Int (s.pos_cnum-s.pos_bol);
    "end_line", `Int e.pos_lnum;
    "end_char", `Int (e.pos_cnum-e.pos_bol) ]

and toJSONctype = function
  | Int(n,p)  -> `Assoc (["type", `String "int"; "pointer", `Int n] @ pos p)
  | Char(n,p) -> `Assoc (["type", `String "char"; "pointer", `Int n] @ pos p)

and toJSONexpr = function
  | Cst_int(i,p)              -> `Assoc (["type", `String "cst_int"; "value", `Int i] @ pos p)
  | Cst_char(i,p)             -> `Assoc (["type", `String "cst_char"; "value", `String i] @ pos p)
  | Cst_string(i,p)           -> `Assoc (["type", `String "cst_string"; "value", `String i] @ pos p)
  | Var(s,pointer,dims,p)     -> `Assoc (["type", `String "var"; "pointer", `Int pointer; "dimensions", `List (List.map toJSONexpr dims); "name", `String s] @ pos p)
  | Address(s,p)              -> `Assoc (["type", `String "address"; "var", `String s] @ pos p)
  | Binop (b,e1,e2,p)         -> `Assoc (["type", `String "binop"; "binop", `String (binopname b); "e1", toJSONexpr e1; "e2", toJSONexpr e2] @ pos p)
  | Call(funname, argvalue,p) -> `Assoc (["type", `String "application"; "function", `String funname; "argvalue", `List (List.map toJSONexpr argvalue) ]@pos p)
  | Sizeof(t,p)               -> `Assoc (["type", `String "sizeof"; "ctype", toJSONctype t] @ pos p)

let rec toJSONinst = function
  | Empty(p)                    -> `Assoc (["action", `String "empty"]@pos p)
  | Expr(e,p)                   -> `Assoc (["action", `String "expr"; "expr", toJSONexpr e]@pos p)
  | Lvar(s,t,p)                 -> `Assoc (["action", `String "vardef"; "ctype", toJSONctype t; "name", `String s]@pos p)
  | Tab_int(s,t,dims,p)         -> `Assoc (["action", `String "tabintdef"; "ctype", toJSONctype t; "dimensions", `List (List.map toJSONexpr dims); "name", `String s]@pos p)
  | Set(s,pointer,e,p)          -> `Assoc (["action", `String "varset"; "pointer", `Int pointer; "name", `String s; "expr", toJSONexpr e]@pos p)
  | Set_tab(s,pointer,dims,e,p) -> `Assoc (["action", `String "tabset"; "pointer", `Int pointer; "dimensions", `List (List.map toJSONexpr dims); "name", `String s; "expr", toJSONexpr e]@pos p)
  | Malloc(s,e,p)               -> `Assoc (["action", `String "malloc"; "name", `String s; "expr", toJSONexpr e]@pos p)
  | Return(e,p)                 -> `Assoc (["action", `String "return"; "expr", toJSONexpr e]@pos p)
  | If(e,b,p)                   -> `Assoc (["action", `String "if"; "expr", toJSONexpr e; "body", `List (List.map toJSONinst b)]@pos p)
  | IfElse(e,b1,b2,p)           -> `Assoc (["action", `String "ifelse"; "expr", toJSONexpr e; "body1", `List (List.map toJSONinst b1); "body2", `List (List.map toJSONinst b2)]@pos p)
  | While(e,b,p)                -> `Assoc (["action", `String "while"; "expr", toJSONexpr e; "body", `List (List.map toJSONinst b)]@pos p)
  | Break(p)                    -> `Assoc (["action", `String "break"]@pos p)
  | Continue(p)                 -> `Assoc (["action", `String "continue"]@pos p)

let toJSONdef = function
  | Gvar(name,t,dims,p)            -> `Assoc (["action", `String "gvardef"; "ctype", toJSONctype t; "dimensions", `List (List.map toJSONexpr dims); "name", `String name ]@pos p)
  | Function(name,varname,body,p)  -> `Assoc (["action", `String "fundef"; "name", `String name; "arg", `List (List.map toJSONinst varname); "body", `List (List.map toJSONinst body)]@pos p)

let toJSON p =
  `List (List.map toJSONdef p)
