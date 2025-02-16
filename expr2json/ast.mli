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

val toJSON : program -> Yojson.t
