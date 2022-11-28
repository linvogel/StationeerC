
type loc = Lexing.position

type ident = Ident of string

type bin_op = 
  | PLUS
  | MINUS
  | ASTERISK
  | SLASH
  | MOD
  | AND
  | OR
  | XOR
  | EQUAL
  | GREATER
  | GREATER_EQ
  | LESS
  | LESS_EQ
  | NOT_EQUAL

type un_op =
  | NOT
  | MINUS

type lhs =
  | VarName     of ident
  | IOField     of ident * ident

type expr =
  | Number      of loc * float
  | Identifier  of loc * ident
  | FCall       of loc * ident * expr list
  | If          of loc * expr * block * block
  | While       of loc * expr * block
  | For         of loc * expr * expr * expr * block
  | DeclAssign  of loc * ident * expr
  | Assign      of loc * lhs * expr
  | BinOp       of loc * bin_op * expr * expr
  | UnOp        of loc * un_op * expr

and block = Block of loc * expr list

type func = Function of loc * ident * ident list * block
type define = Define of loc * ident * ident
type global = Global of loc * ident * expr
type program = Program of loc * define list * global list * func list