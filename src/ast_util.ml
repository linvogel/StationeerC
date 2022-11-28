open Ast


let string_of_ident (i: ident) = 
  match i with
  | Ident s -> s

let string_of_lhs (l: lhs) =
	match l with
	| VarName id -> string_of_ident id
	| IOField (port, field) -> (string_of_ident port) ^ "." ^ (string_of_ident field)
	
let string_of_bin_op (o: bin_op) =
	match o with
	| PLUS -> "+"
	| MINUS -> "-"
	| ASTERISK -> "*"
	| SLASH -> "/"
	| MOD -> "%"
	| AND -> "&"
	| OR -> "|"
	| XOR -> "^"
	| EQUAL -> "=="
	| GREATER -> ">"
	| GREATER_EQ -> ">="
	| LESS -> "<"
	| LESS_EQ -> "<="
	| NOT_EQUAL -> "!="

let string_of_un_op (o: un_op) =
	match o with
	| NOT -> "!"
	| MINUS -> "-"

let rec string_of_expr (e:expr) =
  match e with
  | Number (_, f) -> string_of_float f
  | Identifier (_, id) -> string_of_ident id
  | FCall (_, id, args) -> (string_of_ident id) ^ "(" ^ (List.map string_of_expr args |> String.concat ", ") ^ ")"
  | If (_, cnd, blk_then, blk_else) -> "if (" ^ (string_of_expr cnd) ^ ") " ^ (string_of_block blk_then) ^ " else " ^ (string_of_block blk_else)
  | While (_, cnd, blk) -> "if (" ^ (string_of_expr cnd) ^ ") " ^ (string_of_block blk)
  | For (_, e0, cnd, e2, blk) -> "for (" ^ (string_of_expr e0) ^ "; " ^ (string_of_expr cnd) ^ "; " ^ (string_of_expr e2) ^ ") " ^ (string_of_block blk)
  | DeclAssign (_, id, e) -> "let " ^ (string_of_ident id) ^ " = " ^ (string_of_expr e)
  | Assign (_, l, e) -> (string_of_lhs l) ^ " = " ^ (string_of_expr e)
  | BinOp (_, op, el, er) -> (string_of_expr el) ^ " " ^ (string_of_bin_op op) ^ " " ^ (string_of_expr er)
  | UnOp (_, op, e) -> (string_of_un_op op) ^ (string_of_expr e)
and string_of_block (b: block) =
	match b with
	| Block (_, exprs) -> "{\n" ^ ((List.map (fun x -> (string_of_expr x) ^ ";") exprs) |> String.concat "\n") ^ "}"
let string_of_define (d:define) = 
  match d with
  | Define (_, name, value) -> "define " ^ (string_of_ident name) ^ " " ^ (string_of_ident value) ^ ";"

let string_of_global (g:global) =
  match g with
  | Global (_, name, e) -> "let " ^ (string_of_ident name) ^ " = " ^ (string_of_expr e) ^ ";"

let string_of_program (p:program) =
  match p with
  | Program (_, defs, gbls, funcs) -> begin
      let def_str = List.map string_of_define defs |> String.concat "\n" in
      let gbl_str = List.map string_of_global gbls |> String.concat "\n" in
	  def_str ^ "\n" ^ gbl_str
    end