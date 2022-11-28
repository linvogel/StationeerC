%{
	[@@@coverage exclude_file]
	open Ast
%}

// tokens

%token <string> ID
%token <float> NUMBER
%token LPAREN
%token RPAREN
%token FUNCTION
%token LBRACE
%token RBRACE
%token DOT
%token COMMA
%token SEMI
%token DEFINE
%token IF
%token ELSE
%token WHILE
%token FOR
%token LET
%token PLUS
%token MINUS
%token MOD
%token ASTERISK
%token SLASH
%token AND
%token OR
%token XOR
%token NOT
%token EQUAL
%token GREATER
%token LESS
%token LESS_EQ
%token GREATER_EQ
%token NOT_EQUAL
%token ASSIGN
%token EOF

%start program

%type <Ast.program> program
%type <define> define
%type <global> global
%type <func> func

%type <block> block

%type <expr> expr
%type <lhs> lhs

%type <ident> ident
%type <un_op> un_op
%type <bin_op> bin_op

%%

program:
	| defines=list(define); globals=list(global); functions=list(func); EOF; { Program ($startpos, defines, globals, functions) }

func:
	| FUNCTION; name=ident; LPAREN; args=separated_list(COMMA, ident); RPAREN; block=block; { Function ($startpos, name, args, block) }

define:
	| DEFINE; name=ident; target=ident; SEMI; { Define ($startpos, name, target) }

global:
	| LET; name=ident; ASSIGN; e=expr; SEMI; { Global ($startpos, name, e) }

block:
	| LBRACE; expressions=list(stmt); RBRACE; { Block ($startpos, expressions) }

stmt:
	| e=expr; SEMI; { e }

ident:
	| id=ID; { Ident id }

lhs:
	| port=ident; DOT; field=ident; { IOField (port, field) }
	| name=ident; { VarName name }

expr:
	| LPAREN; e=expr; RPAREN; { e }
	| n=NUMBER; { Number ($startpos, n) }
	| id=ident; { Identifier ($startpos, id) }
	| name=ident; LPAREN; args=separated_list(COMMA, expr); RPAREN; { FCall ($startpos, name, args) }
	| IF; LPAREN; cnd=expr; RPAREN; then_block=block; ELSE; else_block=block; { If ($startpos, cnd, then_block, else_block) }
	| IF; LPAREN; cnd=expr; RPAREN; then_block=block; { If ($startpos, cnd, then_block, Block ($startpos, [])) }
	| WHILE; LPAREN; cnd=expr; RPAREN; blk=block; { While ($startpos, cnd, blk) }
	| FOR; LPAREN; e0=expr; SEMI; e1=expr; e2=expr; RPAREN; blk=block; { For ($startpos, e0, e1, e2, blk) }
	| LET; name=ident; ASSIGN; e=expr; { DeclAssign ($startpos, name, e) }
	| target=lhs; ASSIGN; e=expr; { Assign ($startpos, target, e) }
	| l=expr; op=bin_op; r=expr; { BinOp ($startpos, op, l, r) }
	| op=un_op; e=expr; { UnOp ($startpos, op, e) }

bin_op:
	| PLUS { PLUS }
	| MINUS { MINUS }
	| ASTERISK { ASTERISK }
	| SLASH { SLASH }
	| MOD { MOD }
	| AND { AND }
	| OR { OR }
	| XOR { XOR }
	| EQUAL { EQUAL }
	| GREATER { GREATER }
	| GREATER_EQ { GREATER_EQ }
	| LESS { LESS }
	| LESS_EQ { LESS_EQ }
	| NOT_EQUAL { NOT_EQUAL }

un_op:
	| NOT { NOT }
	| MINUS { MINUS }