{
	open Lexing
	open Parser
	
	exception SyntaxError of string
	
	let next_line (lexbuf:lexbuf) = 
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{ pos with pos_bol = lexbuf.lex_curr_pos;
					   pos_lnum = pos.pos_lnum + 1;
			}
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let num = ('+'|'-')? (digit)* ('.' (digit)*)? ('e' ('+'|'-')? (digit)+ )?
let id = (alpha) (alpha|digit|'_')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"


rule read_token = parse
	| "(" { LPAREN }
	| ")" { RPAREN }
	| "function" { FUNCTION }
	| "{" { LBRACE }
	| "}" { RBRACE }
	| "." { DOT }
	| "," { COMMA }
	| ";" { SEMI }
	| "define" { DEFINE }
	| "if" { IF }
	| "else" { ELSE }
	| "while" { WHILE }
	| "for" { FOR }
	| "let" { LET }
	| "+" { PLUS }
	| "-" { MINUS }
	| "*" { ASTERISK }
	| "/" { SLASH }
	| "%" { MOD }
	| "&" { AND }
	| "|" { OR }
	| "^" { XOR }
	| "!" { NOT }
	| "==" { EQUAL }
	| "<" { GREATER }
	| ">" { LESS }
	| ">=" { LESS_EQ }
	| "<=" { GREATER_EQ }
	| "!=" { NOT_EQUAL }
	| "=" { ASSIGN }
	| whitespace { read_token lexbuf }
	| "//" { read_single_comment lexbuf }
	| "/*" { read_multi_comment lexbuf }
	| num { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
	| id { ID (Lexing.lexeme lexbuf) }
	| newline { next_line lexbuf; read_token lexbuf }
	| eof { EOF }
	| _ { raise (SyntaxError ("LexerError: Unknown token: " ^ Lexing.lexeme lexbuf)) }

and read_single_comment = parse
	| newline { next_line lexbuf; read_token lexbuf }
	| eof { EOF }
	| _ { read_single_comment lexbuf }

and read_multi_comment = parse
	| "*/" { read_token lexbuf }
	| newline { next_line lexbuf; read_multi_comment lexbuf }
	| eof { raise (SyntaxError "LexerError: Unclosed Multi-Line Comment!")}
	| _ { read_multi_comment lexbuf }