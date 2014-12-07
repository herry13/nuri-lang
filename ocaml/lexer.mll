(* Author: Herry (herry13@gmail.com) *)

{
    open Common
	open Lexing
	open Parser

	exception SyntaxError of string

	let next_line lexbuf =
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{
				pos with pos_bol = lexbuf.lex_curr_pos;
				pos_lnum = pos.pos_lnum + 1
			}
    ;;

	(* reserved words *)
	let keywords = [] ;;

    (* true if given 'id' is a keyword, otherwise false *)
	let is_keyword id =
		let rec check id words =
			match words with
			| head :: tail -> if head = id then true else check id tail
			| _            -> false
		in
		check id keywords
    ;;
}

(*************************************************************************
 * reserved symbols
 * - assignment:  ';'
 * - type      :  ':'
 * - vector    :  "[]"  ','
 * - reference :  '.'  ":="
 * - function  :  '$'
 * - constraint:  '='  "!="  ">="  "<="  '>'  '<'  "{}"  "()" "not" "if-then"
 * - string    :  '\''  '"'  '`'
 * - arithmetic:  '+'  '-'  '*'  '/'  '%'
 * - logic     :  '!'  "&&"  "||"  "=>"
 * - comparison:  "=="
 *************************************************************************)

(** regular expressions **)

(* white spaces *)
let white            = [' ' '\t']+
let newline          = '\r' | '\n' | "\r\n"

(* identifier *)
let ident            = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*

(* single and multiple lines comment *)
let comment          = "//" [^'\n''\r']*
let comments         = '/' '*'+ (('*'[^'/'])+|[^'*']+)* '*'+ '/'

(* file inclusion *)
let include_file      = "#include"
let import_file       = "import"

(* built-in values *)
let true_value       = "true"
let false_value      = "false"
let int              = '-'? ['0'-'9']+
let digit            = ['0'-'9']
let frac             = '.' digit*
let exp              = ['e' 'E'] ['-' '+']? digit+
let float            = digit* frac? exp?
let string           = '"' ('\\'_|[^'\\' '"'])* '"'
let null_value       = "null" | "NULL"
let tbd_value        = "TBD"
let unknown_value    = "unknown"
let nothing_value    = "nothing"

(* expression *)
let _else            = "else"

(* object inheritance (prototype or schema) *)
let extends          = "extends"
let isa              = "isa"

(* typing *)
let schema           = "schema"
let enum             = "enum"
let t_bool           = "bool"
let t_int            = "int"
let t_float          = "float"
let t_str            = "string"
let t_obj            = "object"

(* constraint *)
let _global          = "global" | "#always"
let _sometime        = "#sometime"
let _atleast         = "#atleast"
let _atmost          = "#atmost"
let _alldifferent    = "#alldifferent"
let _in              = "in"
let _if              = "if"
let _then            = "then"
let _not             = "not"

(* action *)
let action           = "def" | "action"
let cost             = "cost"
let conditions       = "conditions" | "condition"
let effects          = "effects" | "effect"

(* debug *)
let hash_echo        = "#echo"

(* lexer rules *)
rule token =
	parse
	| white       { token lexbuf }
	| newline     { next_line lexbuf; token lexbuf }
	| comment     { token lexbuf }
	| '/' '*'+    { read_comments lexbuf; token lexbuf }
	| include_file white '\''
	              { INCLUDE_FILE (read_string (Buffer.create 17) lexbuf) }
    | import_file white '\''
                  { IMPORT_FILE (read_string (Buffer.create 17) lexbuf) }
    | "=~" ' '* '/' { REGEXP (read_regex (Buffer.create 17) lexbuf) }
	| ','         { COMMA }
	| '{'         { BEGIN }
	| '}'         { END }
	| '['         { LBRACKET }
	| ']'         { RBRACKET }
	| '('         { LPARENTHESIS }
	| ')'         { RPARENTHESIS }
	| ';'         { EOS }
	| '.'         { SEP }
	| ':'         { COLON }
	| '*'         { ASTERIX }
    | '+'         { TOK_PLUS }
    | '-'         { TOK_SUBTRACT }
    | '/'         { TOK_DIVIDER }
    | '%'         { TOK_MODULO }
	| '='         { EQUAL }
	| "!="        { NOT_EQUAL }
	| ">="        { TOK_GREATER_EQUAL }
	| "<="        { TOK_LESS_EQUAL }
	| '>'         { TOK_GREATER }
	| '<'         { TOK_LESS }
    | ":="        { TOK_COLON_EQUAL }
    | "=="        { TOK_EQUAL_EQUAL }
    | "&&"        { TOK_AND }
    | "||"        { TOK_OR  }
    | "=>"        { TOK_IMPLY }
    | '!'         { TOK_EXCLAMATION }
    | '$'         { TOK_DOLLAR }
	| int         { INT (Lexing.lexeme lexbuf) }
	| float       { FLOAT (Lexing.lexeme lexbuf) }
	| true_value  { BOOL "true" }
	| false_value { BOOL "false" }
	| null_value  { NULL }
	| tbd_value   { TOK_TBD }
	| unknown_value { TOK_UNKNOWN }
	| nothing_value { TOK_NOTHING }
	| extends     { EXTENDS }
	| isa         { ISA }
	| schema      { SCHEMA }
    | enum        { ENUM }
	| t_bool      { TBOOL }
	| t_int       { TINT }
	| t_float     { TFLOAT }
	| t_str       { TSTR }
	| t_obj       { TOBJ }
	| _in         { IN }
	| _not        { NOT }
	| _if         { IF }
	| _then       { THEN }
    | _else       { ELSE }
	| _global     { GLOBAL }
    | _sometime   { SOMETIME }
    | _atleast    { ATLEAST }
    | _atmost     { ATMOST }
    | _alldifferent { ALLDIFFERENT }
	| cost        { COST }
	| conditions  { CONDITIONS }
	| effects     { EFFECTS }
	| action      { ACTION }
    | hash_echo   { HASH_ECHO "#echo" }
	| '\''        { STRING (read_string (Buffer.create 17) lexbuf) }
    | '"'         { ISTRING (read_interpolated_string (Buffer.create 17) lexbuf) }
    | '`'         { SHELL (read_shell (Buffer.create 17) lexbuf) }
	| ident       {
	              	let id = Lexing.lexeme lexbuf in
	                if is_keyword id then raise (SyntaxError (id ^ " is a reserved identifier"))
	              	else ID id
	              }
	| eof         { EOF }

and read_interpolated_string buf =
	parse
	| '"'           { Buffer.contents buf }
	| '\\' '/'      { Buffer.add_char buf '/'; read_interpolated_string buf lexbuf }
	| '\\' '\\'     { Buffer.add_char buf '\\'; read_interpolated_string buf lexbuf }
	| '\\' 'b'      { Buffer.add_char buf '\b'; read_interpolated_string buf lexbuf }
	| '\\' 'f'      { Buffer.add_char buf '\012'; read_interpolated_string buf lexbuf }
	| '\\' 'n'      { Buffer.add_char buf '\n'; read_interpolated_string buf lexbuf }
	| '\\' 'r'      { Buffer.add_char buf '\r'; read_interpolated_string buf lexbuf }
	| '\\' 't'      { Buffer.add_char buf '\t'; read_interpolated_string buf lexbuf }
	| '\n'          { Buffer.add_char buf '\n'; next_line lexbuf; read_interpolated_string buf lexbuf }
	| '\r'          { Buffer.add_char buf '\r'; read_interpolated_string buf lexbuf }
	| [^ '"' '\\' '\n' '\r']+
	                {
	                	Buffer.add_string buf (Lexing.lexeme lexbuf);
	                	read_interpolated_string buf lexbuf
	                }
	| _             { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
	| eof           { raise (SyntaxError "String is not terminated") }

and read_string buf =
    parse
    | '\''          { Buffer.contents buf }
    | '\\' '\''     { buf <. '\''; read_string buf lexbuf }
    | '\\' '\\'     { buf <. '\\'; read_string buf lexbuf }
    | [^ '\'' '\\']+
                    { buf << (Lexing.lexeme lexbuf); read_string buf lexbuf }
    | _             { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof           { raise (SyntaxError "String is not terminated.") }

and read_shell buf =
    parse
    | '`'           { Buffer.contents buf }
    | '\\' '`'      { Buffer.add_char buf '`'; read_shell buf lexbuf }
    | '\\' '\\'     { Buffer.add_char buf '\\'; read_shell buf lexbuf }
    | [^ '`' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_shell buf lexbuf }
    | _             { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof           { raise (SyntaxError "Shell command is not terminated.") }

and read_regex buf =
    parse
    | '/'       { Buffer.contents buf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_regex buf lexbuf }
    | '\\' '/'  { Buffer.add_char buf '/'; read_regex buf lexbuf }
    | '|'       { Buffer.add_string buf "\\|"; read_regex buf lexbuf }
    | '('       { Buffer.add_string buf "\\("; read_regex buf lexbuf }
    | ')'       { Buffer.add_string buf "\\)"; read_regex buf lexbuf }
    | [^ '\\' '/' '|' '(' ')']+
                { Buffer.add_string buf (Lexing.lexeme lexbuf); read_regex buf lexbuf }
    | _         { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof       { raise (SyntaxError "Regex is not terminated.") }

and read_comments =
	parse
	| '*' '/'  { }
	| '\n'     { next_line lexbuf; read_comments lexbuf }
	| _        { read_comments lexbuf }
	| eof      { raise (SyntaxError "Comment is not terminated.") }
