(* Author: Herry (herry13@gmail.com)
   This module contains functions that are used in parsing. *)

open Syntax

(*******************************************************************
 * lexer helper type and functions
 *******************************************************************)

(** The Lexstack type. **)
type 'a t =
	{
		mutable stack    : (string * in_channel * Lexing.lexbuf) list;
		mutable filename : string;
		mutable chan     : in_channel;
		mutable lexbuf   : Lexing.lexbuf;
		lexfunc          : Lexing.lexbuf -> 'a;
	}
;;

(** (filename, line-number, column-number, token) **)
exception ParseError of string * int * int * string ;;

let string_of_parse_error pe =
	match pe with
	| ParseError (fname, lnum, lpos, token) ->
		"--- Parse error ---\nfile:   " ^ fname ^
		"\nline:   " ^ (string_of_int lnum) ^
		"\ncolumn: " ^ (string_of_int lpos) ^
		"\ntoken:  '" ^ token ^ "'"
	| _ -> raise (Failure "invalid parameter")
;;

(** 
 * Create a lexstack with an initial top level filename and the lexer
 * function.
 *)
let create top_filename lexer_function =
	let chan = open_in top_filename in
	{
		stack = [];
		filename = top_filename;
		chan = chan;
		lexbuf = Lexing.from_channel chan;
		lexfunc = lexer_function
	}

let create_stdin lexer_function =
    {
        stack = [];
        filename = "<STDIN>";
        chan = stdin;
        lexbuf = Lexing.from_channel stdin;
        lexfunc = lexer_function
    }

(** Get the current lexeme. **)
let lexeme ls = Lexing.lexeme ls.lexbuf

(** Get filename, line number and column number of current lexeme. **)
let current_pos ls =
	let pos = Lexing.lexeme_end_p ls.lexbuf in
	let linepos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol -
		String.length (Lexing.lexeme ls.lexbuf)
	in
	ls.filename, pos.Lexing.pos_lnum, linepos

(** default file extension **)
let file_extension = ".nuri" ;;

(** library paths **)
let library_paths : string list ref = ref [] ;;

(**
 * If file '<dir>/x.nuri' exists then return the path,
 * or if file '<dir>/x/x.nuri' exists then return the path,
 * or return an empty string.
 *)
let find_file_in_directory ?extension:(ext=file_extension) file dir =
    let dir = dir ^ "/" in
    let file1 = dir ^ file ^ ext in
    if Sys.file_exists file1 then file1
    else
        let file2 = dir ^ file ^ "/" ^ file ^ ext in
        if Sys.file_exists file2 then file2
        else ""
;;

(**
 * If file='x', then find an imported file based on the following priorities:
 * 1. 'x.nuri' in the working directory
 * 2. 'x/x.nuri' in the working directory
 * 3. '<dir1>/x.nuri'   where NURI_LIB=<dir1>:<dir2>:...
 * 4. '<dir1>/x/x.nuri' where NURI_LIB=<dir1>:<dir2>:...
 * 5. '<dir2>/x.nuri'   where NURI_LIB=<dir1>:<dir2>:...
 * 6. '<dir2>/x/x.nuri' where NURI_LIB=<dir1>:<dir2>:...
 * 7. ...and so on
 *)
let find_imported_file file =
    let regexp = Str.regexp ".+\\.nuri" in
    let ext = if Str.string_match regexp file 0 then ""
              else file_extension
    in
    let rec find_file paths =
        match paths with
        | [] ->
            (
                prerr_endline ("[err1500] cannot import '" ^ file ^ "'");
                exit 1500
            )
        | path :: rest ->
            (
                let file1 = find_file_in_directory ~extension:ext file path in
                if file1 <> "" then file1
                else find_file rest
            )
    in
    find_file ("." :: !library_paths)
;;

(** holds a list of files that have been imported **)
let imported_files = ref [] ;;

(**
 * Find the imported file, and if it has been imported before then
 * returns an empty string (""), or returns the file.
 *)
let get_imported_file file : string =
    if file = "" then file
    else
        let file = find_imported_file file in
        if List.exists (fun f -> f = file) !imported_files then ""
        else (
            imported_files := file :: !imported_files;
            file
        )
;;

(**
 * The next token need to accept an unused dummy lexbuf so that
 * a closure consisting of the function and a lexstack can be passed
 * to the ocamlyacc generated parser.
 *)
let rec get_token ls dummy_lexbuf =
	let token = ls.lexfunc ls.lexbuf in
	match token with
	| Parser.INCLUDE_FILE file ->
		(* parse included file: this is SF-style include (the included file
		   must be legal statements) *)
		Parser.INCLUDE
		(
			let lexstack = create file Lexer.token in
			try 
				Parser.include_file (get_token lexstack) dummy_lexbuf
			with e -> check_error_include_file e lexstack
		)
    | Parser.IMPORT_FILE file ->
        Parser.IMPORT
        (
            let imported_file = get_imported_file file in
            if imported_file <> "" then (
                let lexstack =
                    create imported_file Lexer.token
                in
                try
                    Parser.import_file (get_token lexstack) dummy_lexbuf
                with e -> check_error_root_include_file e lexstack
            )
            else (fun c -> c)
        )
	| Parser.EOF ->
		(
			match ls.stack with
			| [] -> Parser.EOF (* buffer is empty, then return EOF *)
			| (fn, ch, lb) :: tail ->
				(* buffer isn't empty, then continue to parse top file *) 
				ls.filename <- fn;
				ls.chan <- ch;
				ls.stack <- tail;
				ls.lexbuf <- lb;
				get_token ls dummy_lexbuf
		)
  | _ -> token

(**
 * Catch the exception, when it is a parse error (Parse_error)
 * then throw a ParseError exception containing a filename, a line number
 * a column number, and a last token that produces the error,
 * otherwise just throw again the exception
 *)
and check_error_include_file e lexstack =
	match e with
	| Parsing.Parse_error ->
		let fname, lnum, lpos = current_pos lexstack in
		raise (ParseError (fname, lnum, lpos, (lexeme lexstack)))
	| e -> raise e

and check_error_root_include_file e lexstack =
	match e with
	| Parsing.Parse_error ->
		let fname, lnum, lpos = current_pos lexstack in
		raise (ParseError (fname, lnum, lpos, (lexeme lexstack)))
	| e -> raise e

and check_error e lexstack =
	match e with
	| Parsing.Parse_error ->
		let fname, lnum, lpos = current_pos lexstack in
		raise (ParseError (fname, lnum, lpos, (lexeme lexstack)))
	| e -> raise e
;;

let ast_of_file file =
    (* create a dummy lexing buffer *)
	let dummy_lexbuf = Lexing.from_string "" in
    (* create a lexing stack *)
	let lexstack = 
        if file <> "" then create file Lexer.token
        else create_stdin Lexer.token
    in
    (* reset imported files list *)
    imported_files := [];
	try
        (* parse the file and then return the AST *)
		Parser.nuri (get_token lexstack) dummy_lexbuf
	with
	| e -> check_error e lexstack
;;
