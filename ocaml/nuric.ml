(* Author: Herry (herry13@gmail.com) *)

open Array
open Common

let usage_msg =
    "Usage: nuric [options] <spec-file>\n" ^
    "       nuric -d <initial_file> <goal_file>\n" ^
    "       nuric -p <initial_file> <goal_file>\n"
;;

let usage_msg_with_options =
    usage_msg ^ "\nwhere [options] are:"
;;

(* options *)
let opt_verbose = ref false ;;
let opt_check_syntax = ref false ;;
let opt_check_type = ref false ;;
let opt_no_main = ref false ;;
let opt_fdr = ref false ;;
let opt_planning = ref false ;;
let opt_not_eval_global_constraints = ref false ;;
let opt_stdin = ref false ;;

let files : string list ref = ref [] ;;

(* environment variable that holds the library paths *)
let env_var_library_paths = "NURI_LIB" ;;

let set_library_paths =
    try
        Parser_helper.library_paths :=
            Str.split (Str.regexp ":") (Sys.getenv env_var_library_paths)
    with
        Not_found -> ()
;;

let evaluate_global_constraints store =
    match Domain.find store ["global"] with
    | Domain.Undefined -> true
    | Domain.Val (Domain.Global g) -> Constraint.apply store g
    | _ -> false
;;

let generate_json ast =
    let mainReference =
        if !opt_no_main then []
        else ["main"]
    in
    (* let ast = Parser_helper.ast_of_file file in *)
    let types = Type.nuriSpecification ~main:mainReference ast in
    let store = Valuation.nuriSpecification ~main:mainReference ast in
    if not !opt_not_eval_global_constraints &&
       not (evaluate_global_constraints store) then (
        prerr_endline "[err700] The specification violates the global constraints.";
        exit 700
    );
    print_endline (Json.of_store types store)
;;

let check_type ast =
    let mainReference =
        if !opt_no_main then []
        else ["main"]
    in
    print_endline (Type.string_of_map (Type.nuriSpecification
        ~main:mainReference ast))
;;

let fd_plan initFile goalFile =
    let fd_preprocessor = "FD_PREPROCESS" in
    let fd_search       = "FD_SEARCH" in
    let fd_option       = "FD_OPTIONS" in
    let fd_debug        = "FD_DEBUG" in
    let sas_file        = "output.sas" in
    let plan_file       = "sas_plan" in
    let default_search_options = "--search \"lazy_greedy(ff())\"" in
    let out_files =
        [sas_file; "plan_numbers_and_cost"; "output"; "output.log"; plan_file]
    in
    try
        let preprocessor   = Sys.getenv fd_preprocessor in
        let search         = Sys.getenv fd_search in
        let search_options =
            try
                Sys.getenv fd_option
            with
            | e -> default_search_options
        in
        let debug =
            try
                let _ = Sys.getenv fd_debug in
                true
            with
            | e -> false
        in
        if not (Sys.file_exists preprocessor) then (
            prerr_string ("[err1200] " ^ preprocessor ^ " is not exist!\n\n");
            exit 1200
        );
        if not (Sys.file_exists search) then (
            prerr_string ("[err1201] " ^ search ^ " is not exist!\n\n");
            exit 1201
        );
        (* generate FDR *)
        let fdr = Fdr.of_nuri (Parser_helper.ast_of_file initFile)
                              (Parser_helper.ast_of_file goalFile)
        in
        (* save FDR to sas_file *)
        write_file sas_file (Fdr.string_of fdr);
        (* invoke preprocessor *)
        let cmd = if !opt_verbose then preprocessor ^ "<" ^ sas_file
                  else preprocessor ^ "<" ^ sas_file ^ ">>output.log"
        in
        if not ((Sys.command cmd) = 0) then (
            prerr_string "[err1202] Preprocessor failed\n\n";
            exit 1202
        );
        (* invoke search *)
        let cmd =
            if !opt_verbose then search ^ " " ^ search_options ^ "<output"
            else search ^ " " ^ search_options ^ "<output>>output.log"
        in
        if not ((Sys.command cmd) = 0) then (
            prerr_string "[err1203] Search failed\n\n";
            exit 1203
        );
        (* read 'plan_file' *)
        let plan = if Sys.file_exists plan_file then read_file plan_file
                   else "null"
        in
        let plan = Plan.json_of_parallel (Plan.parallel_of (Fdr.to_nuri_plan plan fdr))
        in
        if not debug then (
            try
                List.iter (fun f -> Sys.remove f) out_files
            with _ -> ()
        );
        plan
    with
        Not_found ->
            prerr_string ("[err1204] Environment variable FD_PREPROCESS" ^
                " or FD_SEARCH is not defined.\n\n");
            exit 1204
;;


(** main function **)
let main =
    let options = [
        ("-x", Arg.Set opt_check_syntax,
            "    Check syntax.");
        ("-t", Arg.Set opt_check_type,
            "    Check type.");
        ("-m", Arg.Set opt_no_main,
            "    Do not extract main object.");
        ("-g", Arg.Set opt_not_eval_global_constraints,
            "    Do not evaluate global constraints.");
        ("-i", Arg.Set opt_stdin,
            "    Read from standard input (STDIN).");
        ("-d", Arg.Set opt_fdr,
            "    Generate Finite Domain Representation.");
        ("-p", Arg.Set opt_planning,
            "    Planning.");
    ]
    in
    Arg.parse options (fun f -> files := f :: !files) usage_msg_with_options;

    try
        set_library_paths;

        if !opt_check_syntax then
            if !opt_stdin then
                let _ = Parser_helper.ast_of_file "" in ()
            else
                List.iter (fun f ->
                    let _ = Parser_helper.ast_of_file f in
                    ()
                ) !files
        else if !opt_check_type then (
            if !opt_stdin then
                check_type (Parser_helper.ast_of_file "")
            else
                List.iter (fun f ->
                    check_type (Parser_helper.ast_of_file f)
                ) !files
        )
        else if !opt_fdr then (
            match !files with
            | goalFile :: initFile :: [] -> print_endline (Fdr.string_of (Fdr.of_files initFile goalFile))
            | _                          -> prerr_endline "Usage: nuri -f <initial_file> <goal_file>"
        )
        else if !opt_planning then (
            match !files with
            | goalFile :: initFile :: [] -> print_endline (fd_plan initFile goalFile)
            | _                          -> prerr_endline "Usage: nuri -p <initial_file> <goal_file>"
        )
        else (
            match !opt_stdin, !files with
            | true, _ -> generate_json (Parser_helper.ast_of_file "")
            | _, []   -> prerr_endline usage_msg
            | _       -> List.iter (fun f ->
                             generate_json (Parser_helper.ast_of_file f)
                         ) !files
        )
    with
    | Parser_helper.ParseError (f, l, p, t) -> (
            prerr_endline (Parser_helper.string_of_parse_error
                (Parser_helper.ParseError (f, l, p, t)));
            exit 1501
        )
    | Domain.SfError (code, msg)
    | Type.TypeError (code, msg) -> (
            prerr_endline msg;
            exit code
        )
;;

let _ = main ;;
