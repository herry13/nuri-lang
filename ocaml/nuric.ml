(* Author: Herry (herry13@gmail.com) *)

open Array
open Common

let version = "0.8.0" ;;

let usage_msg =
    "Nuri compiler (version " ^ version ^ ")\n" ^
    "Usage: nuric [options] <spec-file>\n" ^
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
let opt_simple_plan = ref false ;;
let opt_not_eval_global_constraints = ref false ;;
let opt_stdin = ref false ;;
let opt_state = ref false ;;
let opt_out_file = ref "" ;;

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
    match Domain.find ["global"] store with
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
    let types = (*Type.nuriSpecification*) Typechecker.eval ~main:mainReference ast in
    let store = Valuation.eval [] [] ast in
    if not !opt_not_eval_global_constraints &&
       not (evaluate_global_constraints store) then (
        prerr_endline "[err700] The specification violates the global constraints.";
        exit 700
    );
    let store =
        if !opt_state then Domain.to_state store
        else store
    in
    print_endline (Nuri_json.of_store types store)
;;

let check_type ast =
    let mainReference =
        if !opt_no_main then []
        else ["main"]
    in
    print_endline (Type.string_of_map (Typechecker.eval
        ~main:mainReference ast))
;;

let home = String.sub Sys.executable_name 0 ((String.length Sys.executable_name) - 5) ;;

let plan initFile goalFile =
    (* generate FDR *)
    let fdr = Fdr.of_nuri (Parser_helper.ast_of_file initFile)
                          (Parser_helper.ast_of_file goalFile)
    in
    (* save FDR to sas_file *)
    let fdr_file = "output.fdr" in
    write_file fdr_file (Fdr.string_of fdr);
    (* run the planner *)
    let command = home ^ "planner " ^ fdr_file in
    if not ((Sys.command command) = 0) then (
        prerr_string "[err1201] Planning failed\n\n";
        exit 1201
    );
    (* read 'plan_file' *)
    let plan_file = "solution_plan" in
    let plan = if Sys.file_exists plan_file then read_file plan_file
               else "null"
    in
    (
        try
            Sys.remove fdr_file;
            Sys.remove plan_file
        with _ -> ()
    );
    let sequentialPlan = Fdr.to_nuri_plan plan fdr in
    if !opt_simple_plan then
        Plan.string_of_sequential sequentialPlan
    else
        let parallelPlan = Plan.parallel_of sequentialPlan in
        Plan.json_of_parallel parallelPlan
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
        ("-s", Arg.Set opt_state,
            "    State only (no action or global constraints).");
        ("-d", Arg.Set opt_fdr,
            "    Generate Finite Domain Representation.");
        ("-p", Arg.Set opt_planning,
            "    Planning.");
        ("-sp", Arg.Set opt_simple_plan,
            "   Simple plan (a list of actions).");
        ("-o", Arg.Set_string opt_out_file,
            "    Output file.");
        ("-v", Arg.Set verbose,
            "    Verbose mode.");
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
            | goalFile :: initFile :: [] -> print_endline (plan initFile goalFile)
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
    | Domain.Error (code, msg)
    (*| Type.Error (code, msg) -> (
            prerr_endline msg;
            exit code
        )*)
    | Nuri_json.Nuri_jsonError (code, msg) -> (
            prerr_endline msg;
            exit code
        )
;;

let _ = main ;;
