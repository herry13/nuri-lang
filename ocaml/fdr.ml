(* Author: Herry (herry13@gmail.com) *)

open Common
open Domain

module A = Action

type t = {
    variables : Variable.ts;
    actions   : Action.ts;
    global    : Domain._constraint
}

(*******************************************************************
 * functions for translating Nuri to FDR
 *******************************************************************)

let (!^) = Domain.(!^) ;;

let version = "3" ;;

let metric = "1" ;;

let of_header buffer =
    Buffer.add_string buffer "begin_version\n";
    Buffer.add_string buffer version;
    Buffer.add_string buffer "\nend_version\nbegin_metric\n";
    Buffer.add_string buffer metric;
    Buffer.add_string buffer "\nend_metric"
;;

let of_variables buffer variables =
    Buffer.add_char buffer '\n';
    Buffer.add_string buffer (string_of_int (Variable.total variables));
    Variable.iter (fun var ->
        Buffer.add_string buffer "\nbegin_variable\n";
        Buffer.add_string buffer (string_of_int (Variable.index var));
        Buffer.add_char buffer '_';
        Buffer.add_string buffer !^(Variable.name var);
        Buffer.add_string buffer "\n-1\n";
        Buffer.add_string buffer (string_of_int (Variable.size var));
        Buffer.add_char buffer '\n';
        Variable.iteri_values (fun i v ->
            Buffer.add_string buffer "Atom (";
            Buffer.add_string buffer !^(Variable.name var);
            Buffer.add_char buffer ' ';
            Buffer.add_string buffer (Json.of_value v);
            Buffer.add_string buffer ")\n";
        ) var;
        Buffer.add_string buffer "end_variable"
    ) variables
;;

let of_init buffer variables =
    Buffer.add_string buffer "\nbegin_state";
    Variable.iter (fun var ->
        let i = Variable.index_of_value (Variable.init var) var in
        (* initial state is not satisfying the global constraint *)
        if i < 0
            then error 901 "initial state is not satisfying the global constraints"
        else
            Buffer.add_char buffer '\n';
            Buffer.add_string buffer (string_of_int i)
    ) variables;
    Buffer.add_string buffer "\nend_state"
;;

let of_goal buffer variables useDummy =
    let buf = Buffer.create 50 in
    let counter = ref 0 in
    Variable.iter (fun var ->
        (**
         * Set the variable's goal, when:
         * - it has more than one value
         * - if it is a dummy variable, then the global constraint
         *   must be exist
         *)
        let goal = Variable.goal var in
        if goal <> TBD then (
            let i = Variable.index_of_value goal var in
            if i < 0
                then error 902 ("The goal value of " ^ !^(Variable.name var) ^
                                " is not a member of the domain.")
            else if (Variable.size var) > 1 &&
                    ((Variable.index var) > 0 || useDummy) && goal <> TBD
                then (
                    Buffer.add_char buf '\n';
                    Buffer.add_string buf (string_of_int (Variable.index var));
                    Buffer.add_char buf ' ';
                    Buffer.add_string buf (string_of_int i);
                    counter := !counter + 1;
                )
        )
    ) variables;
    Buffer.add_string buffer "\nbegin_goal";
    Buffer.add_char buffer '\n';
    Buffer.add_string buffer (string_of_int !counter);
    Buffer.add_buffer buffer buf;
    Buffer.add_string buffer "\nend_goal"
;;

(**
 * Generate the FDR mutex
 *)
let of_mutex buffer variables =
    (*Buffer.add_char buffer '\n';
    Buffer.add_string buffer (string_of_int (Variable.total variables));
    Variable.iter (fun var ->
        Buffer.add_string buffer "\nbegin_mutex_group\n";
        let index = string_of_int (Variable.index var) in
        Buffer.add_string buffer (string_of_int (Variable.size var));
        Buffer.add_char buffer '\n';
        Variable.iteri_values (fun i _ ->
            Buffer.add_string buffer index;
            Buffer.add_char buffer ' ';
            Buffer.add_string buffer (string_of_int i);
            Buffer.add_char buffer '\n';
        ) var;
        Buffer.add_string buffer "end_mutex_group";
    ) variables*)
    Buffer.add_string buffer "\n0"
;;

(**
 * Generate the FDR of a given action. If there is an assignment whose
 * value is not in the variable's domain, then the action is treated as
 * "invalid".
 *)
let of_action buffer action variables actionCounter =
    let valid_operator = ref true in
    let buf = Buffer.create 50 in
    Buffer.add_string buffer "\nbegin_operator\n";
    (* name *)
    Buffer.add_string buffer (A.encode_name !actionCounter action);
    Buffer.add_char buffer '\n';
    (* prevail *)
    let prevail = Buffer.create 50 in
    let n = ref 0 in
    let pre = A.preconditions action in
    let eff = A.effects action in
    MapRef.iter (fun r v ->
        if not (MapRef.mem r eff) then
        (
            let var = Variable.find r variables in
            Buffer.add_string prevail (string_of_int (Variable.index var));
            Buffer.add_char prevail ' ';
            let i = Variable.index_of_value (Basic v) var in
            if i < 0 then valid_operator := false;
            Buffer.add_string prevail (string_of_int i);
            Buffer.add_char prevail '\n';
            n := !n + 1;
        )
    ) pre;
    Buffer.add_string buffer (string_of_int !n);
    Buffer.add_char buffer '\n';
    Buffer.add_string buffer (Buffer.contents prevail);
    (* prepost *)
    let temp = Buffer.create 10 in
    let n = ref 0 in
    MapRef.iter (fun r v ->
        let var = Variable.find r variables in
        Buffer.add_string temp "0 ";
        Buffer.add_string temp (string_of_int (Variable.index var));
        Buffer.add_char temp ' ';
        if MapRef.mem r pre then
            let pre_v = MapRef.find r pre in
            let i = Variable.index_of_value (Basic pre_v) var in
            if i < 0 then valid_operator := false;
            Buffer.add_string temp (string_of_int i);
        else
            Buffer.add_string temp "-1";
        Buffer.add_char temp ' ';
        let j = Variable.index_of_value (Basic v) var in
        if j < 0 then valid_operator := false;
        Buffer.add_string temp (string_of_int j);
        Buffer.add_char temp '\n';
        n := !n + 1;
    ) eff;
    Buffer.add_string buffer (string_of_int !n);
    Buffer.add_char buffer '\n';
    Buffer.add_string buffer (Buffer.contents temp);
    (* check operator validity *)
    if !valid_operator then (
        (* cost *)
        Buffer.add_string buffer (string_of_int (A.cost action));
        Buffer.add_string buffer "\nend_operator";
        Buffer.add_string buffer (Buffer.contents buf);
        actionCounter := !actionCounter + 1
    )
    else
        prerr_endline ("Warning: operator " ^ !^(A.name action) ^
            " is invalid")
;;

(* generate FDR of a set of actions *)
let of_actions buffer actions variables =
    let counter = ref 0 in
    let buf_actions = Buffer.create 50 in
    A.iter (fun a -> of_action buf_actions a variables counter) actions;
    Buffer.add_char buffer '\n';
    Buffer.add_string buffer (string_of_int !counter);
    Buffer.add_string buffer (Buffer.contents buf_actions);;

(* generate FDR axioms *)
let of_axioms buffer = Buffer.add_string buffer "\n0";;

(** translate Nuri task to FDR task **)
let of_nuri astInit astGoal =
    (* step 0: parse the specification and generate a store *)
    let typeEnvInit = Type.nuriSpecification astInit in
    (* perform all-passes *)
    let storeInit = Valuation.nuriSpecification astInit in
    let typeEnvGoal = Type.nuriSpecification astGoal in
    (* perform first & second passes: allow TBD value *)
    let storeGoal = Valuation.nuriSpecificationSecondPass astGoal in
    (* step 1: generate flat-stores of current and desired specifications *)
    let flatStoreInit = normalise storeInit in
    let flatStoreGoal = normalise storeGoal in
    (* step 1a: generate a type->value map *)
    let typeValues = Type.make_type_values typeEnvInit flatStoreInit
        typeEnvGoal flatStoreGoal
    in
    (* step 2: translate *)
    (* step 2.1: generate variables *)
    let variables = Variable.make_ts typeEnvInit flatStoreInit typeEnvGoal
        flatStoreGoal typeValues
    in
    (* step 2.2: global constraints *)
    let (globalConstraints, globalImplications, variables1) =
        Constraint.global_of typeEnvGoal flatStoreGoal variables
    in
    (* step 2.3 & 2.4: generate actions & compile global constraints *)
    let actions = A.ground_actions typeEnvGoal variables1 typeValues
        globalConstraints globalImplications
    in
    { variables = variables1; actions = actions; global = globalConstraints }
;;

let string_of data =
    (* step 2.5: generate FDR *)
    let buffer =
        Buffer.create (100 + (40 * (Variable.total data.variables) * 2))
    in
    of_header buffer;
    of_variables buffer data.variables;
    of_mutex buffer data.variables;
    of_init buffer data.variables;
    of_goal buffer data.variables (data.global <> True);
    of_actions buffer data.actions data.variables;
    of_axioms buffer;
    Buffer.contents buffer
;;

let actions_of data = data.actions ;;

let variables_of data = data.variables ;;

(** convert an FDR plan (in string) to a Nuri plan **)
type temporary_plan = { plan: A.t list; dummy: A.t } ;;

(**
 * @param fdr_plan     string of FDR plan
 * @param data         FDR data that contains Nuri variables and actions
 * @param removeDummy  true to remove dummy operators (by merging their
 *                     preconditions to the following action), false to
 *                     keep dummy operators
 *
 * @return a sequential plan (list of actions)
 *)
let convert_fdr_to_nuri_plan fdr_plan data removeDummy =
    (* merge 'pre1' to 'pre2' : any variable's of 'pre2' can be overridden *)
    let merge pre1 pre2 =
        let pre = MapRef.fold (fun r v pre ->
                if (List.hd r).[0] = '!' then pre
                else MapRef.add r v pre
            ) pre1 pre2
        in
        MapRef.remove Variable.r_dummy pre
    in
    let actions = A.to_array data.actions in
    let dummy = A.make [] MapStr.empty 0 MapRef.empty MapRef.empty in
    let space = Str.regexp " " in
    let fdr_actions = Str.split (Str.regexp "\n") fdr_plan in
    let accumulator = {
            plan = [];
            dummy = dummy
        }
    in
    let result =
        List.fold_left (fun acc line ->
            if (String.get line 0) = ';' then
                acc
            else
                let line = String.sub line 1 ((String.length line)-1) in
                let parts = Str.bounded_split space line 3 in
                let index = int_of_string (List.hd parts) in
                if removeDummy then
                    if (List.hd (List.tl parts)).[1] = '!' then
                        (* a dummy action: "$.!global" *)
                        if acc.dummy = dummy then
                            {
                                plan = acc.plan;
                                dummy = actions.(index)
                            }
                        else
                            (* merge the preconditions of the previous dummy
                               action with the current one *)
                            let a = actions.(index) in
                            let pre = merge (A.preconditions acc.dummy)
                                (A.preconditions a)
                            in
                            let a1 = A.make (A.name a) (A.parameters a)
                                (A.cost a) pre (A.effects a)
                            in
                            {
                                plan = acc.plan;
                                dummy = a1
                            }
                    else
                        (* not a dummy operator *)
                        if acc.dummy = dummy then
                            {
                                plan = actions.(index) :: acc.plan;
                                dummy = dummy
                            }
                        else
                            (* merge previous dummy operator with the
                               current one *)
                            let pre_dummy = A.preconditions acc.dummy in
                            let a = actions.(index) in
                            let pre = A.preconditions a in
                            let eff =
                                MapRef.remove Variable.r_dummy (A.effects a)
                            in
                            let a1 = A.make (A.name a) (A.parameters a)
                                (A.cost a) (merge pre_dummy pre) eff
                            in
                            {
                                plan = a1 :: acc.plan;
                                dummy = dummy
                            }
                else
                    {
                        plan = actions.(index) :: acc.plan;
                        dummy = acc.dummy
                    }
        ) accumulator fdr_actions
    in
    Array.of_list (List.rev result.plan)
;;

let to_nuri_plan (s: string) (dat: t) : Plan.sequential =
    convert_fdr_to_nuri_plan s dat true
;;

let to_raw_nuri_plan (s: string) (dat: t): Plan.sequential =
    convert_fdr_to_nuri_plan s dat false
;;

let of_files initialFile goalFile =
    of_nuri (Parser_helper.ast_of_file initialFile) (Parser_helper.ast_of_file goalFile)
;;
