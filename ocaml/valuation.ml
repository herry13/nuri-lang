(* Author: Herry (herry13@gmail.com) *)

open Common
open Syntax

let referenceGlobal = ["global"] ;;

let sfBoolean b =
    if b = "true" then Domain.Boolean true
    else Domain.Boolean false
;;

let sfInt i = Domain.Int (int_of_string i) ;;

let sfFloat f = Domain.Float (float_of_string f) ;;

let sfString s = Domain.String s ;;

let sfNull = Domain.Null ;;

let sfReference r = r ;;

let sfDataReference dataRef = Domain.Reference (sfReference dataRef) ;;

let sfLinkReference link =
    let linkRef = sfReference link in
    fun r ->
        if Domain.(@<=) linkRef r then Domain.error 1101 ""
        else Domain.Link linkRef
;;

let rec sfVector vector =
    let rec iter vec =
        match vec with
        | []           -> []
        | head :: tail -> (sfBasicValue head) :: (iter tail)
    in
    Domain.Vector (iter vector)

and sfBasicValue value =
    match value with
    | Boolean b   -> sfBoolean b
    | Int i       -> sfInt i
    | Float f     -> sfFloat f
    | String s    -> sfString s
    | Null        -> sfNull
    | Vector vec  -> sfVector vec
    | Reference r -> sfDataReference r

let rec sfPrototype prototypes ns r s =
    match prototypes with
    | BlockPrototype (pb, p) -> sfPrototype p ns r (sfBlock pb r s)
    | ReferencePrototype (pr, p) ->
        sfPrototype p ns r (Domain.inherit_proto s ns (sfReference pr) r)
    | EmptyPrototype -> s

(* TODO: documentation *)
and nuriShell command s ns =
    let delim = Str.regexp "\\." in
    let len = String.length command in
    let bufferCommand = Buffer.create len in
    let bufferVariable = Buffer.create 15 in
    let substitute_variable (var : string) : unit =
        let value : Domain._value = match Domain.resolve s ns (Str.split delim var) with
            | _, Domain.Val Domain.Lazy func -> Domain.Val (Domain.eval_function s ns func)
            | _, v -> v
        in
        match value with
        | Domain.Val Domain.Basic v -> bufferCommand << (Domain.string_of_basic_value v)
        | Domain.Undefined -> Domain.error 1102 ("Cannot find '" ^ var ^ "' in `" ^ command ^ "`")
        | _ -> Domain.error 1103 ("Type of '" ^ var ^ "' in `" ^ command ^ "` is indeterminate.")
    in
    let rec string_interpolation index length state : string =
        if index >= length then (
            if state = 0 then
                Buffer.contents bufferCommand
            else
                Domain.error 1104 ("Invalid command `" ^ command ^ "`")
        ) else (
            if state = 0 then (
                if command.[index] = '$' then
                    string_interpolation (index + 1) length 1
                else (
                    bufferCommand <. command.[index];
                    string_interpolation (index + 1) length 0
                )
            ) else if state = 1 then (
                if command.[index] = '{' then (
                    string_interpolation (index + 1) length 2
                ) else (
                    bufferCommand <. '$';
                    bufferCommand <. command.[index];
                    string_interpolation (index + 1) length 0
                )
            ) else if state = 2 then (
                if command.[index] = '}' then (
                    substitute_variable (Buffer.contents bufferVariable);
                    Buffer.clear bufferVariable;
                    string_interpolation (index + 1) length 0
                ) else (
                    bufferVariable <. command.[index];
                    string_interpolation (index + 1) length 2
                )
            ) else (
                Domain.error 1105 ("Invalid command `" ^ command ^ "`")
            )
        )
    in
    let command = string_interpolation 0 len 0 in
    try
        Domain.Basic (Domain.String (get_process_output command))
    with
        Failure message -> Domain.error 1106 message

(* TODO: documentation *)
and nuriEqual exp1 exp2 s ns =
    Domain.equals ~store:s ~namespace:ns (eval exp1 ns s) (eval exp2 ns s)

and nuriExp_And exp1 exp2 s ns =
    Domain.logic ~operator:"&&" ~store:s ~namespace:ns (&&) (eval exp1 ns s) (eval exp2 ns s)

and nuriExp_Or exp1 exp2 s ns =
    Domain.logic ~operator:"||" ~store:s ~namespace:ns (||) (eval exp1 ns s) (eval exp2 ns s)

and nuriExp_Imply exp1 exp2 s ns =
    Domain.unary ~store:s ~namespace:ns (eval exp1 ns s) (fun v1 -> match v1 with
        | Domain.Basic Domain.Boolean b1 ->
            if b1 then (
                Domain.unary ~store:s ~namespace:ns (eval exp2 ns s) (fun v2 -> match v2 with
                    | Domain.Basic Domain.Boolean b2 -> Domain.Basic (Domain.Boolean b2)
                    | _ -> Domain.error 1107 "Right operand of '=>' is not a boolean."
                )
            ) else (
                Domain.Basic (Domain.Boolean true)
            )
        | _ -> Domain.error 1108 "Left operand of '=>' is not a boolean."
    )

(* TODO: documentation *)
and nuriExp_Not exp s ns =
    Domain.unary ~store:s ~namespace:ns (eval exp ns s) (fun v -> match v with
        | Domain.Basic Domain.Boolean b -> Domain.Basic (Domain.Boolean (not b))
        | _ -> Domain.error 1107 "Operand of '!' is not a boolean."
    )

(* TODO: documentation *)
and nuriExp_Add exp1 exp2 s ns =
    Domain.add ~store:s ~namespace:ns (eval exp1 ns s) (eval exp2 ns s)

and nuriExp_Subtract exp1 exp2 s ns =
    Domain.math ~store:s ~namespace:ns (-) (-.) (eval exp1 ns s) (eval exp2 ns s)

and nuriExp_Multiply exp1 exp2 s ns =
    Domain.math ~store:s ~namespace:ns ( * ) ( *. ) (eval exp1 ns s) (eval exp2 ns s)

and nuriExp_Divide exp1 exp2 s ns =
    Domain.math ~store:s ~namespace:ns ( / ) ( /. ) (eval exp1 ns s) (eval exp2 ns s)

and nuriExp_Modulo exp1 exp2 s ns =
    Domain.math ~store:s ~namespace:ns
               (mod)
               (fun _ _ -> Domain.error 1108 "Both modulo's operands must be integers.")
               (eval exp1 ns s) (eval exp2 ns s)

(* TODO: documentation *)
and nuriExp_IfThenElse ifExp thenExp elseExp s ns =
    Domain.unary ~store:s ~namespace:ns (eval ifExp ns s) (fun v -> match v with
        | Domain.Basic Domain.Boolean b -> if b then (eval thenExp ns s) else (eval elseExp ns s)
        | _ -> Domain.error 1108 "if-clause is not a boolean."
    )

(* TODO: documentation *)
and nuriExp_MatchRegexp exp regexp s ns =
    Domain.unary ~store:s ~namespace:ns (eval exp ns s) (fun v ->
        Domain.Basic (Domain.Boolean (match v with
            | Domain.Basic Domain.String str -> (
                    try (Str.search_forward (Str.regexp regexp) str 0) >= 0
                    with Not_found -> false
                )
            | _ -> false
        ))
    )

(* helper function -- TODO: documentation *)        
and eval (exp : Syntax.expression) ns s : Domain.value =
    match nuriExpression exp ns s  with
    | Domain.Lazy func -> Domain.eval_function s ns func
    | Domain.Basic Domain.Reference r ->
        (
            match (Domain.resolve ~follow:true s ns r) with
            | _, Domain.Undefined
            | _, Domain.Val Domain.Store _ -> Domain.Basic (Domain.Reference r)
            | _, Domain.Val v -> v
        )
    | value -> value

(* TODO: documentation *)
and nuriExpression exp ns s = match exp with
    | Basic value             -> Domain.Basic (sfBasicValue value)
    | Shell command           -> Domain.Lazy (nuriShell command)
    | Exp_Eager exp           -> eval exp ns s
    | Exp_Not exp             -> Domain.Lazy (nuriExp_Not exp)
    | Exp_Equal (exp1, exp2)  -> Domain.Lazy (nuriEqual exp1 exp2)
    | Exp_And (left, right)   -> Domain.Lazy (nuriExp_And left right)
    | Exp_Or (left, right)    -> Domain.Lazy (nuriExp_Or left right)
    | Exp_Imply (left, right) -> Domain.Lazy (nuriExp_Imply left right)
    | Exp_Add (exp1, exp2)    -> Domain.Lazy (nuriExp_Add exp1 exp2)  (* Lazy evaluation  *)
                                 (* nuriExp_Add exp1 exp2 s ns *)     (* Eager evaluation *)
    | Exp_Subtract (exp1, exp2) -> Domain.Lazy (nuriExp_Subtract exp1 exp2)
    | Exp_Multiply (exp1, exp2) -> Domain.Lazy (nuriExp_Multiply exp1 exp2)
    | Exp_Divide (exp1, exp2)   -> Domain.Lazy (nuriExp_Divide exp1 exp2)
    | Exp_Modulo (exp1, exp2)   -> Domain.Lazy (nuriExp_Modulo exp1 exp2)
    | Exp_MatchRegexp (exp, regexp)     -> Domain.Lazy (nuriExp_MatchRegexp exp regexp)
    | Exp_IfThenElse (exp1, exp2, exp3) -> Domain.Lazy (nuriExp_IfThenElse exp1 exp2 exp3)

and sfValue v ns r s =
    let eval_name (r: Domain.reference) (s: Domain.store) =
        let r_name = Domain.(@+.) r "name" in
        let rec get_name r = match r with
            | [] -> "root"
            | id :: [] -> id
            | _ :: rs -> get_name rs
        in
        match Domain.find s r_name with
        | Domain.Undefined
        | Domain.Val Domain.TBD -> Domain.bind s r_name (Domain.Basic (Domain.String (get_name r)))
        | _ -> s
    in
    match v with
    | Expression exp -> Domain.bind s r (nuriExpression exp ns s)
    | Link link      -> Domain.bind s r (sfLinkReference link r)
    | Prototype (EmptySchema, p) ->
        eval_name r (sfPrototype p ns r (Domain.bind s r (Domain.Store [])))
    | Prototype (SID sid, p) ->
        (
            let s1 = Domain.bind s r (Domain.Store []) in
            let s2 = Domain.inherit_proto s1 [] [sid] r in
            eval_name r (sfPrototype p ns r s2)
        )
    | Action a -> nuriAction a ns r s
    | TBD      -> Domain.bind s r Domain.TBD
    | Unknown  -> Domain.bind s r Domain.Unknown
    | None     -> Domain.bind s r Domain.None

(** the type is ignored since this function only evaluates the value **)
and sfAssignment (reference, _, value) ns s =
    if reference = _echo_ then (
        let s1 = sfValue value ns _echo_ s in
        let v1 = match Domain.find s1 _echo_ with
            | Domain.Val Domain.Basic Domain.Reference r
            | Domain.Val Domain.Link r ->
                (
                    match (Domain.resolve ~follow:true s1 ns r) with
                    | _, Domain.Undefined
                    | _, Domain.Val Domain.Store _ -> Domain.Basic (Domain.Reference r)
                    | _, Domain.Val v -> v
                )
            | Domain.Val Domain.Lazy func -> Domain.eval_function s1 ns func
            | Domain.Val v -> v
            | _ -> Domain.Unknown
        in
        print_endline (Json.of_value ~ignore_lazy:false v1);
        s
    ) else (
        sfValue value ns (Domain.(@++) ns reference) s
    )

and sfBlock block ns s =
    match block with
    | AssignmentBlock (a, b) -> sfBlock b ns (sfAssignment a ns s)
    | TrajectoryBlock (t, b) -> sfBlock b ns (nuriTrajectory t s)
    | EmptyBlock             -> s

and nuriSchema (name, parent, b) s =
    let refName = [name] in
    let s1 = Domain.bind s refName (Domain.Store []) in
    let s2 =
        match parent with
        | EmptySchema -> s1
        | SID superid -> Domain.inherit_proto s1 [] [superid] refName
    in
    sfBlock b refName s2

and nuriEnum (name, elements) s =
    let refName = [name] in
    Domain.bind s refName (Domain.Enum elements)

and nuriTrajectory t s = match t with
    | Global g -> nuriGlobal g s

and nuriContext context s =
    match context with
    | AssignmentContext (assignment, nextContext) ->
        nuriContext nextContext (sfAssignment assignment [] s)
    | SchemaContext (schema, nextContext) ->
        nuriContext nextContext (nuriSchema schema s)
    | EnumContext (enum, nextContext) ->
        nuriContext nextContext (nuriEnum enum s)
    | TrajectoryContext (trajectory, nextContext) ->
        nuriContext nextContext (nuriTrajectory trajectory s)
    | EmptyContext -> s

and nuriSpecificationFirstPass nuri = nuriContext nuri []

and nuriSpecificationSecondPass ?main:(referenceMain=["main"]) nuri =
    let s1 = nuriSpecificationFirstPass nuri in
    let s2 =
        match Domain.find s1 referenceMain with
        | Domain.Val (Domain.Store main1) ->
            Domain.accept s1 referenceMain main1 referenceMain
        | _ -> Domain.error 1102 ""
    in
    let add_global s =
        match Domain.find s1 referenceGlobal with
        | Domain.Undefined -> s
        | Domain.Val (Domain.Global global) ->
            Domain.bind s referenceGlobal (Domain.Global global)
        | _ -> Domain.error 1103 "" 
    in
    match Domain.find s2 referenceMain with
    | Domain.Val (Domain.Store main2) -> add_global main2
    | _ -> Domain.error 1104 ""

and nuriSpecificationThirdPass ?main:(referenceMain=["main"]) nuri =
    let s2 = nuriSpecificationSecondPass ~main:referenceMain nuri in
    match Domain.find_value [] s2 Domain.TBD with
    | []  -> s2
    | ref -> Domain.error 1105 (!^ref ^ " = TBD")

and nuriSpecification ?main:(referenceMain=["main"]) nuri =
    nuriSpecificationThirdPass ~main:referenceMain nuri


(** global constraints **)
and nuriGlobal g s =
    let r = ["global"] in
    let gc = nuriConstraint g in
    match Domain.find s r with
    | Domain.Val (Domain.Global gs) ->
        let f = Domain.Global (Domain.And [gc; gs]) in
        Domain.bind s r f
    | Domain.Undefined -> Domain.bind s r (Domain.Global gc)
    | _                  -> Domain.error 1106 ""

(** constraints **)
and nuriConstraint (c : _constraint) =
    let nuriMembership r vec =
        let rec eval v =
            match v with
            | [] -> []
            | head :: tail -> (sfBasicValue head) :: (eval tail)
        in
        Domain.In (sfReference r, eval vec)
    in
    match c with
    | C_Equal (r, v)        -> Domain.Equal ((sfReference r), (sfBasicValue v))
    | C_NotEqual (r, v)     -> Domain.NotEqual ((sfReference r), (sfBasicValue v))
    | C_Greater (r, v)      -> Domain.Greater ((sfReference r), (sfBasicValue v))
    | C_GreaterEqual (r, v) -> Domain.GreaterEqual ((sfReference r),
                                 (sfBasicValue v))
    | C_Less (r, v)         -> Domain.Less ((sfReference r), (sfBasicValue v))
    | C_LessEqual (r, v)    -> Domain.LessEqual ((sfReference r),
                                 (sfBasicValue v))
    | C_Not c1              -> Domain.Not (nuriConstraint c1)
    | C_Imply (c1, c2)      -> Domain.Imply (nuriConstraint c1, nuriConstraint c2)
    | C_In (r, vec)         -> nuriMembership r vec
    | C_And cs              ->
        Domain.And (List.fold_left (
            fun acc c -> (nuriConstraint c) :: acc
        ) [] cs)
    | C_Or cs               ->
        Domain.Or (List.fold_left (
            fun acc c -> (nuriConstraint c) :: acc
        ) [] cs)

(* action *)
and nuriAction (parameters, cost, conditions, effects) =
    let get_parameters = parameters in
    let get_cost =
        match cost with
        | Cost cs   -> int_of_string cs
        | EmptyCost -> 1
    in
    let get_conditions =
        match conditions with
        | EmptyCondition -> Domain.True
        | Condition c    -> nuriConstraint c
    in
    let get_effects = 
        List.fold_left (
            fun acc (r, bv) -> (r, sfBasicValue bv) :: acc
        ) [] effects
    in
    fun ns r s ->
        let action =
            (r, get_parameters, get_cost, get_conditions, get_effects)
        in
        Domain.bind s r (Domain.Action action)
