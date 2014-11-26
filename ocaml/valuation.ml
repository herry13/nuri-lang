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

let sfDataReference dataRef = Domain.Ref (sfReference dataRef) ;;

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

let rec sfPrototype prototypes =
    fun ns r s ->
        match prototypes with
        | BlockPrototype (pb, p) -> sfPrototype p ns r (sfBlock pb r s)
        | ReferencePrototype (pr, p) ->
            sfPrototype p ns r (Domain.inherit_proto s ns (sfReference pr) r)
        | EmptyPrototype -> s

(* TODO: documentation *)
and nuriShell command =
    fun s ns ->
        let len = String.length command in
        let bufferCommand = Buffer.create len in
        let bufferVariable = Buffer.create 15 in
        let rec string_interpolation index length state : string =
            if index >= length then (
                if state = 0 then
                    Buffer.contents bufferCommand
                else
                    Domain.error 1108 ("Invalid command `" ^ command ^ "`")
            ) else (
                if state = 0 then (
                    if command.[index] = '$' then
                        string_interpolation (index + 1) length 1
                    else (
                        Buffer.add_char bufferCommand command.[index];
                        string_interpolation (index + 1) length 0
                    )
                ) else if state = 1 then (
                    if command.[index] = '{' then
                        string_interpolation (index + 1) length 2
                    else
                        string_interpolation (index + 1) length 0
                ) else if state = 2 then (
                    if command.[index] = '}' then (
                        let var = Buffer.contents bufferVariable in
                        (
                            match Domain.resolve s ns (Str.split (Str.regexp "\\.") var) with
                            | _, Domain.Val Domain.Basic value ->
                                Buffer.add_string bufferCommand (Domain.string_of_basic_value value)
                            | _, Domain.Undefined ->
                                Domain.error 1109 ("Variable '" ^ var ^ "' in `" ^ command ^ "` is not found.")
                            | _ ->
                                Domain.error 1110 ("Variable '" ^ var ^ "' in `" ^ command ^ "` is not a string.")
                        );
                        Buffer.clear bufferVariable;
                        string_interpolation (index + 1) length 0
                    ) else (
                        Buffer.add_char bufferVariable command.[index];
                        string_interpolation (index + 1) length 2
                    )
                ) else (
                    Domain.error 1110 ("Invalid command `" ^ command ^ "`")
                )
            )
        in
        let command = string_interpolation 0 len 0 in
        try
            Domain.Basic (Domain.String (get_process_output command))
        with
            Failure message -> Domain.error 1107 message

(* TODO: documentation *)
and nuriExpression exp =
    let eval exp ns s =
        match nuriExpression exp ns s  with
        | Domain.Lazy func -> Domain.resolve_function s ns func
        | value -> value
    in
    fun ns s -> match exp with
        | Basic value -> Domain.Basic (sfBasicValue value)
        | Shell command -> Domain.Lazy (nuriShell command)
        | Equal (exp1, exp2) ->
            Domain.Lazy (fun s ns ->
                let value = match (eval exp1 ns s), (eval exp2 ns s) with
                    | Domain.Basic (Domain.Int v1), Domain.Basic (Domain.Float v2) -> (float_of_int v1) = v2
                    | Domain.Basic (Domain.Float v1), Domain.Basic (Domain.Int v2) -> v1 = (float_of_int v2)
                    | Domain.Basic (Domain.Ref r1), Domain.Basic (Domain.Ref r2) ->
                        (
                            match (Domain.resolve ~follow_ref:true s ns r1),
                                  (Domain.resolve ~follow_ref:true s ns r2)
                            with
                            | (_, Domain.Val v1), (_, Domain.Val v2) -> v1 = v2
                            | _ -> false
                        )
                    | Domain.Basic (Domain.Ref r), v1
                    | v1, Domain.Basic (Domain.Ref r) ->
                        (
                            match Domain.resolve ~follow_ref:true s ns r with
                            | _, Domain.Val v2 -> v1 = v2
                            | _ -> false
                        )
                    | v1, v2 -> v1 = v2
                in
                Domain.Basic (Domain.Boolean value)
            )
        | Exp_Not exp ->
            Domain.Lazy (fun s ns ->
                match eval exp ns s with
                | Domain.Basic (Domain.Boolean b) -> Domain.Basic (Domain.Boolean (not b))
                | Domain.Basic (Domain.Ref r) ->
                    (
                        match Domain.resolve ~follow_ref:true s ns r with
                        | _, Domain.Val (Domain.Basic (Domain.Boolean b)) ->
                            Domain.Basic (Domain.Boolean (not b))
                        | _ ->
                            Domain.error 1111 "The operand of 'not' is not a boolean."
                    )
                | _ -> Domain.error 1112 "The operand of 'not' is not a boolean."
            )
        | Add (exp1, exp2) ->
            Domain.Lazy (fun s ns ->
                match (eval exp1 ns s),
                      (eval exp2 ns s)
                with
                | Domain.Basic v1, Domain.Basic v2 -> Domain.Basic (Domain.add ~store:s ~namespace:ns v1 v2)
                | _, Domain.Basic _ -> Domain.error 1113 "Left operand is not a basic value."
                | Domain.Basic _, _ -> Domain.error 1114 "Right operand is not a basic value."
                | _, _ -> Domain.error 1115 "Both operands are not basic values."
            )
        | IfThenElse (exp1, exp2, exp3) ->
            Domain.Lazy (fun s ns ->
                match (eval exp1 ns s),
                      (eval exp2 ns s),
                      (eval exp3 ns s)
                with
                | Domain.Basic (Domain.Boolean condition), thenValue, elseValue ->
                    if condition then thenValue else elseValue
                | _ -> Domain.error 1116 "Invalid if-then-else statement."
            )
        | MatchRegexp (exp, regexp) ->
            Domain.Lazy (fun s ns ->
                let match_regexp str =
                     let value =
                        try
                            (Str.search_forward (Str.regexp regexp) str 0) >= 0
                        with
                            Not_found -> false
                    in
                    Domain.Basic (Domain.Boolean value)
                in
                match eval exp ns s with
                | Domain.Basic (Domain.String str) -> match_regexp str
                | Domain.Basic (Domain.Ref r) ->
                    (
                        match Domain.resolve ~follow_ref:true s ns r with
                        | _, Domain.Val Domain.Basic Domain.String str -> match_regexp str
                        | _ -> Domain.error 1117 "The operand of match-regexp is not a string."
                        
                    )
                | _ -> Domain.error 1119 "The operand of match-regexp is not a string."
            )

and sfValue v =
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
    fun ns r s ->
        match v with
        | Expression exp -> Domain.bind s r (nuriExpression exp ns s)
        | Link link   -> Domain.bind s r (sfLinkReference link r)
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
        | Nothing  -> Domain.bind s r Domain.Nothing

(** the type is ignored since this function only evaluates the value **)
and sfAssignment (reference, _, value) =
    fun ns s -> sfValue value ns (Domain.(@++) ns reference) s

and sfBlock block =
    fun ns s ->
        match block with
        | AssignmentBlock (a, b) -> sfBlock b ns (sfAssignment a ns s)
        | TrajectoryBlock (t, b) -> sfBlock b ns (nuriTrajectory t s)
        | EmptyBlock             -> s

and nuriSchema (name, parent, b) =
    fun s ->
        let refName = [name] in
        let s1 = Domain.bind s refName (Domain.Store []) in
        let s2 =
            match parent with
            | EmptySchema -> s1
            | SID superid -> Domain.inherit_proto s1 [] [superid] refName
        in
        sfBlock b refName s2

and nuriEnum (name, elements) =
    fun s ->
        let refName = [name] in
        Domain.bind s refName (Domain.Enum elements)

and nuriTrajectory t = match t with
    | Global g -> nuriGlobal g

and nuriContext context =
    fun s ->
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
    match Domain.value_TBD_exists [] s2 with
    | []  -> s2
    | ref -> Domain.error 1105 ((Domain.(!^) ref) ^ " = TBD")

and nuriSpecification ?main:(referenceMain=["main"]) nuri =
    nuriSpecificationThirdPass ~main:referenceMain nuri


(** global constraints **)
and nuriGlobal g =
    fun s ->
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
    | Eq (r, v)           -> Domain.Eq ((sfReference r), (sfBasicValue v))
    | Ne (r, v)           -> Domain.Ne ((sfReference r), (sfBasicValue v))
    | Greater (r, v)      -> Domain.Greater ((sfReference r), (sfBasicValue v))
    | GreaterEqual (r, v) -> Domain.GreaterEqual ((sfReference r),
                                 (sfBasicValue v))
    | Less (r, v)         -> Domain.Less ((sfReference r), (sfBasicValue v))
    | LessEqual (r, v)    -> Domain.LessEqual ((sfReference r),
                                 (sfBasicValue v))
    | Not c1              -> Domain.Not (nuriConstraint c1)
    | Imply (c1, c2)      -> Domain.Imply (nuriConstraint c1, nuriConstraint c2)
    | In (r, vec)         -> nuriMembership r vec
    | And cs              ->
        Domain.And (List.fold_left (
            fun acc c -> (nuriConstraint c) :: acc
        ) [] cs)
    | Or cs               ->
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
