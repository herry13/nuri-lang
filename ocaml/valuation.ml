(* Author: Herry (herry13@gmail.com) *)

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
        | Basic value -> Domain.bind s r (Domain.Basic (sfBasicValue value))
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
        | GlobalBlock (g, b)     -> sfBlock b ns (nuriGlobal g s)
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

and nuriContext context =
    fun s ->
        match context with
        | AssignmentContext (assignment, nextContext) ->
            nuriContext nextContext (sfAssignment assignment [] s)
        | SchemaContext (schema, nextContext) ->
            nuriContext nextContext (nuriSchema schema s)
        | EnumContext (enum, nextContext) ->
            nuriContext nextContext (nuriEnum enum s)
        | GlobalContext (global, nextContext) ->
            nuriContext nextContext (nuriGlobal global s)
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
