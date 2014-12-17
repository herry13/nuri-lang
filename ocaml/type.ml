(** Module Type contains the type environment (list and map)
    as well as its algebra functions.

    Module dependencies:
    - Common

    @author Herry (herry13\@gmail.com)
    @since 2014
*)

open Common
open Syntax

type system = { types : types ; environment : environment }

and types = t list

and environment = variable_type list

and variable_type = Domain.reference * t

and map = t MapRef.t

exception Error of int * string

(** Raise an exception Error.
    @param code the error's code
    @param message the error's message
    @raise Error
*)
let error code message =
    let msg = if message = "" then message else (" " ^ message) in
    raise (Error (code, "[typeErr" ^ (string_of_int code) ^ "]" ^ msg))
;;


(* alias of functions from module Domain *)
let (@+)  = Domain.(@+) ;;
let (@+.) = Domain.(@+.) ;;
let (@-)  = Domain.(@-) ;;
let (!-)  = Domain.(!-) ;;
let (@<=) = Domain.(@<=) ;;
let (@<)  = Domain.(@<) ;;
let (@<<) = Domain.(@<<) ;;
let (!<<) = Domain.(!<<) ;;


(** Generate a string of a map of variable-type.
    @param map the map of variable-type
    @return a string
*)
let string_of_map ?buffer:(buf = Buffer.create 42) map =
    MapRef.iter (fun var t ->
        buf << !^var ;
        buf << " : " ;
        buf << (string_of_type t) ;
        buf <. '\n'
    ) map ;
    buf
;;

(** Return the type of a variable within given map. If the variable is not exist then
    it returns T_Undefined. *)
let type_of variable map = if MapRef.mem variable map then MapRef.find variable map
                           else T_Undefined
;;


(** Generate a string of a type-environment.
    @param env the type-environment
    @return a string
*)
let string_of_environment ?buffer:(buf = Buffer.create 42) env =
    List.iter (fun (var, t) ->
        buf << !^var ;
        buf << " : " ;
        buf << (string_of_type t) ;
        buf <. '\n'
    ) env ;
    buf
;;

let string_of_types ?buffer:(buf = Buffer.create 42) types =
    List.iter (fun t ->
        buf << (string_of_type t) ;
        buf <. '\n'
    ) types ;
    buf
;;

let string_of_system ?buffer:(buf = Buffer.create 42) sys =
    buf << "types:\n" ;
    let buf = string_of_types ~buffer:buf sys.types in
    buf << "environment:\n" ;
    let buf = string_of_environment ~buffer:buf sys.environment in
    buf
;;

let builtin_types = [
    T_Bool ;
    T_Int ;
    T_Float ;
    T_String ;
    T_Null ;
    T_Any ;
    T_Action ;
    T_Constraint ;
    T_Object T_PlainObject ;
    T_Object T_PlainSchema
] ;;

let initial_system = { types = builtin_types ; environment = [] } ;;

(*******************************************************************
 * typing judgement functions
 *******************************************************************)

(** Return the type of given variable within given type-environment. If the
    variable is not exist, then return T_Undefined.
    @param variable the variable to be searched
    @param env the type-environment
    @return the type of variable
*)
let rec (@:) variable sys =
    let rec iter env = match env with
        | []                                -> T_Undefined
        | (var, t) :: _ when var = variable -> t
        | _ :: tail                         -> iter tail
    in
    iter sys.environment
;;

(** Return true of the first (left) type is a subtype of the second (right)
    type, otherwise false.
    @param type1 the first (left) type
    @param type2 the second (right) type
    @return bool
*)
let rec (<:) type1 type2 = match type1, type2 with
    | _, _ when type1 = type2                         -> true
    | T_Any, _ when type2 <> T_Undefined              -> true
    | T_Int, T_Float                                  -> true
    | T_Enum (id1, _), T_Enum (id2, _) when id1 = id2 -> true
    | T_Null, T_Reference _                           -> true
    | T_Object t1, T_Object t2                        -> true
    | T_Reference t1, T_Reference t2                  -> t1 <:: t2
    | T_List t1, T_List t2                            -> t1 <: t2
    | _                                               -> false

and (<::) schema1 schema2 = match schema1, schema2 with
    | _, _ when schema1 = schema2                         -> true
    | T_Schema (id1, _), T_Schema (id2, _) when id1 = id2 -> true
    | T_Schema _, T_PlainObject                           -> true
    | T_Schema (_, super), _                              -> super <:: schema2
    | _                                                   -> false
;;

let rec has t sys = match t with
    | T_Undefined | T_Forward _ -> false
    | T_Reference t_obj -> has (T_Object t_obj) sys
    | T_List tl         -> has tl sys
    | T_Enum (id, _) -> List.exists (fun tx -> match tx with
            | T_Enum (idx, _) when id = idx -> true
            | _ -> false
        ) sys.types

    | T_Object T_Schema (id, _) -> List.exists (fun tx -> match tx with
            | T_Object T_Schema (idx, _) when id = idx -> true
            | _ -> false
        ) sys.types

    | _ -> List.exists (fun tx -> t = tx) sys.types
;;

let well_formed sys = List.for_all (fun (_, t) -> has t sys) sys.environment ;;


(*******************************************************************
 * type assignment functions
 *******************************************************************)

let assign t_explicit t_value var sys = match (var @: sys), t_explicit, t_value with
    | T_Undefined, T_Undefined, T_Any ->
        error 490 ("You need to explicitly define the type of '" ^ !^var ^ "'.")

    | T_Undefined, T_Undefined, _ ->
        { types = sys.types ; environment = (var, t_value) :: sys.environment }

    | T_Undefined, _, _ when t_value <: t_explicit ->
        { types = sys.types ; environment = (var, t_explicit) :: sys.environment }

    | T_Forward _, T_Undefined, _ ->
        { types = sys.types ; environment = (var, t_value) :: sys.environment }

    | T_Forward _, _, _ | T_Undefined, _, T_Forward _ -> 
        { types = sys.types ; environment = (var, t_explicit) :: (var, t_value) :: sys.environment }

    | T_Undefined, _, _ ->
        error 406 ("The value's type is not subtype of the explicit type of '" ^ !^var ^ "'.")

    | t_var, T_Undefined, _ when t_value <: t_var -> sys

    | t_var, T_Undefined, T_Forward _ ->
        { types = sys.types ; environment = (var, t_value) :: sys.environment }

    | _, T_Undefined, _ ->
        error 407 ("The value's type is not subtype of the variable's type: " ^ !^var ^ ".")

    | t_var, _, _ when (t_value <: t_explicit) && (t_explicit <: t_var) -> sys

    | _ when not (t_value <: t_explicit) ->
        error 408 ("The value's type is not subtype of the explicit type of '" ^ !^var ^ "'.")

    | _ ->
        error 409 ("The explicit type is not subtype of the variable's type: " ^ !^var ^ ".")
;;

let variables_with_prefix ?remove_prefix:(no_prefix = true) prefix env =
    if prefix = [] then
        env
    else if env = [] then
        []
    else
        List.fold_left (fun env (var, t) ->
            if prefix @< var then
                let varx = if no_prefix then var @- prefix else var in
                (varx, t) :: env
            else
                env
        ) [] env
;;

let copy srcPrefix destPrefix sys =
    let env = List.fold_left (fun env (var, t) ->
            (destPrefix @+ var, t) :: env
        ) sys.environment (variables_with_prefix srcPrefix sys.environment)
    in
    { types = sys.types ; environment = env }
;;

let rec resolve var namespace sys = match namespace, namespace @<< var with
    | [], Domain.Invalid -> ([], T_Undefined)
    | _, Domain.Invalid  -> resolve var !-namespace sys
    | [], Domain.Valid r -> ([], r @: sys)
    | _, Domain.Valid r  ->
        match r @: sys with
        | T_Undefined -> resolve var !-namespace sys
        | t           -> (namespace, t)
;;

let _inherit src dest namespace sys =
    let prototype = match resolve src namespace sys with
        | _, T_Undefined -> error 410 ("Prototype is not found: " ^ !^src)
        | ns, t when t <: T_Object T_PlainObject -> ns @+ src
        | _, t -> error 411 ("Prototype is not an object: " ^ (string_of_type t))
    in
    copy prototype dest sys
;;

let main_of ?main_reference:(main = ["main"]) sys =
    { types = sys.types ; environment = variables_with_prefix main sys.environment }
;;

(*
let normalize reference namespace = match reference, namespace with
    | "root" :: rs, _ -> (rs, [])
    | "parent" :: _, [] -> error 433 ("Invalid reference: " ^ !^reference)
    | "parent" :: rs, _ :: ns -> (rs, ns)
    | "this" :: rs, _ -> (rs, ns)
    | _ -> (rs, ns)
;;

let rec resolve_follow ?visited:(accumulator = SetRef.empty) reference namespace sys =
    let follow r ns =
        let r1 = ns @<< r in
        if SetRef.exists (fun r2 -> r2 = r1)

    let (r, ns) = normalize reference namespace in
    if SetRef.exists (fun r -> r = reference) accumulator then
        error 413 ("Cyclic reference detected: " ^ !^reference)
    else
        match resolve r ns sys with
        | _, T_Undefined ->
            error 414 ("'" ^ !^reference ^ "' is not exists within '" ^ !^namespace ^ "'")

        | nss, T_Forward T_LinkForward r | nss, T_Forward T_ReferenceForward r ->
            follow r nss

        | nss, t -> (nss @<< r, t)

let rec resolve_forward_ref_type ?visited:(accumulator=SetRef.empty) env base var =
    let follow_forward_type base refValue =
        let r = base @++ var in
        if SetRef.exists (fun rx -> rx = r) accumulator
            then error 413 ("Cyclic reference detected: " ^ !^r)
        else if r @<= refValue
            then error 412 ("Implicit cyclic reference detected: " ^ !^refValue)
        else
            resolve_forward_ref_type ~visited:(SetRef.add r accumulator) env r refValue
    in
    let (base1, var1) =
        match var with
        | "root" :: rs   -> ([], rs)
        | "parent" :: rs ->
            if base = [] then
                error 433 ("Invalid variable: " ^ !^var)
            else
                ([], (!--base) @++ rs)
        | "this" :: rs   -> ([], base @++ rs)
        | _              -> (base, var)
    in
    if SetRef.exists (fun r -> r = var1) accumulator
        then error 413 ("Cyclic reference " ^ !^var)
    else
        match resolve env base1 var1 with
        | _, NotFound -> error 414 (!^var1 ^ " is not found in " ^ !^base1)
        | base2, Type T_Forward T_ReferenceForward var2
        | base2, Type T_Forward T_LinkForward var2 -> follow_forward_type base2 var2
        | base2, Type t -> (base2 @++ var1, t)
;;
*)

(*
let replace_forward_type_in env mainReference =
    let rec replace env var t t_forward = match t_forward with
        | T_LinkForward r ->
            (
                let (proto, t_val) = resolve_forward_ref_type env var r in
                let env1 = (var, t_val) :: env in
                if t_val <: T_Object T_PlainObject then copy env1 proto var
                else env1
            )
        | T_ReferenceForward r ->
            (
                let (proto, t_val) = resolve_forward_ref_type env var r in
                let t_val = match t_val with
                    | T_Object t -> T_Reference t
                    | v -> v
                in
                let env1 = (var, t_val) :: env in
                if t_val <: T_Object T_PlainObject then copy env1 proto var
                else env1
            )
    in
    let rec iter env src =
        match src with
        | [] -> env
        | (r, t) :: tail ->
            if not (mainReference @<= r) || r = mainReference then
                iter env tail
            else
                let result =
                    match t with
                    | T_Forward t_forward -> replace env r t t_forward
                    | _                 -> (r, t) :: env
                in
                iter result tail
    in
    iter env env
;;


let replace env var t_forward =
    let replace_link_forward r = match resolve_follow env var r in
        | _, t_value when 

        let (prototype, t_value) = resolve_forward_reference env var r in
        let new_env = (var, t_value) :: env
        if t_value <: T_Object T_PlainObject then copy new_env prototype var
        else new_env
    in
    let replace_reference_forward r =

    match t_forward with
    | T_LinkForward r -> replace_link_forward r
    | T_ReferenceForward r -> replace_reference_forward r
;;

let replace_type_forwards ?main_reference:(main = ["main"]) sys =
    let rec iter dest src = match src with
        | [] -> dest
        | (var, _) :: tail when not (main @< r) -> iter dest src
        | (var, t) :: tail ->
            (
                let env = match t with
                    | T_Forward tf -> replace dest var tf
                    | _            -> (var, t) :: dest
                in
                iter env tail
            )
    in
    { types = sys.types ; environment = iter sys.environment sys.environment }
;;
*)
