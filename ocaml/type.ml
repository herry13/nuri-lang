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

and variable_type = variable * t

and variable = string list

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
let (@++)  = Domain.(@++) ;;
let (@+.)  = Domain.(@+.) ;;
let (@--)  = Domain.(@--) ;;
let (!--)  = Domain.(!--) ;;
let (@<=)  = Domain.(@<=) ;;
let (@<)   = Domain.(@<) ;;
let (@<<)  = Domain.(@<<) ;;
let (!<<)  = Domain.(!<<) ;;


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

(** Return true if given variable exists in given type-environment, otherwise false.
    @param variable the variable to be checked its existance
    @param env the type-environment
    @return bool
*)
let (@:) variable sys =
    let rec iter env = match env with
        | []                                -> false
        | (var, _) :: _ when var = variable -> true
        | _ :: tail                         -> iter tail
    in
    iter sys.environment
;;

(** Return the type of given variable within given type-environment. If the
    variable is not exist, then return T_Undefined.
    @param variable the variable to be searched
    @param env the type-environment
    @return the type of variable
*)
let rec (-:) variable sys =
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
    | T_Undefined
    | T_Forward _       -> false
    | T_Reference t_obj -> has (T_Object t_obj) sys
    | T_List tl         -> has tl sys
    | T_Enum (id, _) ->
        (
            List.exists (fun tx -> match tx with
                | T_Enum (idx, _) when id = idx -> true
                | _ -> false
            ) sys.types
        )
    | T_Object T_Schema (id, _) ->
        (
            List.exists (fun tx -> match tx with
                | T_Object T_Schema (idx, _) when id = idx -> true
                | _ -> false
            ) sys.types
        )
    | _ -> List.exists (fun tx -> t = tx) sys.types
;;

let well_formed sys = List.for_all (fun (_, t) -> has t sys) sys.environment ;;


(*******************************************************************
 * type assignment functions
 *******************************************************************)

let assign t_explicit t_value var sys = match (var -: sys), t_explicit, t_value with
    | T_Undefined, T_Undefined, T_Any ->
        error 490 ("You need to explicitly define the type of '" ^ !^var ^ "'.")

    | T_Undefined, T_Undefined, _ ->
        { types = sys.types ; environment = (var, t_value) :: sys.environment }

    | T_Undefined, _, _ when t_value <: t_explicit ->
        { types = sys.types ; environment = (var, t_explicit) :: sys.environment }

    | T_Forward _, T_Undefined, _ ->
        { types = sys.types ; environment = (var, t_value) :: sys.environment }

    | T_Forward _, _, _
    | T_Undefined, _, T_Forward _ -> 
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
