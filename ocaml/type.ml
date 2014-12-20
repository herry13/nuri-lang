(** Module Type contains the type environment (list and map)
    as well as its algebra functions.

    Module dependencies:
    - Common
    - Syntax

    @author Herry (herry13\@gmail.com)
    @since 2014
*)

open Common
open Syntax


(*******************************************************************
 * Type environment & map
 *******************************************************************)
type system = { types : types ; environment : environment }

and types = t list

and environment = variable_type list

and variable_type = Domain.reference * t

and map = t MapRef.t


(*******************************************************************
 * Exception & helper functions
 *******************************************************************)

(** Raised when there is a type-error *)
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


(*******************************************************************
 * Type map functions
 *******************************************************************)

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
    it returns T_Undefined.
*)
let type_of variable map = if MapRef.mem variable map then MapRef.find variable map
                           else T_Undefined
;;


(** Generate a string of a type-environment.
    @param env the type-environment
    @return a string
*)
let string_of_environment ?buffer:(buf = Buffer.create 42) environment =
    List.iter (fun (variable, t) ->
        buf << !^variable;
        buf << " : ";
        buf << (string_of_type t);
        buf <. '\n'
    ) environment;
    buf
;;

let initial_environment = [] ;;

(*******************************************************************
 * Typing judgement functions
 *******************************************************************)

(** Return the type of given variable within given type-environment. If the
    variable is not exist, then return T_Undefined.
    @param variable the variable to be searched
    @param env the type-environment
    @return the type of variable
*)
let rec (@:) variable environment =
    let rec iter environment = match environment with
        | []                                -> T_Undefined
        | (var, t) :: _ when var = variable -> t
        | _ :: tail                         -> iter tail
    in
    iter environment
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

    | T_Schema t1, T_Schema t2
    | T_Object t1, T_Object t2
    | T_Reference t1, T_Reference t2                  -> t1 <:: t2

    | T_List t1, T_List t2                            -> t1 <: t2
    | _                                               -> false

and (<::) type1 type2 = match type1, type2 with
    | _, T_Plain -> true
    | T_User (id1, _), T_User (id2, _) when id1 = id2 -> true
    | T_User (_, super), _ -> super <:: type2
    | _ -> false
;;


let rec has t map = match t with
    | T_Undefined | T_Forward _ -> false

    | T_Bool | T_Int | T_Float | T_String
    | T_Null | T_Any | T_Action | T_Constraint -> true

    | T_Enum (id, _) ->
        if MapRef.mem [id] map then (MapRef.find [id] map) = t
        else false

    | T_List t_list -> has t_list map

    | T_Schema T_Plain | T_Object T_Plain | T_Reference T_Plain -> true

    | T_Schema T_User (id, _) ->
        if MapRef.mem [id] map then (MapRef.find [id] map) <: (T_Schema T_Plain)
        else false

    | T_Object t_object | T_Reference t_object -> has (T_Schema t_object) map
;;

let well_formed map = MapRef.for_all (fun _ t -> has t map) map ;;

(*******************************************************************
 * Type assignment functions
 *******************************************************************)

let assign t_explicit t_value variable environment =
    match (variable @: environment), t_explicit, t_value with
    | T_Undefined, T_Undefined, T_Any ->
        error 490 ("You need to explicitly define the type of '" ^ !^variable ^ "'.")

    | T_Undefined, T_Undefined, _ -> (variable, t_value) :: environment

    | T_Undefined, _, _ when t_value <: t_explicit -> (variable, t_explicit) :: environment

    | T_Forward _, T_Undefined, _ -> (variable, t_value) :: environment

    | T_Forward _, _, _ | T_Undefined, _, T_Forward _ ->
        (variable, t_explicit) :: (variable, t_value) :: environment

    | T_Undefined, _, _ ->
        error 406 ("The value's type is not subtype of the explicit type of '" ^ !^variable ^ "'.")

    | t_var, T_Undefined, _ when t_value <: t_var -> environment

    | t_var, T_Undefined, T_Forward _ -> (variable, t_value) :: environment

    | _, T_Undefined, _ ->
        error 407 ("The value's type is not subtype of the variable's type: " ^ !^variable ^ ".")

    | t_var, _, _ when (t_value <: t_explicit) && (t_explicit <: t_var) -> environment

    | _ when not (t_value <: t_explicit) ->
        error 408 ("The value's type is not subtype of the explicit type of '" ^ !^variable ^ "'.")

    | _ ->
        error 409 ("The explicit type is not subtype of the variable's type: " ^ !^variable ^ ".")
;;

let variables_with_prefix ?remove_prefix:(noPrefix = true) prefix environment =
    if prefix = [] then
        environment
    else if environment = [] then
        []
    else
        List.fold_left (fun env (var, t) ->
            if prefix @< var then
                let variable = if noPrefix then var @- prefix else var in
                (variable, t) :: env
            else
                env
        ) [] environment
;;

let copy srcPrefix destPrefix environment =
    List.fold_left (fun env (var, t) ->
        (destPrefix @+ var, t) :: env
    ) environment (variables_with_prefix srcPrefix environment)
;;

let rec resolve reference namespace environment =
    match namespace, namespace @<< reference with
    | [], Domain.Invalid   -> ([], T_Undefined)
    | _ , Domain.Invalid   -> resolve reference !-namespace environment
    | [], Domain.Valid ref -> ([], ref @: environment)
    | _ , Domain.Valid ref ->
        begin match ref @: environment with
        | T_Undefined -> resolve reference !-namespace environment
        | t           -> (namespace, t)
        end
;;

let _inherit srcPrefix destPrefix namespace environment =
    let validPrefix = match resolve srcPrefix namespace environment with
        | _, T_Undefined -> error 410 ("Prototype '" ^ !^srcPrefix ^ "' is not found.")
        | ns, t when t <: T_Object T_Plain ->
            begin match ns @<< srcPrefix with
            | Domain.Valid ref -> ref
            | Domain.Invalid   -> error 411 "Invalid prototype."
            end
        | _, t -> error 411 ("Prototype '" ^ !^srcPrefix ^ "' is not an object.")
    in
    copy validPrefix destPrefix environment
;;


(*******************************************************************
 * Second-pass type evaluation functions
 *******************************************************************)

let main_of ?main_reference:(main = ["main"]) environment =
    variables_with_prefix main environment
;;

let rec resolve_forward_type ?visited:(accumulator = SetRef.empty) var namespace environment =
    match namespace, namespace @<< var with
    | [], Domain.Invalid -> error 433 ("Undefined forward type of '" ^ !^var ^ "'")
    | _, Domain.Invalid -> resolve_forward_type ~visited:accumulator var !-namespace environment
    | _, Domain.Valid r ->
        if SetRef.exists (fun rx -> rx = r) accumulator then
            error 434 ("Cyclic reference is detected: " ^ !^r)
        else
            match namespace, r @: environment with
            | [], T_Undefined -> error 435 ("'" ^ !^r ^ "' is not found.")
            | _, T_Undefined -> resolve_forward_type ~visited:accumulator var !-namespace environment
            | ns, T_Forward T_ReferenceForward ref
            | ns, T_Forward T_LinkForward ref ->
                resolve_forward_type ~visited:(SetRef.add r accumulator) ref ns environment
            | _, t -> (r, t)
;;

let replace_forward_type prefix environment =
    let replace t_forward variable environment =
        let replace_link_forward link =
            match resolve_forward_type link variable environment with
            | prototype, t when t <: T_Object T_Plain ->
                copy prototype variable ((variable, t) :: environment)
            | _, t ->
                (variable, t) :: environment
        in
        let replace_reference_forward reference =
            match resolve_forward_type reference variable environment with
            | _, T_Object t -> (variable, T_Object t) :: environment
            | _, t -> (variable, t) :: environment
        in
        match t_forward with
        | T_LinkForward r      -> replace_link_forward r
        | T_ReferenceForward r -> replace_reference_forward r
    in
    List.fold_left (fun acc (var, t) ->
        if prefix @< var then
            match t with
            | T_Forward tf -> replace tf var acc
            | _ -> (var, t) :: acc
        else
            acc
    ) environment environment
;;
