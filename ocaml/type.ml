(** Module Type contains the type environment (list and map)
    as well as its algebra functions.

    Module dependencies:
    - Common
    - Syntax

    TODO:
    - Type-checking over action's preconditions and effects
      (this could be done after 2nd-pass)

    @author Herry (herry13\@gmail.com)
    @since 2014
*)

open Common
open Syntax


(*******************************************************************
 * Type environment & map
 *******************************************************************)

type environment = t list MapRef.t

and variable_type = Domain.reference * t

and map = t MapRef.t

(** Generate a string of a map of variable-type.
    @param map the map of variable-type
    @return a string
*)
let string_of_map map =
  let foreach_pair var t buffer =
    buffer <<| !^var <<| " : " <<| (string_of_type t) <.| '\n'
  in
  Buffer.contents (MapRef.fold foreach_pair map (Buffer.create 42))
;;

(** Generate a string of a type-environment.
    @param env the type-environment
    @return a string
*)
let string_of_environment environment =
  let foreach_pair var ts buffer =
    (join " | " string_of_type ts (buffer <<| !^var <<| " : ")) <.| '\n'
  in
  Buffer.contents (MapRef.fold foreach_pair environment (Buffer.create 42))
;;

let empty = MapRef.empty ;;

let map_of env =
  MapRef.fold (fun var ts -> MapRef.add var (List.hd ts)) env MapRef.empty
;;

(** Raised when there is a type-error *)
exception Error of int * string

(** Raise an exception Error.
    @param code the error's code
    @param message the error's message
    @raise Error
*)
let error ?env:(env = empty) ?map:(map = MapRef.empty) code message =
  if !verbose then begin
    print_endline "---- Type Environment ----";
    print_endline (string_of_environment env);
    print_endline "\n---- Type Map ----";
    print_endline (string_of_map map)
  end;
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


(** Return the type of a variable within given map. If the variable is not
    exist then it returns T_Undefined.
*)
let type_of variable map =
  if MapRef.mem variable map then MapRef.find variable map
  else T_Undefined
;;

(*******************************************************************
 * Typing judgement functions
 *******************************************************************)

(** Return the type of given variable within given type-environment. If the
    variable is not exist, then return T_Undefined.
    @param variable the variable to be searched
    @param env the type-environment
    @return the type of variable
*)
let rec find variable environment = match variable with
  | [] -> T_Object T_Plain
  | _ when MapRef.mem variable environment ->
    List.hd (MapRef.find variable environment)

  | _ -> T_Undefined
;;

let (@:) = find ;;

(** Return true of the first (left) type is a subtype of the second (right)
    type, otherwise false.
    @param type1 the first (left) type
    @param type2 the second (right) type
    @return bool
*)
let rec (<:) type1 type2 = match type1, type2 with
  | T_Undefined, _ | _, T_Undefined           -> false
  | _, _ when type1 = type2                   -> true
  | T_Any, _ when type2 <> T_Undefined        -> true
  | T_Int, T_Float                            -> true
  | T_Symbol id1, T_Symbol id2 when id1 = id2 -> true
  | T_Null, T_Reference _                     -> true

  | T_Schema t1, T_Schema t2 | T_Object t1, T_Object t2
  | T_Reference t1, T_Reference t2            -> t1 <:: t2

  | T_List t1, T_List t2                      -> t1 <: t2
  | _                                         -> false

and (<::) type1 type2 = match type1, type2 with
  | _, T_Plain                                      -> true
  | T_User (id1, _), T_User (id2, _) when id1 = id2 -> true
  | T_User (_, super), _                            -> super <:: type2
  | _                                               -> false
;;

let subtype = (<:) ;;

let (=:=) t1 t2 = t1 <: t2 && t2 <: t1 ;;

let rec has t map = match t with
  | T_Undefined | T_Forward _ -> false

  | T_Bool | T_Int | T_Float | T_String
  | T_Null | T_Any | T_Action | T_Constraint -> true

  | T_Symbol id -> has (T_Enum (id, [])) map

  | T_Enum (id, _) ->
    if MapRef.mem [id] map then
      begin match MapRef.find [id] map with
      | T_Enum (idx, _) -> id = idx
      | _ -> false
      end
    else false

  | T_List t_list -> has t_list map

  | T_Schema T_Plain | T_Object T_Plain | T_Reference T_Plain -> true

  | T_Schema T_User (id, _) ->
    if MapRef.mem [id] map then
      (MapRef.find [id] map) <: (T_Schema T_Plain)
    else
      false

  | T_Object t_object | T_Reference t_object -> has (T_Schema t_object) map
;;

let well_formed completeTypeMap mainMap =
  let _ =
    MapRef.for_all (fun var t -> match has t completeTypeMap with
      | true -> true
      | false ->
        error ~map:completeTypeMap
              490
              ("Not well-formed: type " ^ (string_of_type t) ^ " (" ^ !^var ^
                ") is not resolved.")
    ) mainMap
  in
  mainMap
;;

let symbol_of_enum symbol enumID environment =
  match find [enumID] environment with
  | T_Enum (id, symbols) when enumID = id -> List.exists ((=) symbol) symbols
  | _ -> false
;;

let bind_type environment variable t_var t_explicit t_value =
  match t_var, t_explicit, t_value with
  | T_Undefined, T_Undefined, T_Any ->
    error ~env:environment
          402
          ("You need to explicitly define the type of '" ^ !^variable ^ "'.")

  | T_Undefined, T_Undefined, _ -> MapRef.add variable [t_value] environment

  | T_Undefined, _, _ when t_value <: t_explicit ->
    MapRef.add variable [t_explicit] environment

  | T_Forward _, T_Undefined, _ ->
    MapRef.add variable
               (t_value :: (MapRef.find variable environment))
               environment

  | T_Forward _, T_Forward _, _ ->
    MapRef.add variable
               (t_value :: t_explicit :: (MapRef.find variable environment))
               environment

  | T_Forward _, _, _ | T_Undefined, _, T_Forward _ ->
    MapRef.add variable
               (t_explicit :: t_value :: (MapRef.find variable environment))
               environment

  | T_Undefined, _, _ ->
    error ~env:environment
          403
          ((string_of_type t_value) ^ " (value) is not a subtype of " ^
            (string_of_type t_explicit) ^ " (explicit) -- " ^ !^variable ^ ".")

  | T_Schema _, _, _ ->
    error ~env:environment 404 "Re-defining a schema is not allowed."

  | T_Enum _, _, _ ->
    error ~env:environment 405 "Re-defining an enum is not allowed."

  | t_variable, T_Undefined, _ when t_value <: t_variable -> environment

  | t_variable, T_Undefined, T_Forward _ ->
    MapRef.add variable
               (List.append (MapRef.find variable environment) [t_value])
               environment

  | _, T_Undefined, _ ->
    error ~env:environment
          406
          ("The value's type is not subtype of the variable's " ^ "type: " ^
            !^variable ^ ".")

  | t_variable, _, _
    when (t_value <: t_explicit) && (t_explicit <: t_variable) -> environment

  | _ when not (t_value <: t_explicit) ->
    error ~env:environment
          407
          ((string_of_type t_value) ^ " (value) is not a subtype of " ^
            (string_of_type t_explicit) ^ " (explicit) at variable '" ^
            !^variable ^ "'.")

  | _ -> error ~env:environment
               408
               ("The explicit type is not subtype of the variable's type: " ^
                 !^variable ^ ".")
;;

(** Type assignment functions *)
let bind environment variable ?t_variable:(t_variable = T_Undefined) =
  match (find !-variable environment), t_variable with
  | T_Object _, T_Undefined | T_Schema _, T_Undefined ->
    bind_type environment variable (find variable environment)

  | T_Object _, _ | T_Schema _, _ ->
    bind_type environment variable t_variable

  | _ ->
    error 401 ("Prefix of " ^ !^variable ^ " is not an object or schema.")
;;

let copy srcPrefix destPrefix environment =
  MapRef.fold (fun variable ts accu ->
    if srcPrefix @< variable then
      begin
        let destVar = destPrefix @+ (variable @- srcPrefix) in
        if MapRef.mem destVar accu then
          MapRef.add destVar (List.append ts (MapRef.find destVar accu)) accu
        else
          MapRef.add destVar ts accu
      end
    else
      accu
  ) environment environment
;;

let rec resolve reference namespace environment =
  let rec prevail_of = function
    | [] -> ([], [])
    | ref ->
      begin match find ref environment with
      | T_Undefined -> prevail_of !-ref
      | T_Reference T_User (id, _) -> ([id], ref)
      | _ -> ([], [])
      end
  in
  match namespace, namespace @<< reference with
  | [], Domain.Invalid   -> ([], T_Undefined)
  | _ , Domain.Invalid   -> resolve reference !-namespace environment
  | [], Domain.Valid ref -> (ref, find ref environment)
  | _ , Domain.Valid ref ->
    begin match find ref environment with
    | T_Undefined ->
      begin match prevail_of ref with
      | [], _ -> resolve reference !-namespace environment
      | (schemaRef, srcRef) ->
        resolve (schemaRef @+ (ref @- srcRef)) [] environment
      end
    | t           -> (ref, t)
    end
;;

let inherit_ srcPrefix destPrefix namespace environment =
  let validSrcPrefix = match resolve srcPrefix namespace environment with
    | _, T_Undefined ->
      error 411 ("Prototype " ^ !^srcPrefix ^ " is not found.")

    | ref, T_Object _ | ref, T_Schema _ -> ref

    | _ ->
      error 412 ("Prototype " ^ !^srcPrefix ^ " is not an object or schema")
  in
  copy validSrcPrefix destPrefix environment
;;

let rec at ?env:(env = empty) indexes t_array =
  match indexes, t_array with
  | _, T_Forward T_Link ref | _, T_Forward T_Ref ref ->
    T_Forward (T_RefIndex (ref, indexes))

  | _, T_Forward T_RefIndex (ref, ids) ->
    T_Forward (T_RefIndex (ref, List.append ids indexes))

  | _ :: [], T_List tl -> tl
  | _ :: tail, T_List tl -> at tail tl
  | _, T_List _ -> error 413 "Invalid index of array."
  | _ -> error ~env:env
               414
               ("Accessing an index of a non-array value: " ^
                 (string_of_type t_array) ^ ".")
;;

let main_of mainReference environment =
  MapRef.fold (fun var ts accu ->
    if mainReference @< var then
      MapRef.add (var @- mainReference) ts accu
    else
      accu
  ) environment empty
;;

let rec resolve_forward_type ?visited:(visited = SetRef.empty) reference
                             namespace environment =
  if SetRef.exists (fun r -> r = reference) visited then
    error ~env:environment 422 ("Cyclic reference: " ^ !^reference ^ ".")
  else
    begin match resolve reference namespace environment with
    | _, T_Undefined ->
      error ~env:environment 423 ("Indeterminate forward-type: " ^ !^reference)

    | srcRef, T_Forward T_Ref r | srcRef, T_Forward T_Link r ->
      resolve_forward_type ~visited:(SetRef.add reference visited)
                           r
                           srcRef
                           environment

    | result -> result
    end
;;

let rec sub_forward_type t_forward var env = match t_forward with
  | T_Link ref -> resolve_forward_type ref var env
  | T_Ref ref ->
    begin match resolve_forward_type ref var env with
    | srcRef, T_Object t_obj -> (srcRef, T_Reference t_obj)
    | result -> result
    end
  | T_RefIndex (ref, indexes) ->
    begin
      let (srcRef, tl) = resolve_forward_type ref var env in
      match at ~env:env indexes tl with
      | T_Forward tf -> sub_forward_type tf var env
      | t -> (srcRef, t)
    end
;;

let rec sub_type_list t_list var env = match t_list with
  | T_Forward tf -> let (_, tl) = sub_forward_type tf var env in tl
  | T_List t -> T_List (sub_type_list t var env)
  | t -> t
;;

let rec replace_forward_type prefix environment =
  let replace var ts env = function
    | prototype, ((T_Object _) as t) ->
      begin
        let env1 = copy prototype var (MapRef.add var (t :: ts) env) in
        replace_forward_type var env1
      end
    | _, t -> MapRef.add var (t :: ts) env
  in
  MapRef.fold (fun var ts env ->
    match ts with
    | _ when not (prefix @< var) -> env

    | (T_Forward tf) :: tail ->
      replace var tail env (sub_forward_type tf var env)

    | (T_List tl) :: tail ->
      replace var tail env ([], T_List (sub_type_list tl var env))

    | _ -> env
  ) environment environment
;;

let rec merge_types prefix environment : map =
  let rec iter types t var =
    let test tx ts =
      if tx <: t then iter ts t var
      else error ~env:environment 430 ("Merge types failed: " ^ !^var ^ ".")
    in
    match types with
    | (T_Forward tf) :: ts ->
      begin
        let (_, tx) = sub_forward_type tf var environment in
        test tx ts
      end

    | (T_List tl) :: ts ->
      test (T_List (sub_type_list tl var environment)) ts

    | _ -> t
  in
  let foreach_variable_type var ts map = match ts with
    | [] -> error ~env:environment 432 ("Invalid type of '" ^ !^var ^ "'.")
    | _ when not (prefix @< var) -> map
    | t :: [] -> MapRef.add (var @- prefix) t map
    | t :: tail -> MapRef.add (var @- prefix) (iter tail t var) map
  in
  MapRef.fold foreach_variable_type environment MapRef.empty
;;

(*******************************************************************
 * a map from type to set of values
 *******************************************************************)

module MapType = Map.Make
(
  struct
    type t = Syntax.t
    let compare = Pervasives.compare
  end
)

type type_values = Domain.SetValue.t MapType.t ;;

let values_of t typeValues =
  if MapType.mem t typeValues then MapType.find t typeValues
  else Domain.SetValue.empty
;;

let make_type_values initTypeMap initFlatStore goalTypeMap goalFlatStore =
  let add t value typeValues =
    MapType.add t
                (Domain.SetValue.add value (values_of t typeValues))
                typeValues
  in
  let null = Domain.Basic Domain.Null in
  let rec add_object_value t_object value map =
    let map1 = add (T_Object t_object) value map in
    let t_reference = T_Reference t_object in
    let map2 = add t_reference value map1 in
    let map3 = add t_reference null map2 in
    match t_object with
    | T_Plain           -> map3
    | T_User (_, super) -> add_object_value super value map3
  in
  (*let add_enum_values t symbols map =
    if MapType.mem t map then
      map
    else
      List.fold_left (fun acc symbol ->
        add t (Domain.Basic (Domain.Symbol symbol)) acc
      ) map symbols
  in*)
  let add_from_store typeMap = MapRef.fold (fun var value map ->
      begin match type_of var typeMap with
      | T_Undefined | T_Forward _ ->
        error ~map:typeMap
              431
              ("Invalid type of '" ^ !^var ^ "'")

      | T_Object t_object ->
        add_object_value t_object
                         (Domain.Basic (Domain.Reference var))
                         map

      | T_Schema _ | T_Enum _ -> map
      (*| (T_Enum (id, symbols)) as t ->
        add_enum_values t symbols map*)

      | t -> add t value map
      end
    )
  in
  let add_from_actions typeMap =
    let add_from_effects = List.fold_left (fun map (reference, value) ->
        begin match type_of reference typeMap with
        | T_Undefined | T_Schema _ | T_Forward _ | T_Action | T_Constraint ->
          error ~map:typeMap
                432
                ("Invalid type of '" ^ !^reference ^ "'")

        | T_Object t_object ->
          add (T_Reference t_object) (Domain.Basic value) map

        | t -> add t (Domain.Basic value) map
        end
      )
    in
    Domain.SetValue.fold (fun value map ->
      begin match value with
      | Domain.Action (_, _, _, _, effects) -> add_from_effects map effects
      | _                                   -> map
      end
    )
  in
  let mapInit1 = add_from_store initTypeMap initFlatStore MapType.empty in
  let mapInit2 = add_from_actions initTypeMap
                                  (values_of T_Action mapInit1)
                                  mapInit1
  in
  let mapGoal1 = add_from_store goalTypeMap goalFlatStore mapInit2 in
  let mapGoal2 = add_from_actions goalTypeMap
                                  (values_of T_Action mapGoal1)
                                  mapGoal1
  in
  mapGoal2
;;
