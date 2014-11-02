(* Author: Herry (herry13@gmail.com) *)

open Common
open Syntax

(**
 * This module performs static type checking of given Nuri specifications.
 *
 * Any part with TODOC requires to be documented.
 *)

(*******************************************************************
 * type environment
 *******************************************************************)

type map        = t MapRef.t

type _t         = Type of t
                | NotFound
and var         = string list * t
and environment = var list
(*and lenv = environment*)

(*******************************************************************
 * helper functions
 *******************************************************************)

let rec (@==) reference1 reference2 =
    match reference1, reference2 with
    | [], []                                -> true
    | id1 :: rs1, id2 :: rs2 when id1 = id2 -> rs1 @== rs2
    | _                                     -> false
;;

(* alias of functions from module Domain *)
let (@++)  = Domain.(@++) ;;
let (@--)  = Domain.(@--) ;;
let prefix = Domain.prefix ;;
let (@<=)  = Domain.(@<=) ;;
let (@<)   = Domain.(@<) ;;
let (!!)   = Domain.(!!) ;;
let (@<<)  = Domain.(@<<) ;;
let (!^)   = Domain.(!^) ;;

exception TypeError of int * string

let error code message =
    raise (TypeError (code, "[type-err" ^ string_of_int(code) ^ "] " ^
        message))
;;

let string_of_map map =
    MapRef.fold (
        fun r t s -> s ^ (!^r) ^ " : " ^ (string_of_type t) ^ "\n"
    ) map ""
;;

(** convert a type environment to a map **)
let map_of environment =
    List.fold_left (
        fun acc (r, t) -> MapRef.add r t acc
    ) MapRef.empty environment
;;

let type_of var map =
    if MapRef.mem var map then MapRef.find var map
    else TUndefined
;;

(*******************************************************************
 * typing judgement functions
 *******************************************************************)

(** return the type of variable 'reference' **)
let rec find env var =
    match env with
    | []             -> NotFound
    | (r, t) :: tail -> if var = r then Type t
                        else find tail var
;;

(** return true if variable 'reference' is defined in the environment,
    otherwise false *)
let rec domain env var =
    match env with
    | []             -> false
    | (r, _) :: tail -> if var = r then true
                        else (domain tail var)
;;

(**
 * Subtyping rules
 * return true if type 'type1' is a sub-type of 'type2', otherwise false
 *)
let rec (<:) type1 type2 =
    let rec (<::) schema1 schema2 =
        match schema1, schema2 with
        | _, _ when schema1 = schema2 -> true               (* (Reflex)         *)
        | TUserSchema (_, _), TObject -> true               (* (Object Subtype) *)
        | TUserSchema (_, super), _   -> super <:: schema2  (* (Trans)          *)
        | _, _                        -> false
    in
    match type1, type2 with
    | _, _ when type1 = type2                       -> true      (* (Reflex)            *)
    | TAny, _ when type2 <> TUndefined              -> true      (* TODOC (Any Subtype) *)
    | TInt, TFloat                                  -> true      (* TODOC (IntFloat)    *)
    | TEnum (id1, _), TEnum (id2, _) when id1 = id2 -> true      (* TODOC (Enum)        *)
    | TSchema t1, TSchema t2                        -> t1 <:: t2
    | TList t1, TList t2                            -> t1 <: t2  (* (List Subtype)      *)
    | TRef t1, TRef t2                              -> t1 <:: t2 (* (Ref Subtype)       *)
    | TNull, TRef _                                 -> true      (* (Ref Null)          *)
    | _, _                                          -> false
;;

let subtype type1 type2 = type1 <: type2 ;;

(*******************************************************************
 * well-formed environments and types functions
 *******************************************************************)

(* return true if 't' is defined in the environment 'env', otherwise false *)
let rec has_type env t =
    match t with
    | TUndefined -> false                               (* 't' is not defined   *)
    | TBool                                             (* (Type Bool)          *)
    | TInt                                              (* (Type Int)           *)
    | TFloat                                            (* (Type Float)         *)
    | TString                                           (* (Type String)        *)
    | TNull                                             (* (Type Null)          *)
    | TAny                                              (* (Type Any)           *) 
    | TAction                                           (* (Type Action)        *)
    | TGlobal                                           (* (Type Constraint)    *)
    | TEnum (_, _)                                      (* TODOC (Type Enum)    *)
    | TForward (_, _)                                   (* TODOC (Type Forward) *)
    | TSchema TObject                                   (* (Type Object)        *)
    | TSchema TRootSchema -> true                       (* TODOC (Type Schema)  *)
    | TList tl            -> has_type env tl            (* (Type List)          *)
    | TRef tr             -> has_type env (TSchema tr)  (* (Type Ref)           *)
    | TSchema TUserSchema (id1, _) -> (                 (* (Type UserSchema)    *)
        match env with
        | [] -> false
        | (_, TSchema (TUserSchema (id2, _))) :: tail -> if id1 = id2 then true
                                                         else has_type tail t
        | (_, _) :: tail -> has_type tail t
    )
;;

let is_well_typed env =
    let rec iter e env =
        match e with
        | []             -> true
        | (r, t) :: tail -> if (has_type env t) then iter tail env
                            else false
    in
    iter env env
;;

let is_schema t = t <: (TSchema TRootSchema) ;;

let rec object_of_schema t =
    let rec object_of t =
        match t with
        | TRootSchema -> TObject
        | TUserSchema (id, super) -> TUserSchema (id, object_of super)
        | _ -> error 401 "Cannot create type of object of a non-schema type"
    in
    match t with
    | TSchema TUserSchema (id, TRootSchema) -> TSchema (TUserSchema (id, TObject))
    | TSchema TUserSchema (id, super)       -> TSchema (TUserSchema (id, object_of super))
    | _ -> error 401 "Cannot create type object of a non-schema type"
;;

(*******************************************************************
 * type assignment functions
 *******************************************************************)

(* bind 't' to variable 'reference' *)
let bind env var t =
    match (find env var), var with
    | NotFound, []       -> error 402 "Invalid variable."
    | NotFound, id :: [] -> (var, t) :: env
    | NotFound, _ ->
        (
            match find env (prefix var) with
            | Type TEnum (_, _) -> (var, t) :: env
            | Type t1 when t1 <: TSchema TObject -> (var, t) :: env
            | NotFound -> error 403 ("Prefix of " ^ !^var ^ " is undefined.")
            | _ -> error 404 ("Prefix of " ^ !^var ^ " is not an object.")
        )
    | _, _ -> error 405 ("cannot bind type to an existing variable " ^ !^var)
;;

(**
 * @param typeEnv       type environment
 * @param reference     reference of the variable
 * @param _type         pre-defined type
 * @param typeValue     type of value which will be assigned
 *)
let assign env var t tValue =
    match (find env var), t, tValue with
    | NotFound, TUndefined, TAny ->
        error 405 (!^var ^ " cannot assign Any to an undefined variable")
    | NotFound, TUndefined, _ ->                                 (* (Assign1) *)
        bind env var tValue
    | NotFound, _, _ when tValue <: t ->                  (* (Assign3) *)
        bind env var t
    | Type (TForward (_, _)), _, _                               (* TODOC *)
    | NotFound, _, TForward (_, _) ->                            (* TODOC (Assign5) *)
        (var, t) :: (var, tValue) :: env
    | NotFound, _, _ ->
        error 406 (!^var ^ " not satisfy rule (Assign3)")
    | Type tVar, TUndefined, _ when tValue <: tVar ->   (* (Assign2) *)
        env
    | Type tVar, TUndefined, TForward (_, _) ->               (* TODOC (Assign6) *)
        (var, tValue) :: env
    | Type _, TUndefined, _ ->
        error 407 (!^var ^ " not satisfy rule (Assign2)")
    | Type tVar, _, _
      when (tValue <: t) && (t <: tVar) ->         (* (Assign4) *)
        env
    | Type _, _, _ ->
        error 408 (!^var ^ " not satisfy rule (Assign2) & (Assign4)")
;;

(**
 * TODOC
 * Return part of type environment where the reference is the prefix of
 * the variables. The prefix of variables will be removed when 'replacePrefix'
 * is true, otherwise the variables will have original references.
 *)
let rec env_of_ref env var replacePrefix =
    if var = [] then env
    else if env = [] then []
    else
        List.fold_left (fun e (r, t) ->
            if var @< r then
                let re = if replacePrefix then (r @-- var) else r in
                (re, t) :: e
            else
                e
        ) [] env
;;

(**
 * TODOC
 * @param env   type environment
 * @param base  the prefix where the variable will be resolved
 * @param var   var to be resolved
 *)
let rec resolve env base var =
    match base, var with
    | _, "root"   :: rs -> ([], find env !!rs)
    | _, "parent" :: rs ->
        if base = []
            then error 409 "Parent of root is impossible."
        else
            (prefix base, find env !!((prefix base) @++ rs))
    | _, "this"   :: rs -> (base, find env !!(base @++ rs))
    | [], _             -> ([], find env var)
    | _, _ ->
        match find env (base @<< var) with
        | NotFound -> resolve env (prefix base) var
        | t        -> (base, t)
;;

(**
 * TODOC
 * Copy attributes of prototype to the destination reference. This is used
 * to inherit attributes from a prototype to another object.
 *
 * @param env      type environment
 * @param proto    prototype reference whose attributes to be copied
 * @param dest     reference of destination
 *)
let copy env proto dest =
    let protoEnv = env_of_ref env proto true in
    List.fold_left (
        fun ep (rep, tep) -> (dest @++ rep, tep) :: ep
    ) env protoEnv
;;

(**
 * TODOC
 * Inherit attributes from prototype to an object.
 *
 * @param env   type environment
 * @param base  the base prefix where variables will be resolved
 * @param proto reference of prototype
 * @param var   target variable
 *)
let inherit_env env base proto var =
    let get_proto =
        match resolve env base proto with
        | _, NotFound -> error 410 ("Prototype is not found: " ^ !^proto)
        | baseProto, Type t when t <: TSchema TObject -> baseProto @++ proto
        | _, Type t -> error 411 ("Invalid prototype " ^ !^proto ^ ":" ^ (string_of_type t))
    in
    copy env get_proto var 
;;

(*******************************************************************
 * second-pass type environment
 *******************************************************************)

(**
 * TODOC
 * Resolve a forward type.
 *
 * @param env   type environment
 * @param base  base prefix to resolve references
 * @param var   variable of the value
 * @param accumulator  accumulator of visited variables
 *)
let rec resolve_forward_type env base var accumulator =
    let follow_forward_type base refValue =
        let r = base @++ var in
        if SetRef.exists (fun rx -> rx = r) accumulator
            then error 413 ("Cyclic reference detected: " ^ !^r)
        else if r @<= refValue
            then error 412 ("Implicit cyclic reference detected: " ^ !^refValue)
        else
            resolve_forward_type env r refValue (SetRef.add r accumulator)
    in
    let (base1, var1) =
        match var with
        | "root" :: rs   -> ([], rs)
        | "parent" :: rs ->
            if base = [] then
                error 433 ("Invalid variable: " ^ !^var)
            else
                ([], (prefix base) @++ rs)
        | "this" :: rs   -> ([], base @++ rs)
        | _              -> (base, var)
    in
    if SetRef.exists (fun r -> r = var1) accumulator
        then error 413 ("Cyclic reference " ^ !^var)
    else
        match resolve env base1 var1 with
        | _, NotFound -> error 414 (!^var1 ^ " is not found in " ^ !^base1)
        | base2, Type TForward (var2, _) -> follow_forward_type base2 var2
        | base2, Type t -> (base2 @++ var1, t)
;;

(**
 * TODOC
 * Second pass: visit every element of 'typeEnv', and then resolved all variables
 * that have type TForward
 *)
let replace_forward_type_in env mainReference =
    let replace env var t refForward tForward =
        let (proto, t_val) =
            match resolve_forward_type env var refForward SetRef.empty with
            | proto, TSchema t ->
                (
                    match tForward with
                    | TLinkForward -> (proto, TSchema t)
                    | TRefForward  -> (proto, TRef t)
                )
            | proto, t -> (proto, t)
        in
        let env1 = (var, t_val) :: env in
        if t_val <: TSchema TObject then copy env1 proto var
        else env1
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
                    | TForward (refForward, tForward) -> replace env r t refForward tForward
                    | _                               -> (r, t) :: env
                in
                iter result tail
    in
    iter env env
;;

(**
 * Perform second valuation of environment 'env':
 * - resolve all TForwards
 *)
let second_pass_eval env mainReference =
    let env1 = replace_forward_type_in env mainReference in
    if is_well_typed env1 then env1
    else error 417 "Type environment is not well-typed."
;;

(**
 * Extract main object.
 *)
let get_main env mainReference =
    let rec iter src dest =
        match src with
        | []             -> dest
        | (r, t) :: tail ->
            if not (mainReference @<= r) then
                iter tail dest
            else
                let r1 = List.tl r in
                if r1 = [] || domain dest r1 then iter tail dest
                else iter tail ((r1, t) :: dest)
    in
    iter env []

(*******************************************************************
 * type inference and valuation functions
 *******************************************************************)

let sfBoolean b = TBool  (* (Bool) *)

let sfInt i = TInt (* (Int) *)

let sfFloat f = TFloat (* (Float) *)

let sfString s = TString    (* (Str)  *)

let sfNull = TNull       (* (Null) *)

let sfReference r = r

(* (Deref Data) *)
let sfDataReference dr : environment -> reference -> t =
    (**
     * @param ns namespace
     * @param e  type environment
     *)
    fun e ns ->
        let r = (sfReference dr) in
        match resolve e ns r with
        | _, Type TForward (rz, tf)    -> TForward (rz, tf)
        | _, Type TEnum (id, elements) -> TEnum (id, elements)
        | _, Type TBool       -> TBool
        | _, Type TInt        -> TInt
        | _, Type TFloat      -> TFloat
        | _, Type TString     -> TString
        | _, Type TNull       -> TNull
        | _, Type TAny        -> TAny
        | _, Type TAction     -> TAction
        | _, Type TGlobal -> TGlobal
        | _, Type TList t     -> TList t
        | _, Type TRef ts     -> TRef ts
        | _, Type TSchema ts  when (TSchema ts) <: (TSchema TObject) -> TRef ts
        | _, Type TSchema _   -> error 420 ("Dereference of " ^ !^r ^ " is a schema")
        | _, Type TUndefined  -> error 421 ("Dereference of " ^ !^r ^ " is TUndefined")
        | _, NotFound         -> TForward (r, TRefForward)

(* (Deref Link) *)
let sfLinkReference lr : environment -> reference -> reference ->
    (reference * t) =
    (**
     * @param ns namespace
     * @param e  type environment
     *)
    fun e ns r ->
        let link = sfReference lr in
        match resolve e ns link with
        | nsp, NotFound -> (nsp @++ link, TForward (link, TLinkForward))
        | nsp, Type t   -> (nsp @++ link, t)

let rec sfVector vec : environment -> reference -> t =
    (**
     * @param ns namespace
     * @param e  type environment
     *)
    fun e ns ->
        let rec eval v =  (* (Vec) *)
            match v with
            | [] -> TUndefined
            | head :: tail ->
                let t_head =
                    match sfBasicValue head e ns with
                    | TForward _ -> TUndefined
                    | t          -> t
                in
                if tail = [] then t_head
                else if t_head = eval tail then t_head
                else error 422 "types of vector elements are different"
        in
        TList (eval vec)

and sfBasicValue bv : environment -> reference -> t =
    (**
     * @param ns namespace
     * @param e  type environment
     *)
    fun e ns ->
        match bv with
        | Boolean b    -> sfBoolean b
        | Int i        -> sfInt i
        | Float f      -> sfFloat f
        | String s     -> sfString s
        | Null         -> sfNull
        | Vector vec   -> sfVector vec e ns
        | Reference dr -> sfDataReference dr e ns

(**
 * @param proto prototype AST element
 * @param first true if this is the first prototype, otherwise false
 * @param t_val the type of the first prototype
 *)
let rec sfPrototype proto first t_val : reference -> reference ->
    environment -> environment =
    (**
     * @param ns namespace
     * @param r  target variable
     * @param e  type environment
     *)
    fun ns r e ->
        match proto with
        | EmptyPrototype -> (* (Proto1) *)
            if first then assign e r t_val (TSchema TObject)
            else e
        | BlockPrototype (pb, p) -> (* (Proto2) *)
            let t_block =
                if first && t_val = TUndefined then TSchema TObject
                else t_val
            in
            let e1 = assign e r t_val t_block in
            sfPrototype p false t_block ns r (nuriBlock pb r e1)
        | ReferencePrototype (pr, p) ->
            let proto = sfReference pr in
            match resolve e ns proto with
            | _, NotFound -> error 423 ("prototype is not found: " ^ !^proto)
            | _, Type t   -> (* (Proto3) & (Proto4) *)
                let e_proto = assign e r t_val t in
                let t_proto = if first then t else t_val in
                sfPrototype p false t_proto ns r
                    (inherit_env e_proto ns proto r)

and sfValue v : reference -> reference -> t -> environment ->
    environment =
    (**
     * @param ns namespace
     * @param r  variable's reference
     * @param t  predefined type
     * @param e  type environment
     *)
    fun ns r t e ->
        let t =
            match t with
            | TSchema TUserSchema (id, _) -> (
                match find e [id] with
                | Type TEnum (eid, elements) -> TEnum (eid, elements)
                | _                          -> t
            )
            | _ -> t
        in
        match v with
        | TBD -> assign e r t TAny
        | Unknown -> assign e r t TAny
        | Nothing -> assign e r t TAny
        | Basic bv -> assign e r t (sfBasicValue bv e ns)
        | Link link ->
            (
                let (r_link, t_link) = sfLinkReference link e ns r in
                let e1 = assign e r t t_link in
                if t_link <: TSchema TObject then copy e1 r_link r
                else e1
            )
        | Action a -> assign e r t TAction
        | Prototype (schema, proto) ->
            match schema with
            | SID sid ->
                (
                    match find e [sid] with
                    | NotFound  -> error 424 ("schema " ^ sid ^ " is not exist")
                    | Type TSchema TUserSchema (sid, super)
                      when (TSchema super) <: (TSchema TRootSchema) ->
                          let t_sid = object_of_schema (TSchema (TUserSchema (sid, super))) in
                          let e1 = inherit_env e ns [sid] r in
                          sfPrototype proto true t_sid ns r e1
                    | _ ->
                        error 425 (sid ^ " is not a schema")
                )
            | EmptySchema -> sfPrototype proto true TUndefined ns r e

and sfAssignment (r, t, v) : reference -> environment -> environment =
    (**
     * @param ns namespace
     * @param e  type environment
     *)
    fun ns e -> sfValue v ns (ns @++ r) t e

and nuriBlock block : reference -> environment -> environment =
    (**
     * @param ns namespace
     * @param e  type environment
     *)
    fun ns e ->
        match block with
        | AssignmentBlock (a, b) -> nuriBlock b ns (sfAssignment a ns e)
        | GlobalBlock (g, b)     -> nuriBlock b ns (nuriGlobal g e)
        | EmptyBlock             -> e

and nuriSchema s : environment -> environment =
    let (id, parent, b) = s in
    fun e ->
        let r_id = [id] in
        if domain e r_id then error 426 (id ^ " is bound multiple times.");
        let define_schema id super =
            let t = TSchema (TUserSchema (id, super)) in
            let e1 = assign e r_id TUndefined t in
            let e2 =
                match super with
                | TUserSchema (super_id, _) -> inherit_env e1 [] [super_id] r_id
                | _ -> e1
            in
            nuriBlock b r_id e2
        in
        match parent with
        | EmptySchema -> define_schema id TRootSchema
        | SID super ->
            match find e [super] with
            | NotFound -> error 427 ("super schema " ^ super ^ " is not exist")
            | Type TSchema ts -> if is_schema (TSchema ts) then define_schema id ts
                                 else error 108 (super ^ " is not a schema")
            | _ -> error 428 (super ^ " is not a schema")

and nuriEnum enum : environment -> environment =
    let (id, elements) = enum in
    fun e ->
        let r_id = [id] in
        if domain e r_id then error 440 (id ^ " is bound multiple times.");
        let t_enum = TEnum (id, elements) in
        let e1 = assign e r_id TUndefined t_enum in
        List.fold_left (fun acc el ->
            let r_el = [id; el] in
            assign acc r_el TUndefined t_enum
        ) e1 elements

and nuriGlobal g : environment -> environment =
    fun e -> assign e ["global"] TUndefined TGlobal

and nuriContext ctx : environment -> environment =
    fun e ->
        match ctx with
        | AssignmentContext (a, c) -> nuriContext c (sfAssignment a [] e)
        | SchemaContext (s, c)     -> nuriContext c (nuriSchema s e)
        | EnumContext (enum, c)    -> nuriContext c (nuriEnum enum e)
        | GlobalContext (g, c)     -> nuriContext c (nuriGlobal g e)
        | EmptyContext             -> e

and nuriSpecification ?main:(mainReference=["main"]) nuri =
    let e1 = nuriContext nuri [] in
    if mainReference = [] then
        let e2 = second_pass_eval e1 mainReference in
        map_of e2
    else if not (domain e1 mainReference) then
        error 429 "main object is not exist"
    else
        let e2 = get_main (second_pass_eval e1 mainReference) mainReference in
        let e_main = assign e2 ["global"] TUndefined TGlobal in
        map_of e_main
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

type type_values = Domain.SetValue.t MapType.t

let values_of t typeValues =
    if MapType.mem t typeValues then MapType.find t typeValues
    else Domain.SetValue.empty

let add_value t value typeValues =
    MapType.add t (Domain.SetValue.add value (values_of t typeValues)) typeValues

let make_type_values typeEnvInit flatStoreInit typeEnvGoal flatStoreGoal =
    (** group action effects' values based on their type **)
    let add_action_values environment map =
        let actions = values_of TAction map in
        let add_effect_values =
            List.fold_left (
                fun acc (r, v) ->
                    match v with
                    | Domain.Boolean _ ->
                        add_value TBool (Domain.Basic v) acc
                    | Domain.Int     _ ->
                        add_value TInt (Domain.Basic v) acc
                    | Domain.Float   _ ->
                        add_value TFloat (Domain.Basic v) acc
                    | Domain.String  _ ->
                        add_value TString (Domain.Basic v) acc
                    | Domain.Vector  _ -> (* TODO *)
                        error 430 "TODO: adding vector value of effects"
                    | _         -> acc
            )
        in
        Domain.SetValue.fold (
            fun v map ->
                match v with
                | Domain.Action (n, ps, c, pre, eff) ->
                    add_effect_values map eff
                | _                           -> map
        ) actions map
    in
    let null = Domain.Basic Domain.Null in
    let rec add_object ts value map =
        let map = add_value (TSchema ts) value map in
        let map = add_value (TRef ts) value map in
        let map = add_value (TRef ts) null map in
        match ts with
        | TObject -> add_value (TSchema TObject) value map
        | TUserSchema (_, super) -> add_object super value map
        | _ -> map
    in
    let add_store_values typeEnv =
        MapRef.fold (
            fun r v (map: type_values) ->
                match type_of r typeEnv with
                | TUndefined -> error 432 ("Type of " ^ !^r ^ " is undefined.")
                | TSchema ts when (TSchema ts) <: (TSchema TObject) ->
                    add_object ts (Domain.Basic (Domain.Ref r)) map
                | TSchema _ -> map
                | t -> add_value t v map
        )
    in
    let map00 = add_store_values typeEnvInit flatStoreInit MapType.empty in
    let map01 = add_action_values typeEnvInit map00 in
    let map10 = add_store_values typeEnvGoal flatStoreGoal map01 in
    add_action_values typeEnvGoal map10
