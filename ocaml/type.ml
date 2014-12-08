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
let (@+.)  = Domain.(@+.) ;;
let (@--)  = Domain.(@--) ;;
let (!--)  = Domain.(!--) ;;
let (@<=)  = Domain.(@<=) ;;
let (@<)   = Domain.(@<) ;;
let (@<<)  = Domain.(@<<) ;;
let (!<<)   = Domain.(!<<) ;;

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
    else T_Undefined
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
        | T_Schema (id1, _), T_Schema (id2, _) when id1 = id2 -> true
        | T_Schema (_, _), T_PlainObject -> true               (* (Object Subtype) *)
        | T_Schema (_, super), _   -> super <:: schema2  (* (Trans)          *)
        | _, _                        -> false
    in
    match type1, type2 with
    | _, _ when type1 = type2                       -> true      (* (Reflex)            *)
    | T_Any, _ when type2 <> T_Undefined              -> true      (* TODOC (Any Subtype) *)
    | T_Int, T_Float                                  -> true      (* TODOC (IntFloat)    *)
    | T_Enum (id1, _), T_Enum (id2, _) when id1 = id2 -> true      (* TODOC (Enum)        *)
    | T_Object t1, T_Object t2                        -> t1 <:: t2
    | T_List t1, T_List t2                            -> t1 <: t2  (* (List Subtype)      *)
    | T_Reference t1, T_Reference t2                              -> t1 <:: t2 (* (Ref Subtype)       *)
    | T_Null, T_Reference _                                 -> true      (* (Ref Null)          *)
    | _, _                                          -> false
;;

let subtype type1 type2 = type1 <: type2 ;;

(*******************************************************************
 * well-formed environments and types functions
 *******************************************************************)

(* return true if 't' is defined in the environment 'env', otherwise false *)
let rec has_type env t =
    match t with
    | T_Undefined -> false                               (* 't' is not defined   *)
    | T_Bool                                             (* (Type Bool)          *)
    | T_Int                                              (* (Type Int)           *)
    | T_Float                                            (* (Type Float)         *)
    | T_String                                           (* (Type String)        *)
    | T_Null                                             (* (Type Null)          *)
    | T_Any                                              (* (Type Any)           *) 
    | T_Action                                           (* (Type Action)        *)
    | T_Constraint                                           (* (Type Constraint)    *)
    | T_Enum _                                           (* TODOC (Type Enum)    *)
    | T_Forward _                                        (* TODOC (Type Forward) *)
    | T_Object T_PlainObject                                   (* (Type Object)        *)
    | T_Object T_PlainSchema -> true                       (* TODOC (Type Schema)  *)
    | T_List tl            -> has_type env tl            (* (Type List)          *)
    | T_Reference tr             -> has_type env (T_Object tr)  (* (Type Ref)           *)
    | T_Object T_Schema (id1, _) -> (                 (* (Type UserSchema)    *)
        match env with
        | [] -> false
        | (_, T_Object (T_Schema (id2, _))) :: tail -> if id1 = id2 then true
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

let is_schema t = t <: (T_Object T_PlainSchema) ;;

let rec object_of_schema t =
    let rec object_of t =
        match t with
        | T_PlainSchema -> T_PlainObject
        | T_Schema (id, super) -> T_Schema (id, object_of super)
        | _ -> error 401 "Cannot create type of object of a non-schema type"
    in
    match t with
    | T_Object T_Schema (id, T_PlainSchema) -> T_Object (T_Schema (id, T_PlainObject))
    | T_Object T_Schema (id, super)       -> T_Object (T_Schema (id, object_of super))
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
            match find env !--var with
            | Type T_Enum (_, _) -> (var, t) :: env
            | Type t1 when t1 <: T_Object T_PlainObject -> (var, t) :: env
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
    | NotFound, T_Undefined, T_Any ->
        error 490 ("An explicit type of " ^ !^var ^ " is required.")
    | NotFound, T_Undefined, _ ->                                 (* (Assign1) *)
        bind env var tValue
    | NotFound, _, _ when tValue <: t ->                  (* (Assign3) *)
        bind env var t
    | Type (T_Forward _), _, _                                    (* TODOC *)
    | NotFound, _, T_Forward _ ->                            (* TODOC (Assign5) *)
        (var, t) :: (var, tValue) :: env
    | NotFound, _, _ ->
        error 406 (!^var ^ " not satisfy rule (Assign3)")
    | Type tVar, T_Undefined, _ when tValue <: tVar ->   (* (Assign2) *)
        env
    | Type tVar, T_Undefined, T_Forward _ ->               (* TODOC (Assign6) *)
        (var, tValue) :: env
    | Type _, T_Undefined, _ ->
        error 407 (!^var ^ " not satisfy rule (Assign2)")
    | Type tVar, _, _
      when (tValue <: t) && (t <: tVar) ->         (* (Assign4) *)
        env
    | Type _, _, _ ->
        error 408 (!^var ^ " not satisfy rule (Assign2) & (Assign4)")
;;

(**
 * TODOC
 * Return part of type environment where the reference is the !--of
 * the variables. The !--of variables will be removed when 'replacePrefix'
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
 * @param base  the !--where the variable will be resolved
 * @param var   var to be resolved
 *)
let rec resolve env base var =
    match base, var with
    | _, "root"   :: rs -> ([], find env !<<rs)
    | _, "parent" :: rs ->
        if base = []
            then error 409 "Parent of root is impossible."
        else
            (!--base, find env !<<(!--base @++ rs))
    | _, "this"   :: rs -> (base, find env !<<(base @++ rs))
    | [], _             -> ([], find env var)
    | _, _ ->
        match find env (base @<< var) with
        | NotFound -> resolve env (!--base) var
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
 * @param base  the base !--where variables will be resolved
 * @param proto reference of prototype
 * @param var   target variable
 *)
let inherit_env env base proto var =
    let get_proto =
        match resolve env base proto with
        | _, NotFound -> error 410 ("Prototype is not found: " ^ !^proto)
        | baseProto, Type t when t <: T_Object T_PlainObject -> baseProto @++ proto
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
 * @param base  base !--to resolve references
 * @param var   variable of the value
 * @param accumulator  accumulator of visited variables
 *)
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

(**
 * TODOC
 * Second pass: visit every element of 'typeEnv', and then resolved all variables
 * that have type T_Forward
 *)
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

(**
 * Perform second valuation of environment 'env':
 * - resolve all T_Forwards
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

let sfBoolean b = T_Bool  (* (Bool) *)

let sfInt i = T_Int (* (Int) *)

let sfFloat f = T_Float (* (Float) *)

let sfString s = T_String    (* (Str)  *)

let sfNull = T_Null       (* (Null) *)

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
        | _, Type T_Forward t  -> T_Forward t
        | _, Type T_Enum (id, elements) -> T_Enum (id, elements)
        | _, Type T_Bool       -> T_Bool
        | _, Type T_Int        -> T_Int
        | _, Type T_Float      -> T_Float
        | _, Type T_String     -> T_String
        | _, Type T_Null       -> T_Null
        | _, Type T_Any        -> T_Any
        | _, Type T_Action     -> T_Action
        | _, Type T_Constraint -> T_Constraint
        | _, Type T_List t     -> T_List t
        | _, Type T_Reference ts     -> T_Reference ts
        | _, Type T_Object ts  when (T_Object ts) <: (T_Object T_PlainObject) -> T_Reference ts
        | _, Type T_Object _   -> error 420 ("Dereference of " ^ !^r ^ " is a schema")
        | _, Type T_Undefined  -> error 421 ("Dereference of " ^ !^r ^ " is T_Undefined")
        | _, NotFound         -> T_Forward (T_ReferenceForward r)

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
        | nsp, NotFound -> (nsp @++ link, T_Forward (T_LinkForward link))
        | nsp, Type t   -> (nsp @++ link, t)

let rec sfVector vec : environment -> reference -> t =
    (**
     * @param ns namespace
     * @param e  type environment
     *)
    fun e ns ->
        let rec eval v =  (* (Vec) *)
            match v with
            | [] -> T_Undefined
            | head :: tail ->
                let t_head =
                    match sfBasicValue head e ns with
                    | T_Forward _ -> T_Undefined
                    | t          -> t
                in
                if tail = [] then t_head
                else if t_head = eval tail then t_head
                else error 422 "types of vector elements are different"
        in
        T_List (eval vec)

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
            if first then assign e r t_val (T_Object T_PlainObject)
            else e
        | BlockPrototype (pb, p) -> (* (Proto2) *)
            let t_block =
                if first && t_val = T_Undefined then T_Object T_PlainObject
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

and nuriExpression exp : reference -> reference -> t -> environment -> t =
    fun ns r t e ->
        let unary operator expression (checker : t -> t) =
            match nuriExpression expression ns r t e with
            | T_Forward _ -> error 441 ("The operand's type of '" ^ operator ^ "' is indeterminate.")
            | T_Undefined -> error 442 ("The operand's type of '" ^ operator ^ "' is undefined.")
            | t -> checker t
        in
        let binary operator exp1 exp2 (checker : t -> t -> t) =
            match (nuriExpression exp1 ns r t e), (nuriExpression exp2 ns r t e) with
            | T_Forward _, _ ->
                error 443 ("The left operand's type of '" ^ operator ^ "' is indeterminate.")
            | _, T_Forward _ ->
                error 444 ("The right operand's type of '" ^ operator ^ "' is indeterminate.")
            | T_Undefined, _ ->
                error 445 ("The left operand's type of '" ^ operator ^ "' is undefined.")
            | _, T_Undefined ->
                error 446 ("The right operand's type of '" ^ operator ^ "' is undefined.")
            | t1, t2 -> checker t1 t2
        in
        let binary_logic operator left right = binary operator left right (fun tLeft tRight ->
                if tLeft <: T_Bool && tRight <: T_Bool then T_Bool
                else error 447 ("Left and right operands of '" ^ operator ^ "' are not booleans.")
            )
        in
        let binary_arithmetic operator left right = binary operator left right (fun tLeft tRight ->
                match tLeft, tRight with
                | T_Int, T_Int -> T_Int
                | T_Int, T_Float | T_Float, T_Int | T_Float, T_Float -> T_Float
                | _ -> error 448 ("Left or right operand of '" ^ operator ^ "' is neither an integer nor a float.")
            )
        in
        match exp with
        | Basic bv      -> sfBasicValue bv e ns
        | Shell _       -> T_String (* TODO: Documentation *)
        | Exp_Eager exp -> unary "$" exp (fun t -> t)
        | Exp_IString _ -> T_String
        | Exp_Not exp -> unary "!" exp (fun t ->
                if t <: T_Bool then T_Bool
                else error 448 "The operand of '!' is not a boolean."
            )
        | Exp_Equal (exp1, exp2) -> binary "==" exp1 exp2 (fun t1 t2 ->
                if t1 <: t2 || t2 <: t1 then T_Bool
                else error 450 "The types of left and right operands of '==' are not the same."
            )
        | Exp_NotEqual (exp1, exp2) -> binary "!=" exp1 exp2 (fun t1 t2 ->
                if t1 <: t2 || t2 <: t1 then T_Bool
                else error 457 "The types of left and right operands of '!=' are not the same."
            )
        | Exp_And   (left, right) -> binary_logic "&&" left right
        | Exp_Or    (left, right) -> binary_logic "||" left right
        | Exp_Imply (left, right) -> binary_logic "=>" left right
        | Exp_Add (exp1, exp2) -> binary "+" exp1 exp2 (fun t1 t2 ->
                match t1, t2 with
                | T_Int, T_Float | T_Float, T_Int | T_Float, T_Float -> T_Float
                | T_Int, T_Int -> T_Int
                | T_String, _ | _, T_String -> T_String
                | _ -> error 451 "Both operands of '+' are neither integer, float, or string."
            )
        | Exp_Subtract (exp1, exp2) -> binary_arithmetic "-" exp1 exp2
        | Exp_Multiply (exp1, exp2) -> binary_arithmetic "*" exp1 exp2
        | Exp_Divide (exp1, exp2)   -> binary_arithmetic "/" exp1 exp2
        | Exp_Modulo (exp1, exp2)   -> binary_arithmetic "%" exp1 exp2
        | Exp_MatchRegexp (exp, regexp) -> unary "=~" exp (fun t ->
                if t <: T_String then T_Bool
                else error 452 "The left operand of '=~' is not a string."
            )
        | Exp_IfThenElse (exp1, exp2, exp3) ->
            (   (* TODO: Documentation *)
                match (nuriExpression exp1 ns r t e),
                      (nuriExpression exp2 ns r t e),
                      (nuriExpression exp3 ns r t e)
                with
                | T_Bool, T_Forward _, _ -> error 453 ("The type of 'then' expression is indeterminate.")
                | T_Bool, _, T_Forward _ -> error 454 ("The type of 'else' expression is indeterminate.")
                | T_Bool, t2, t3 when t2 <: t3 && t3 <: t2 -> t2
                | T_Bool, t2, t3 -> error 455 ("The types of 'then' (" ^ (string_of_type t2) ^ ") " ^
                                               "and 'else' (" ^ (string_of_type t3) ^ ") expressions " ^
                                               "are not the same.")
                | _, _, _ -> error 456 "The type of 'if' expression is not a boolean."
            )

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
            | T_Object T_Schema (id, _) -> (
                match find e [id] with
                | Type T_Enum (eid, elements) -> T_Enum (eid, elements)
                | _                          -> t
            )
            | _ -> t
        in
        let r_name = r @+. "name" in
        match v with
        | Expression exp -> assign e r t (nuriExpression exp ns r t e)
        | TBD -> assign e r t T_Any
        | Unknown -> assign e r t T_Any
        | None -> assign e r t T_Any
        | Link link ->
            (
                let (r_link, t_link) = sfLinkReference link e ns r in
                let e1 = assign e r t t_link in
                if t_link <: T_Object T_PlainObject then copy e1 r_link r
                else e1
            )
        | Action a -> assign e r t T_Action
        | Prototype (schema, proto) ->
            (
                let e = match schema with
                    | SID sid ->
                        (
                            match find e [sid] with
                            | NotFound  -> error 424 ("schema " ^ sid ^ " is not found.")
                            | Type T_Object T_Schema (sid, super)
                              when (T_Object super) <: (T_Object T_PlainSchema) ->
                                  let t_sid = object_of_schema (T_Object (T_Schema (sid, super))) in
                                  let e1 = inherit_env e ns [sid] r in
                                  sfPrototype proto true t_sid ns r e1
                            | _ ->
                                error 425 (sid ^ " is not a schema")
                        )
                    | EmptySchema -> sfPrototype proto true T_Undefined ns r e
                in
                match find e r_name with
                | NotFound -> assign e r_name T_Undefined T_String
                | _ -> e
            )

and sfAssignment (r, t, v) : reference -> environment -> environment =
    (**
     * @param ns namespace
     * @param e  type environment
     *)
    fun ns ->
        if r = _echo_ then fun e -> e
        else sfValue v ns (ns @++ r) t

and nuriBlock block : reference -> environment -> environment =
    (**
     * @param ns namespace
     * @param e  type environment
     *)
    fun ns e ->
        match block with
        | AssignmentBlock (a, b) -> nuriBlock b ns (sfAssignment a ns e)
        | TrajectoryBlock (t, b) -> nuriBlock b ns (nuriTrajectory t e)
        | EmptyBlock             -> e

and nuriSchema s : environment -> environment =
    let (id, parent, b) = s in
    fun e ->
        let r_id = [id] in
        if domain e r_id then error 426 (id ^ " is bound multiple times.");
        let define_schema id super =
            let t = T_Object (T_Schema (id, super)) in
            let e1 = assign e r_id T_Undefined t in
            let e2 =
                match super with
                | T_Schema (super_id, _) -> inherit_env e1 [] [super_id] r_id
                | _ -> e1
            in
            nuriBlock b r_id e2
        in
        match parent with
        | EmptySchema -> define_schema id T_PlainSchema
        | SID super ->
            match find e [super] with
            | NotFound -> error 427 ("Super schema " ^ super ^ " is not found.")
            | Type T_Object ts -> if is_schema (T_Object ts) then define_schema id ts
                                 else error 108 (super ^ " is not a schema")
            | _ -> error 428 (super ^ " is not a schema")

and nuriEnum enum : environment -> environment =
    let (id, elements) = enum in
    fun e ->
        let r_id = [id] in
        if domain e r_id then error 440 (id ^ " is bound multiple times.");
        let t_enum = T_Enum (id, elements) in
        let e1 = assign e r_id T_Undefined t_enum in
        List.fold_left (fun acc el ->
            let r_el = [id; el] in
            assign acc r_el T_Undefined t_enum
        ) e1 elements

and nuriGlobal g : environment -> environment =
    fun e -> assign e ["global"] T_Undefined T_Constraint

and nuriTrajectory t = match t with
    | Global g -> nuriGlobal g

and nuriContext ctx : environment -> environment =
    fun e ->
        match ctx with
        | AssignmentContext (a, c) -> nuriContext c (sfAssignment a [] e)
        | SchemaContext (s, c)     -> nuriContext c (nuriSchema s e)
        | EnumContext (enum, c)    -> nuriContext c (nuriEnum enum e)
        | TrajectoryContext (t, c) -> nuriContext c (nuriTrajectory t e)
        | EmptyContext             -> e

and nuriSpecification ?main:(mainReference=["main"]) nuri =
    let e1 = nuriContext nuri [] in
    if mainReference = [] then
        let e2 = second_pass_eval e1 mainReference in
        map_of e2
    else if not (domain e1 mainReference) then
        error 429 ("Object '" ^ (!^mainReference) ^ "' is not found.")
    else
        let e2 = get_main (second_pass_eval e1 mainReference) mainReference in
        let e_main = assign e2 ["global"] T_Undefined T_Constraint in
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
        let actions = values_of T_Action map in
        let add_effect_values =
            List.fold_left (
                fun acc (r, v) ->
                    match v with
                    | Domain.Boolean _ ->
                        add_value T_Bool (Domain.Basic v) acc
                    | Domain.Int     _ ->
                        add_value T_Int (Domain.Basic v) acc
                    | Domain.Float   _ ->
                        add_value T_Float (Domain.Basic v) acc
                    | Domain.String  _ ->
                        add_value T_String (Domain.Basic v) acc
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
        let map = add_value (T_Object ts) value map in
        let map = add_value (T_Reference ts) value map in
        let map = add_value (T_Reference ts) null map in
        match ts with
        | T_PlainObject -> add_value (T_Object T_PlainObject) value map
        | T_Schema (_, super) -> add_object super value map
        | _ -> map
    in
    let add_store_values typeEnv =
        MapRef.fold (
            fun r v (map: type_values) ->
                match type_of r typeEnv with
                | T_Undefined -> error 432 ("Type of " ^ !^r ^ " is undefined.")
                | T_Object ts when (T_Object ts) <: (T_Object T_PlainObject) ->
                    add_object ts (Domain.Basic (Domain.Reference r)) map
                | T_Object _ -> map
                | t -> add_value t v map
        )
    in
    let map0 = add_store_values typeEnvInit flatStoreInit MapType.empty in
    let map1 = add_action_values typeEnvInit map0 in
    let map2 = add_store_values typeEnvGoal flatStoreGoal map1 in
    let map3 = add_action_values typeEnvGoal map2 in
    MapType.fold (fun (t: Syntax.t) (values: Domain.SetValue.t) (map: type_values) ->
        match t with
        | T_Enum (id, symbols) ->
            (
                List.fold_left (fun map symbol ->
                    let v : Domain.value = Domain.Basic (Domain.Reference [id; symbol]) in
                    add_value t v map
                ) map symbols
            )
        | _ -> map
    ) map3 map3
;;
