(** Module Typechecker contains the type-checking function.

    Module dependencies:
    - Common
    - Syntax
    - Type
    - Domain

    @author Herry (herry13\@gmail.com)
    @since 2014
*)

open Common
open Syntax
open Type


(*******************************************************************
 * Helper functions
 *******************************************************************)

let (!-)  = Domain.(!-) ;;
let (@<<) = Domain.(@<<) ;;
let (@+)  = Domain.(@+) ;;
let (@+.) = Domain.(@+.) ;;
let (@<=) = Domain.(@<=) ;;

let t_plain_object = T_Object T_Plain ;;


let global_constraints : (environment -> t) list ref = ref [] ;;

let add_global c = global_constraints := c :: !global_constraints ;;

(*******************************************************************
 * Type evaluation functions
 *******************************************************************)

let rec nuri_boolean b = T_Bool

and nuri_int i = T_Int

and nuri_float f = T_Float

and nuri_string s = T_String

and nuri_null = T_Null

and nuri_reference r = r

and nuri_data_reference dataRef t_explicit namespace typeEnv : t =
  match dataRef, t_explicit with
  | id :: [], (T_Symbol enumID) when symbol_of_enum id enumID typeEnv ->
    t_explicit

  | _ ->
    begin
      let ref = nuri_reference dataRef in
      match resolve ref namespace typeEnv with
      | _, T_Undefined -> T_Forward (T_Ref ref)
      | _, T_Schema _ ->
        error ~env:typeEnv 1700 ("Cannot refer to a schema: " ^ !^ref)
    
      | _, T_Action ->
        error ~env:typeEnv 1701 ("Cannot refer to an action: " ^ !^ref)
    
      | _, T_Constraint ->
        error ~env:typeEnv 1702 ("Cannot refer to a constraint: " ^ !^ref)
    
      | _, T_Enum _ ->
        error ~env:typeEnv 1703 ("Cannot refer to an enum: " ^ !^ref)
    
      | _, T_Object (_ as t_object) -> T_Reference t_object
      | _, t -> t
    end

and nuri_link_reference linkReference destReference namespace typeEnv =
  let ref = nuri_reference linkReference in
  match resolve ref namespace typeEnv with
  | _, T_Undefined -> (!-destReference, T_Forward (T_Link ref))
  | srcRef, _ when srcRef @<= destReference ->
    error ~env:typeEnv
          1705
          ("Inner-cyclic reference: " ^ !^srcRef ^ " <= " ^ !^destReference)

  | result -> result

and nuri_array _array t_explicit namespace typeEnv : t =
  let t = match t_explicit with
    | T_List tl -> tl
    | _         -> t_explicit
  in
  let rec eval = function
    | [] -> T_Undefined
    | head :: [] -> nuri_basic_value head t namespace typeEnv
    | head :: tail ->
      begin match nuri_basic_value head t namespace typeEnv, eval tail with
      | T_Forward _, _ | _, T_Forward _ -> T_Undefined
      | t_head, t_tail when t_head =:= t_tail -> t_head
      | t_head, t_tail ->
        error ~env:typeEnv
              1710
              ("Type of array elements are different e.g. " ^
                (string_of_type t_head) ^ " and " ^
                (string_of_type t_tail) ^ ".")
      end
  in
  match eval _array with
  | T_Undefined -> T_Undefined
  | t           -> T_List t

and nuri_basic_value basicValue t_explicit namespace typeEnv : t =
  match basicValue with
  | Boolean b     -> nuri_boolean b
  | Int i         -> nuri_int i
  | Float f       -> nuri_float f
  | String s      -> nuri_string s
  | Null          -> nuri_null
  | Vector vector -> nuri_array vector t_explicit namespace typeEnv
  | Reference ref -> nuri_data_reference ref t_explicit namespace typeEnv
  | RefIndex (ref, indexes) ->
    begin
      let t_ref = nuri_data_reference ref t_explicit namespace typeEnv in
      at ~env:typeEnv indexes t_ref
    end

(* TODO: refactor (try to remove 'isFirst') *)
and nuri_prototype ?isFirst:(isFirst=true) prototype t_explicit destRef
                  namespace typeEnv : environment =
  match prototype with
  | EmptyPrototype when isFirst ->
    let env = bind typeEnv destRef t_explicit t_plain_object in
    bind env (destRef @+. "name") T_Undefined T_String

  | EmptyPrototype -> bind typeEnv (destRef @+. "name") T_Undefined T_String

  | BlockPrototype (block, nextProto) ->
    begin
      let t_block = match isFirst, t_explicit with
        | true, T_Undefined -> t_plain_object
        | _ -> t_explicit
      in
      let env1 = bind typeEnv destRef t_explicit t_block in
      let env2 = nuri_block block destRef env1 in
      nuri_prototype ~isFirst:false nextProto t_block destRef namespace env2
    end

  | ReferencePrototype (reference, nextProto) ->
    begin
      let protoRef = nuri_reference reference in
      match resolve protoRef namespace typeEnv with
      | _, T_Undefined ->
        error ~env:typeEnv
              1715
              ("Forward prototype is not supported: " ^ !^protoRef)

      | _, t ->
        begin
          let env1 = bind typeEnv destRef t_explicit t in
          let env2 = inherit_ protoRef destRef namespace env1 in
          let tx = if t_explicit = T_Undefined then t else t_explicit in
          nuri_prototype ~isFirst:false nextProto tx destRef namespace env2
        end
    end

and nuri_trajectory constraints namespace typeEnv = match constraints with
  | Global global ->
    add_global (nuri_constraint global namespace); typeEnv

(* TODO: this must be implemented after generating the main map *)
and nuri_constraint constraint_ namespace : environment -> t =
  match constraint_ with
  | C_Equal (left, right) ->
    eval_c_binary (nuri_compare "eq") left right namespace

  | C_NotEqual (left, right) ->
    eval_c_binary (nuri_compare "ne") left right namespace

  | C_Greater (left, right) ->
    eval_c_binary (nuri_num_compare "gt") left right namespace

  | C_GreaterEqual (left, right) ->
    eval_c_binary (nuri_num_compare "ge") left right namespace

  | C_Less (left, right) ->
    eval_c_binary (nuri_num_compare "lt") left right namespace

  | C_LessEqual (left, right) ->
    eval_c_binary (nuri_num_compare "le") left right namespace

  | C_In (left, right) ->
    let comparator t_left = function
      | T_List t when t_left <: t -> T_Bool
      | T_List t ->
        error 1717 "Right operand (in) is not subtype of left operand."

      | _ -> error 1718 "Right operand (in) is not an array."
    in
    eval_c_binary comparator left right namespace

  | C_Not c ->
    fun env -> eval_c_clauses "not" [c] namespace env

  | C_Imply (premise, conclusion) ->
    fun env -> eval_c_clauses "imply" [premise; conclusion] namespace env

  | C_And [] | C_Or [] -> fun _ -> T_Bool

  | C_And clauses ->
    fun env -> eval_c_clauses "and" clauses namespace env

  | C_Or clauses ->
    fun env -> eval_c_clauses "or" clauses namespace env

and eval_c_binary comparator left right namespace typeEnv =
  let t_left : t = nuri_basic_value left T_Undefined namespace typeEnv in
  let t_right : t = nuri_basic_value right T_Undefined namespace typeEnv in
  comparator t_left t_right

and eval_c_clauses operator clauses namespace typeEnv =
  if List.for_all (fun c -> (nuri_constraint c namespace typeEnv) <: T_Bool)
                  clauses
  then
    T_Bool
  else
    error 1719 ("Clause(s) of '" ^ operator ^ "' is not boolean.")

(** Ensure that the operand's type is determinate. *)
and nuri_unary_expression t_explicit namespace typeEnv operator expression
                          callback : t =
  match nuri_expression expression t_explicit namespace typeEnv with
  | T_Forward _ ->
    error ~env:typeEnv
          1720
          ("Type of (" ^ operator ^ ") operand is indeterminate.")

  | T_Undefined ->
    error ~env:typeEnv
          1721
          ("Type of (" ^ operator ^ ") operand is undefined.")

  | t -> callback t

(** Ensure that the types of both operands are determinate. *)
and nuri_binary_expression t_explicit namespace typeEnv operator
                           leftExpression rightExpression callback : t =
  match nuri_expression leftExpression t_explicit namespace typeEnv,
        nuri_expression rightExpression t_explicit namespace typeEnv
  with
  | T_Forward _, _ ->
    error ~env:typeEnv
          1722
          ("Type of left (" ^ operator ^ ") operand is indeterminate.")

  | _, T_Forward _ ->
    error ~env:typeEnv
          1723
          ("Type of right (" ^ operator ^ ") operand is indeterminate.")

  | T_Undefined, _ ->
    error ~env:typeEnv
          1724
          ("Type of left (" ^ operator ^ ") operand is undefined.")

  | _, T_Undefined ->
    error ~env:typeEnv
          1725
          ("Type of right (" ^ operator ^ ") operand is undefined.")

  | t_left, t_right -> callback t_left t_right

and nuri_negation t_operand =
  if t_operand <: T_Bool then T_Bool
  else error 1726 "Operand of '!' is not a boolean."

and nuri_pattern_matching t_operand =
  if t_operand <: T_String then T_Bool
  else error 1727 "Left operand of '=~' is not a string."

and nuri_compare operator (t_left : t) (t_right : t) : t =
  if t_left <: t_right || t_right <: t_left then T_Bool
  else error 1728 ("Types of (" ^ operator ^ ") operands are not compatible.")

and nuri_logic operator t_left t_right =
  if t_left <: T_Bool && t_right <: T_Bool then T_Bool
  else error 1729 ("Types of (" ^ operator ^ ") operands are not boolean.")

and nuri_math operator t_left t_right = match t_left, t_right with
  | T_Int, T_Float | T_Float, T_Int | T_Float, T_Float -> T_Float
  | T_Int, T_Int -> T_Int
  | _ -> error 1730 ("Types of (" ^ operator ^ ") operands are not integer " ^
                    "or float.")

and nuri_num_compare operator t_left t_right = match t_left, t_right with
  | T_Int, T_Float   | T_Float, T_Int
  | T_Float, T_Float | T_Int, T_Int -> T_Bool
  | _ -> error 1731 ("Types of (" ^ operator ^ ") operands are not integer " ^
                    "or float.")

and nuri_add_expression t_left t_right = match t_left, t_right with
  | T_String, _ | _, T_String -> T_String
  | _ ->
    begin try
      nuri_math "+" t_left t_right
    with
      Type.Error _ ->
        error 1740 "Types of (+) operands are not integer, float, or string."
    end

and nuri_expression expression t_explicit namespace typeEnv : t =
  let unary = nuri_unary_expression t_explicit namespace typeEnv in
  let binary = nuri_binary_expression t_explicit namespace typeEnv in
  match expression with
  | Basic value -> nuri_basic_value value t_explicit namespace typeEnv
  | Exp_Index (exp, indexes) ->
    begin
      let t_exp = nuri_expression exp t_explicit namespace typeEnv in
      at ~env:typeEnv indexes t_exp
    end

  | Shell _ | Exp_IString _ -> T_String
  | Exp_Eager exp -> unary "$" exp (fun t -> t)
  | Exp_Not exp -> unary "!" exp nuri_negation
  | Exp_MatchRegexp (exp, _) -> unary "=~" exp nuri_pattern_matching
  | Exp_Equal (leftExp, rightExp) ->
    binary "==" leftExp rightExp (nuri_compare "==")

  | Exp_NotEqual (leftExp, rightExp) ->
    binary "!=" leftExp rightExp (nuri_compare "!=")

  | Exp_And (leftExp, rightExp) ->
    binary "&&" leftExp rightExp (nuri_logic "&&")

  | Exp_Or (leftExp, rightExp) ->
    binary "||" leftExp rightExp (nuri_logic "||")

  | Exp_Imply (leftExp, rightExp) ->
    binary "=>" leftExp rightExp (nuri_logic "=>")

  | Exp_Add (leftExp, rightExp) ->
    binary "+" leftExp rightExp nuri_add_expression

  | Exp_Subtract (leftExp, rightExp) ->
    binary "-" leftExp rightExp (nuri_math "-")

  | Exp_Multiply (leftExp, rightExp) ->
    binary "*" leftExp rightExp (nuri_math "*")

  | Exp_Divide (leftExp, rightExp) ->
    binary "/" leftExp rightExp (nuri_math "/")

  | Exp_Modulo (leftExp, rightExp) ->
    binary "%" leftExp rightExp (nuri_math "%")

  | Exp_IfThenElse (expIf, expThen, expElse) ->
    begin match nuri_expression expIf t_explicit namespace typeEnv,
                nuri_expression expThen t_explicit namespace typeEnv,
                nuri_expression expElse t_explicit namespace typeEnv
    with
    | T_Bool, T_Forward _, _ ->
      error ~env:typeEnv 1745 "Type of 'then' expression is indeterminate."

    | T_Bool, _, T_Forward _ ->
      error ~env:typeEnv 1746 "Type of 'else' expression is indeterminate."

    | T_Bool, T_Undefined, _ ->
      error ~env:typeEnv 1747 "Type of 'then' expression is undefined."

    | T_Bool, _, T_Undefined ->
      error ~env:typeEnv 1748 "Type of 'else' expression is undefined."

    | T_Bool, t_then, t_else when t_then =:= t_else -> t_then

    | T_Bool, _, _ ->
      error ~env:typeEnv
            1749
            "Types of 'then' and 'else' clauses are different."

    | _ -> error ~env:typeEnv 1750 "Type of 'if' expression is not a boolean."
    end

and nuri_value value t_variable t_explicit destRef namespace typeEnv =
  match value with
  | TBD | Unknown | None ->
    bind typeEnv destRef ~t_variable:t_variable t_explicit T_Any

  | Action _ ->
    bind typeEnv destRef ~t_variable:t_variable t_explicit T_Action

  | Link linkRef ->
    begin
      let (srcRef, t) =
        nuri_link_reference linkRef destRef namespace typeEnv
      in
      let env = bind typeEnv destRef ~t_variable:t_variable t_explicit t in
      if t <: t_plain_object then copy srcRef destRef env
      else env
    end

  | Prototype (EmptySchema, prototype) ->
    nuri_prototype prototype T_Undefined destRef namespace typeEnv

  | Prototype (SID schemaIdentifier, prototype) ->
    begin
      let schemaRef = [schemaIdentifier] in
      match schemaRef @: typeEnv with
      | T_Undefined ->
        error ~env:typeEnv
              1755
              ("Forward schema is not supported: " ^ schemaIdentifier)

      | T_Schema t_object ->
        begin
          let t = T_Object t_object in
          let env1 =
            bind typeEnv destRef ~t_variable:t_variable t_explicit t
          in
          let env2 = inherit_ schemaRef destRef [] env1 in
          nuri_prototype prototype t destRef namespace env2
        end

      | _ -> error ~env:typeEnv 1756 ("Invalid schema: " ^ schemaIdentifier)
    end
  | Expression expression ->
    begin
      let t_exp = if t_explicit = T_Undefined then destRef @: typeEnv
                  else t_explicit
      in
      let t_value = nuri_expression expression t_exp namespace typeEnv in
      bind typeEnv destRef ~t_variable:t_variable t_explicit t_value
    end

and nuri_type_explicit t typeEnv = match t with
  | T_Object T_User (id, _) ->
    begin match [id] @: typeEnv with
    | T_Schema (_ as ts) -> T_Object ts
    | T_Enum _ -> T_Symbol id
    | _ -> error ~env:typeEnv 1757 ("Invalid type: " ^ id)
    end
  | T_List tl -> T_List (nuri_type_explicit tl typeEnv)
  | _ -> t

and nuri_assign_type_value destRef t value namespace typeEnv =
  if destRef = reference_of_echo then
    typeEnv
  else
    begin
      let t_explicit = nuri_type_explicit t typeEnv in
      match namespace @<< destRef with
      | Domain.Invalid ->
        error ~env:typeEnv
              1760
              ("Invalid reference: " ^ !^(namespace @+ destRef))
  
      | Domain.Valid ref ->
        nuri_value value T_Undefined t_explicit ref namespace typeEnv
    end

and nuri_assignment assignment namespace typeEnv = match assignment with
  | TypeValue (ref, t, value) ->
    nuri_assign_type_value ref t value namespace typeEnv

  | RefIndexValue (ref, indexes, value) ->
    begin
      let t_ref = nuri_data_reference ref T_Undefined namespace typeEnv in
      let t_element = at ~env:typeEnv indexes t_ref in
      nuri_value value t_element t_element ref namespace typeEnv
    end

and nuri_block block namespace typeEnv : environment = match block with
  | AssignmentBlock (assignment, block) ->
    nuri_block block namespace (nuri_assignment assignment namespace typeEnv)

  | TrajectoryBlock (trajectory, block) ->
    nuri_block block namespace (nuri_trajectory trajectory namespace typeEnv)

  | EmptyBlock -> typeEnv

and nuri_schema_parent parent typeEnv = match parent with
  | EmptySchema -> ([], T_Plain)
  | SID parentIdentifier ->
    begin
      let parentRef = [parentIdentifier] in
      match parentRef @: typeEnv with
      | T_Schema t -> (parentRef, t)
      | _ -> error ~env:typeEnv
                   1765
                   ("Invalid parent schema: " ^ parentIdentifier)
    end

and nuri_schema (name, parent, block) typeEnv =
  let (parentRef, t_parent) = nuri_schema_parent parent typeEnv in
  let schemaRef = [name] in
  let env1 = match schemaRef @: typeEnv with
    | T_Undefined ->
      bind typeEnv schemaRef T_Undefined (T_Schema (T_User (name, t_parent)))

    | _ -> error ~env:typeEnv 1766 ("'" ^ name ^ "' is bound multiple times.")
  in
  let env2 = if parentRef = [] then env1
             else inherit_ parentRef schemaRef [] env1
  in
  nuri_block block schemaRef env2

and nuri_enum (name, symbols) typeEnv : environment =
  let enumRef = [name] in
  match enumRef @: typeEnv with
  | T_Undefined ->
    begin
      let env = MapRef.add enumRef [T_Enum (name, symbols)] typeEnv in
      List.fold_left (fun accu symbol ->
        MapRef.add [name; symbol] [T_Symbol name] accu
      ) env symbols
    end
  | _ -> error ~env:typeEnv 1770 ("'" ^ name ^ "' is bound multiple times.")

and nuri_context context (typeEnv : environment) = match context with
  | AssignmentContext (assignment, nextContext) ->
    nuri_context nextContext (nuri_assignment assignment [] typeEnv)

  | SchemaContext (schema, nextContext) ->
    nuri_context nextContext (nuri_schema schema typeEnv)

  | EnumContext (enum, nextContext) ->
    nuri_context nextContext (nuri_enum enum typeEnv)

  | TrajectoryContext (trajectory, nextContext) ->
    nuri_context nextContext (nuri_trajectory trajectory [] typeEnv)

  | EmptyContext -> typeEnv

and nuri_specification ?main:(mainReference=["main"]) nuri =
  let initEnv = bind empty reference_of_global T_Undefined T_Constraint in

  (* first-pass *)
  let env1 = nuri_context nuri initEnv in

  (* second-pass *)
  let env2 = match mainReference @: env1 with
    | T_Undefined ->
      error ~env:env1
            1775
            ("Main variable (" ^ !^mainReference ^ ") is not exist.")

    | T_Object _ -> replace_forward_type mainReference env1

    | _ ->
      error ~env:env1
            1780
            ("Main variable (" ^ !^mainReference ^ ") is not an object.")
  in
  let mainEnv = if mainReference = [] then map_of env2
                else merge_types mainReference env2
  in

  if List.for_all (fun c -> (c env2) <: T_Bool) !global_constraints then ()
  else error 1781 "The type of global constraint(s) is not boolean.";

  (* third-pass *)
  well_formed (map_of env2) mainEnv
;;

let eval = nuri_specification ;;
