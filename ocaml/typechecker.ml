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
  | id :: [], (T_Symbol enumID) when symbol_of_enum enumID typeEnv id ->
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

(* TODO: refactor (try to remove 'isFirst') *)
and nuri_prototype ?isFirst:(isFirst=true) prototype t_explicit destRef
                  namespace typeEnv : environment =
  match prototype with
  | EmptyPrototype when isFirst ->
    bind t_explicit t_plain_object destRef typeEnv

  | EmptyPrototype -> typeEnv

  | BlockPrototype (block, nextProto) ->
    begin
      let t_block = match isFirst, t_explicit with
        | true, T_Undefined -> t_plain_object
        | _ -> t_explicit
      in
      let env1 = bind t_explicit t_block destRef typeEnv in
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
          let env1 = bind t_explicit t destRef typeEnv in
          let env2 = _inherit protoRef destRef namespace env1 in
          let tx = if t_explicit = T_Undefined then t else t_explicit in
          nuri_prototype ~isFirst:false nextProto tx destRef namespace env2
        end
    end

and nuri_constraints constraints namespace typeEnv = match constraints with
  | Global global -> nuri_global global namespace typeEnv

and nuri_global global namespace typeEnv : environment =
  bind T_Undefined T_Constraint reference_of_global typeEnv

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

and nuri_not_expression t_operand =
  if t_operand <: T_Bool then T_Bool
  else error 1726 "Operand of '!' is not a boolean."

and nuri_match_regexp_expression t_operand =
  if t_operand <: T_String then T_Bool
  else error 1727 "Left operand of '=~' is not a string."

and nuri_equality_expression operator t_left t_right =
  if t_left <: t_right || t_right <: t_left then T_Bool
  else error 1728 ("Types of (" ^ operator ^ ") operands are not compatible.")

and nuri_logic_expression operator t_left t_right =
  if t_left <: T_Bool && t_right <: T_Bool then T_Bool
  else error 1729 ("Types of (" ^ operator ^ ") operands are not boolean.")

and nuri_math_expression operator t_left t_right = match t_left, t_right with
  | T_Int, T_Float | T_Float, T_Int | T_Float, T_Float -> T_Float
  | T_Int, T_Int -> T_Int
  | _ -> error 1730 ("Types of (" ^ operator ^ ") operands are not integer " ^
                    "or float.")

and nuri_add_expression t_left t_right = match t_left, t_right with
  | T_String, _ | _, T_String -> T_String
  | _ ->
    begin try
      nuri_math_expression "+" t_left t_right
    with
      Type.Error _ ->
        error 1740 "Types of (+) operands are not integer, float, or string."
    end

and nuri_expression expression t_explicit namespace typeEnv : t =
  let unary = nuri_unary_expression t_explicit namespace typeEnv in
  let binary = nuri_binary_expression t_explicit namespace typeEnv in
  match expression with
  | Basic value -> nuri_basic_value value t_explicit namespace typeEnv
  | Shell _ | Exp_IString _ -> T_String
  | Exp_Eager exp -> unary "$" exp (fun t -> t)
  | Exp_Not exp -> unary "!" exp nuri_not_expression
  | Exp_MatchRegexp (exp, _) -> unary "=~" exp nuri_match_regexp_expression
  | Exp_Equal (leftExp, rightExp) ->
    binary "==" leftExp rightExp (nuri_equality_expression "==")

  | Exp_NotEqual (leftExp, rightExp) ->
    binary "!=" leftExp rightExp (nuri_equality_expression "!=")

  | Exp_And (leftExp, rightExp) ->
    binary "&&" leftExp rightExp (nuri_logic_expression "&&")

  | Exp_Or (leftExp, rightExp) ->
    binary "||" leftExp rightExp (nuri_logic_expression "||")

  | Exp_Imply (leftExp, rightExp) ->
    binary "=>" leftExp rightExp (nuri_logic_expression "=>")

  | Exp_Add (leftExp, rightExp) ->
    binary "+" leftExp rightExp nuri_add_expression

  | Exp_Subtract (leftExp, rightExp) ->
    binary "-" leftExp rightExp (nuri_math_expression "-")

  | Exp_Multiply (leftExp, rightExp) ->
    binary "*" leftExp rightExp (nuri_math_expression "*")

  | Exp_Divide (leftExp, rightExp) ->
    binary "/" leftExp rightExp (nuri_math_expression "/")

  | Exp_Modulo (leftExp, rightExp) ->
    binary "%" leftExp rightExp (nuri_math_expression "%")

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

and nuri_value value t_explicit destRef namespace typeEnv : environment =
  match value with
  | TBD | Unknown | None -> bind t_explicit T_Any destRef typeEnv
  | Action _ -> bind t_explicit T_Action destRef typeEnv
  | Link linkRef ->
    begin
      let (srcRef, t) =
        nuri_link_reference linkRef destRef namespace typeEnv
      in
      let env = bind t_explicit t destRef typeEnv in
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
          let env1 = bind t_explicit t destRef typeEnv in
          let env2 = _inherit schemaRef destRef [] env1 in
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
      bind t_explicit t_value destRef typeEnv
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

and nuri_assignment (destRef, t, value) namespace typeEnv =
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
  
      | Domain.Valid ref -> nuri_value value t_explicit ref namespace typeEnv
    end

and nuri_block block namespace typeEnv : environment = match block with
  | AssignmentBlock (assignment, block) ->
    nuri_block block namespace (nuri_assignment assignment namespace typeEnv)

  | TrajectoryBlock (constraints, block) ->
    nuri_block block namespace (nuri_constraints constraints namespace typeEnv)

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
      bind T_Undefined (T_Schema (T_User (name, t_parent))) schemaRef typeEnv

    | _ -> error ~env:typeEnv 1766 ("'" ^ name ^ "' is bound multiple times.")
  in
  let env2 = if parentRef = [] then env1
             else _inherit parentRef schemaRef [] env1
  in
  nuri_block block schemaRef env2

and nuri_enum (name, symbols) typeEnv : environment =
  let enumRef = [name] in
  match enumRef @: typeEnv with
  | T_Undefined ->
    begin
      let env = (enumRef, T_Enum (name, symbols)) :: typeEnv in
      List.fold_left (fun env symbol ->
        ([name; symbol], (T_Symbol name)) :: env
      ) env symbols
    end
  | _ -> error ~env:typeEnv 1770 ("'" ^ name ^ "' is bound multiple times.")

and nuri_context context typeEnv = match context with
  | AssignmentContext (assignment, nextContext) ->
    nuri_context nextContext (nuri_assignment assignment [] typeEnv)

  | SchemaContext (schema, nextContext) ->
    nuri_context nextContext (nuri_schema schema typeEnv)

  | EnumContext (enum, nextContext) ->
    nuri_context nextContext (nuri_enum enum typeEnv)

  | TrajectoryContext (constraints, nextContext) ->
    nuri_context nextContext (nuri_constraints constraints [] typeEnv)

  | EmptyContext -> typeEnv

and nuri_specification ?main:(mainReference=["main"]) nuri =
  (* first-pass *)
  let env1 = nuri_context nuri initial_environment in

  (* second-pass *)
  let env2 = match mainReference @: env1 with
    | T_Undefined ->
      error ~env:env1
            1775
            ("Main variable (" ^ !^mainReference ^ ") is not exist.")

    | t when t <: t_plain_object -> replace_forward_type mainReference env1

    | _ ->
      error ~env:env1
            1780
            ("Main variable (" ^ !^mainReference ^ ") is not an object.")
  in

  (* third-pass *)
  let env3 = if mainReference = [] then env2
             else main_of mainReference env2
  in

  if not (well_formed (map_of env2) (map_of env3)) then ();

  (* forth-pass *)
  let env4 = bind T_Undefined T_Constraint reference_of_global env3 in
  map_of env4
;;

let eval = nuri_specification ;;
