(* Author: Herry (herry13@gmail.com) *)

open Common
open Domain

(** variable := name * index * values * init * goal *)
type t =
{
  name   : reference;
  index  : int;
  values : value array;
  init   : value;
  goal   : value
}

(** collection of variables *)
type ts = {
  map : t MapRef.t;
  arr : t array
}

exception Error of int * string

let error code message =
  raise (Error (code, ("[err" ^ (string_of_int code) ^ "] ") ^ message))
;;

(*****************************************************************
 * Variable operation functions
 *****************************************************************)

let make name index values initValue goalValue =
  {
    name   = name;
    index  = index;
    values = values;
    init   = initValue;
    goal   = goalValue
  }
;;

let iteri_values func variable = Array.iteri func variable.values ;;

let string_of_variable variable =
  let buf = Buffer.create 40 in
  buf <<| !^(variable.name) <.| '|' <<| (string_of_int variable.index) <. '|';
  Array.iter (fun v -> buf <<| (Nuri_json.of_value v) <. ';') variable.values;
  buf <.| '|' <<| (Nuri_json.of_value variable.init) <.| '|'
      <<$ (Nuri_json.of_value variable.goal);
;;

let reference_of_dummy = ["!global"] ;;

let dummy =
  {
    name   = reference_of_dummy;
    index  = 0;
    values = [| Basic (Boolean true); Basic (Boolean false) |];
    init   = Basic (Boolean false);
    goal   = Basic (Boolean true)
  }
;;

let index variable = variable.index ;;

let name variable = variable.name ;;

let init variable = variable.init ;;

let goal variable = variable.goal ;;

let values variable = variable.values ;;

let size variable = Array.length variable.values ;;

let index_of_value value variable =
  let len = Array.length variable.values in
  let rec iter index =
    if index >= len then -1
    else if variable.values.(index) = value then index
    else iter (index + 1)
  in
  iter 0
;;

let intersect values variable =
  let foreach_value accumulator value =
    if List.mem value values then value :: accumulator
    else accumulator
  in
  let newValues = Array.fold_left foreach_value [] variable.values in
  {
    name   = variable.name;
    index  = variable.index;
    values = Array.of_list newValues;
    init   = variable.init;
    goal   = variable.goal
  }
;;


(*****************************************************************
 * Collection of variables operation functions
 *****************************************************************)

let sort variables =
  Array.fast_sort (fun v1 v2 -> v1.index - v2.index) variables.arr
;;

let mem reference variables = MapRef.mem reference variables.map ;;

let find reference variables = MapRef.find reference variables.map ;;

let values_of reference variables =
  if MapRef.mem reference variables.map then
    (MapRef.find reference variables.map).values
  else
    [| |]
;;
    
let total variables = Array.length variables.arr ;;

let iter func variables = Array.iter func variables.arr ;;

let intersection_with_values reference vector variables =
  let variable = find reference variables in
  let filter accumulator value = match value with
    | Basic v -> if List.mem v vector then value :: accumulator
                 else accumulator
    | _       -> (* value :: *) accumulator
  in
  let intersectionValues = Array.fold_left filter [] variable.values in
  let variable1 = {
      name   = variable.name;
      index  = variable.index;
      values = Array.of_list intersectionValues;
      init   = variable.init;
      goal   = variable.goal
    }
  in
  variables.arr.(variable1.index) <- variable1;
  {
    map = MapRef.add reference variable1 variables.map;
    arr = variables.arr
  }
;;

let intersection_with_value reference value variables =
  let variable = find reference variables in
  let len = Array.length variable.values in
  let rec exists index =
    if index >= len then false
    else if variable.values.(index) = value then true
    else exists (index + 1)
  in
  let intersectionValues =
    if exists 0 then [| value |]
    else [| |]
  in
  let variable1 = {
      name   = variable.name;
      index  = variable.index;
      values = intersectionValues;
      init   = variable.init;
      goal   = variable.goal
    }
  in
  variables.arr.(variable1.index) <- variable1;
  {
    map = MapRef.add reference variable1 variables.map;
    arr = variables.arr
  }
;;

let intersect_ts values varName variables =
  if mem varName variables then
    let var = intersect values (find varName variables) in
    variables.arr.(var.index) <- var;
    {
      map = MapRef.add varName var variables.map;
      arr = variables.arr
    }
  else
    variables
;;

let remove_value_from reference value variables =
  let variable = find reference variables in
  let selectedValues = Array.fold_left (
      fun acc v -> if v = value then acc else v :: acc
    ) [] variable.values
  in
  let newVariable = {
      name   = variable.name;
      index  = variable.index;
      values = Array.of_list selectedValues;
      init   = variable.init;
      goal   = variable.goal
    }
  in
  variables.arr.(newVariable.index) <- newVariable;
  {
    map = MapRef.add reference newVariable variables.map;
    arr = variables.arr
  }
;;

let remove_values_from reference vector variables =
  let variable = find reference variables in
  let filter accumulator value = match value with
    | Basic v -> if List.mem v vector then accumulator
                 else value :: accumulator
    | _       -> value :: accumulator
  in
  let selectedValues = Array.fold_left filter [] variable.values in
  let newVariable = {
      name   = variable.name;
      index  = variable.index;
      values = Array.of_list selectedValues;
      init   = variable.init;
      goal   = variable.goal
    }
  in
  variables.arr.(newVariable.index) <- newVariable;
  {
    map = MapRef.add reference newVariable variables.map;
    arr = variables.arr
  }
;;

let string_of_variables variables =
  let buf = Buffer.create 40 in
  Array.iter (fun var ->
    buf <<| (string_of_variable var) <. '\n'
  ) variables.arr;
  Buffer.contents buf
;;

(** a temporary type which is used to generate a collection of variables *)
type temp_variables = {
  _map              : t MapRef.t;
  _list             : t list;
  nextVariableIndex : int
}

let static_object_domain = [| static_object |] ;;

(** generate a collection of variables from given type environment and
    flat-store of initial and goal states *)
let make_ts typeEnvInit flatStoreInit typeEnvGoal flatStoreGoal typeValues =
  let type_of_variable varName =
    match (Type.type_of varName typeEnvInit),
          (Type.type_of varName typeEnvGoal)
    with
    | t, Syntax.T_Undefined -> t

    | (Syntax.T_Object t1) as t, Syntax.T_Object t2 when t1 = t2 ->
      begin
        if MapRef.find varName flatStoreGoal = TBD then
          error 601 ("Non-primitive variable '" ^ !^varName ^ "' " ^
                     "cannot have TBD value at the goal state.")
        else
          t
      end

    | _ -> error 602 ("The types of '" ^ !^varName ^ "' at initial " ^
                      "and goal states are incompatible.")
  in
  let accumulator =
    {
      _map = MapRef.add reference_of_dummy dummy MapRef.empty;
      _list = [dummy];
      nextVariableIndex = 1
    }
  in
  let values_of_type t =
    Array.of_list (SetValue.elements (Type.values_of t typeValues))
  in
  let result = MapRef.fold (fun varName value acc ->
    if MapRef.mem varName acc._map then
      error 603 "Duplicate variable is detected."
    else
      begin match type_of_variable varName with
      | Syntax.T_Action | Syntax.T_Constraint ->
        (* action and global constraints are skipped *)
        acc

      | Syntax.T_Object _ ->
        begin
          let variable = make varName
                              acc.nextVariableIndex
                              static_object_domain
                              static_object
                              static_object
          in
          {
            _map = MapRef.add varName variable acc._map;
            _list = variable :: acc._list;
            nextVariableIndex = acc.nextVariableIndex + 1
          }
        end

      | t ->
        begin
          let values = values_of_type t in
          let initValue = MapRef.find varName flatStoreInit in
          let goalValue = if not (MapRef.mem varName flatStoreGoal) then TBD
                          else MapRef.find varName flatStoreGoal
          in
          let variable = make varName
                              acc.nextVariableIndex
                              values
                              initValue
                              goalValue
          in
          {
            _map = MapRef.add varName variable acc._map;
            _list = variable :: acc._list;
            nextVariableIndex = acc.nextVariableIndex + 1
          }
        end
      end
    ) flatStoreInit accumulator
  in
  let variables =
    {
      map = result._map;
      arr = (Array.of_list result._list)
    }
  in
  sort variables;
  variables
;;
