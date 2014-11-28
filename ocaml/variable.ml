(* Author: Herry (herry13@gmail.com) *)

open Common
open Domain

(** variable := name * index * values * init * goal **)
type t = {
    name   : reference;
    index  : int;
    values : value array;
    init   : value;
    goal   : value
}

(** collection of variables **)
type ts = {
    map : t MapRef.t;
    arr : t array
}


(*****************************************************************
 * variable functions
 *****************************************************************)

let make r i vals init goal = {
        name   = r;
        index  = i;
        values = vals;
        init   = init;
        goal   = goal
    }
;;

let iteri_values func var = Array.iteri func var.values ;;

let string_of_values =
    Array.fold_left (
        fun acc v -> acc ^ (Json.of_value v) ^ ";"
    ) ""
;;

let string_of_variable var =
    !^(var.name) ^ "|" ^ string_of_int(var.index) ^ "|" ^
    (string_of_values var.values) ^ "|" ^ (Json.of_value var.init) ^ "|" ^
    (Json.of_value var.goal)
;;

let r_dummy = ["!global"] ;;

let dummy = {
        name   = r_dummy;
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

(*****************************************************************
 * variables functions
 *****************************************************************)

let sort variables =
    Array.fast_sort (
        fun v1 v2 -> v1.index - v2.index
    ) variables.arr
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
    let intersectionValues = Array.fold_left (fun acc value ->
            match value with
            | Basic v -> if List.mem v vector then value :: acc
                         else acc
            | _       -> value :: acc
        ) [] variable.values
    in
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
    let selectedValues = Array.fold_left (fun acc value ->
            match value with
            | Basic v -> if List.mem v vector then acc else value :: acc
            | _       -> value :: acc
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

let string_of_variables variables =
    Array.fold_left (
        fun acc variable -> (string_of_variable variable) ^ "\n" ^ acc
    ) "" variables.arr
;;

(** a temporary type which is used to generate a collection of variables **)
type temp_variables = {
    _map               : t MapRef.t;
    _list             : t list;
    nextVariableIndex : int
}

(** generate a collection of variables from given type environment and
    flat-store of initial and goal states **)
let make_ts typeEnvInit flatStoreInit typeEnvGoal flatStoreGoal typeValues =
    let typeObject = Syntax.T_Schema Syntax.T_Object in
    let type_of_variable ref =
        match (Type.type_of ref typeEnvInit), (Type.type_of ref typeEnvGoal) with
        | type1, type2 when type1 = type2 ->
            (* only basic-value that can have TBD value at the goal state *)
            if (Type.subtype type1 typeObject) && (MapRef.find ref flatStoreGoal) = TBD then
                error 601 ("Only variable with type bool, int, float, string, and ref (*) " ^
                           "that can have TBD value at the goal state.")
            else
                type1
        | type1, Syntax.T_Undefined -> type1
        | _, _ -> error 602 "incompatible type between initial & goal states"
    in
    let accumulator = {
            _map              = MapRef.add r_dummy dummy MapRef.empty;
            _list             = [dummy];
            nextVariableIndex = 1
        }
    in
    let result = MapRef.fold (
            fun r v acc ->
                if MapRef.mem r acc._map then error 603 "";
                match type_of_variable r with
                | Syntax.T_Action    (* action and global constraints are skipped *)
                | Syntax.T_Global -> acc
                | t when Type.subtype t typeObject ->
                    let value = static_object in
                    let variable = make r acc.nextVariableIndex [|value|] value value in
                    {
                        _map              = MapRef.add r variable acc._map;
                        _list             = variable :: acc._list;
                        nextVariableIndex = acc.nextVariableIndex + 1
                    }
                | t ->
                    let values = Array.of_list (SetValue.elements
                        (Type.values_of t typeValues))
                    in
                    let init = MapRef.find r flatStoreInit in
                    let goal =
                        if MapRef.mem r flatStoreGoal then MapRef.find r flatStoreGoal
                        else TBD
                    in
                    let variable = make r acc.nextVariableIndex values init goal in
                    {
                        _map              = MapRef.add r variable acc._map;
                        _list             = variable :: acc._list;
                        nextVariableIndex = acc.nextVariableIndex + 1
                    }
        ) flatStoreInit accumulator
    in
    let variables = {
            map = result._map;
            arr = (Array.of_list result._list)
        }
    in
    sort variables;
    variables
;;
