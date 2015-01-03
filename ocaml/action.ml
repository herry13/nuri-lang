(* Author: Herry (herry13@gmail.com) *)

open Common
open Domain

(**
 * This module represents a grounded action.
 *)

(** type of an action **)
type t =
{
  name          : reference;
  parameters    : basic MapStr.t;
  cost          : cost;
  preconditions : basic MapRef.t;
  effects       : basic MapRef.t
}

(** type of a collection of actions **)
type ts = { total: int; actions: t list }

let make name params cost pre eff =
  {
    name          = name;
    parameters    = params;
    cost          = cost;
    preconditions = pre;
    effects       = eff
  }
;;
  
let name a          = a.name;;
let parameters a    = a.parameters;;
let cost a          = a.cost;;
let preconditions a = a.preconditions;;
let effects a       = a.effects;;


(****************************************************************************
 * Functions to operate over a collection of actions (ts)
 ****************************************************************************)

let iter func ts = List.iter func ts.actions;;

let fold func accumulator ts =
  List.fold_left func accumulator ts.actions
;;

let add action ts =
  {
    total   = ts.total + 1;
    actions = action :: ts.actions
  }
;;

let empty_ts = { total = 0; actions = [] };;

let to_array ts = Array.of_list ts.actions;;


(****************************************************************************
 * Functions to generate JSON of actions.
 ****************************************************************************)

let json_of_parameters buffer parameters =
  buffer <. '{';
  let first = ref 1 in
  let each_parameter id value : unit =
    if id <> "this" && id <> "parent" then
      begin
        if !first = 0 then buffer <. ',';
        buffer <.| '"' <<| id <<| "\":" << (Nuri_json.of_value (Basic value));
        first := 0
      end
  in
  MapStr.iter each_parameter parameters;
  buffer <. '}'
;;

let json_of_preconditions buffer preconditions =
  buffer <. '{';
  let first = ref 1 in
  let each_precondition reference value =
    if !first = 0 then buffer <. ',';
    buffer <.| '"' <<| !^reference <<| "\":" << (Nuri_json.of_value (Basic value));
    first := 0
  in
  MapRef.iter each_precondition preconditions;
  buffer <. '}'
;;

let json_of_effects = json_of_preconditions;;

let json_of_elements buffer action =
  buffer <<| "\"name\":\"" <<| !^(action.name) << "\",\"parameters\":";
  json_of_parameters buffer action.parameters;
  buffer <<| ",\"cost\":"
         <<| (string_of_int action.cost)
         <<  ",\"conditions\":";
  json_of_preconditions buffer action.preconditions;
  buffer << ",\"effects\":";
  json_of_effects buffer action.effects
;;

let json_of_action buffer action =
  buffer <. '{';
  json_of_elements buffer action;
  buffer <. '}'
;;

let json_of action =
  let buffer = Buffer.create 42 in
  json_of_action buffer action;
  Buffer.contents buffer
;;

let to_string_buffer action buffer =
  buffer << !^(action.name);
  json_of_parameters buffer action.parameters
;;

(*let join buffer separator to_string vector =*)
let json_of_parallel_action action before after =
  let buffer = Buffer.create 42 in
  buffer <. '{';
  json_of_elements buffer action;
  buffer << ",\"before\":[";
  join buffer "," string_of_int before;
  buffer << "],\"after\":[";
  join buffer "," string_of_int after;
  buffer <<$ "]}"
;;

let json_of_actions actions =
  let buffer = Buffer.create 42 in
  buffer <. '[';
  dash buffer "," json_of_action actions; 
  buffer <.$ ']';
;;

(****************************************************************************
 * Functions to encode/decode actions' name. This will be used in FDR.
 ****************************************************************************)

let encode_name id a =
  let buf = Buffer.create 42 in
  buf <<| (string_of_int id) <<| " \"" <<| !^(a.name) << "\" ";
  json_of_parameters buf a.parameters;
  Buffer.contents buf
;;

(*
let decode_name (s: string): int * reference * basic MapStr.t =
  let rec iter_param (map: basic MapStr.t) ps =
    match ps with
    | [] -> map
    | (id, _) :: tail when id = "this" -> iter_param map tail
    | (id, (Basic v)) :: tail -> iter_param (MapStr.add id v map) tail
    | _ -> error 801 "invalid action name"
  in
  match Str.bounded_split (Str.regexp " ") s 3 with
  | s1 :: s2 :: s3 :: [] -> (
      let id = int_of_string s1 in
      let name =
        match from_json s2 with
        | Basic (Reference r) -> r
        | _ -> error 802 "invalid value"
      in
      let params =
        match from_json s3 with
        | Store s -> iter_param MapStr.empty s
        | _       -> error 803 "invalid value"
      in
      (id, name, params)
    )
  | _ -> error 804 "invalid action name"
;;
*)

(****************************************************************************
 * Functions to ground actions.
 ****************************************************************************)

(**
 * Convert a list of (identifier => type) to a list of maps
 * (identifier => value list).
 *)
let ground_parameters parameters name typeValues =
  let filter values = SetValue.fold (fun v acc -> match v with
      | Basic bv -> bv :: acc
      | _        -> acc
    ) values []
  in
  let values_of t = filter (Type.values_of t typeValues) in
  let this = !-name in

  (* add 'this' value *)
  let map1 = MapStr.add "this" [Reference this] MapStr.empty in

  (* add 'parent' value (if possible) *)
  let map2 = match this with
    | [] -> map1
    | _  -> MapStr.add "parent" [Reference (!-this)] map1
  in

  (* add (user-defined) parameters' values based on their type *)
  let foreach_user_parameter accumulator (paramID, paramType) =
    MapStr.add paramID (values_of paramType) accumulator
  in
  let map3 = List.fold_left foreach_user_parameter map2 parameters in

  (* Generate all combinations of grounded-parameters. *)
  let foreach_map_add_pair paramID paramValue accumulator map =
    (MapStr.add paramID paramValue map) :: accumulator
  in
  let foreach_value_pair_with paramID maps accumulator paramValue =
    if maps = [] then
      (MapStr.add paramID paramValue MapStr.empty) :: accumulator
    else
      List.fold_left (foreach_map_add_pair paramID paramValue) accumulator maps
  in
  let foreach_parameter paramID paramValues accumulator =
    List.fold_left (foreach_value_pair_with paramID accumulator) [] paramValues
  in
  MapStr.fold foreach_parameter map3 []
;;

(** convert a conjunction of atoms to a map **)
let map_of_atoms = fun map constraint_ ->
  match constraint_ with
  | Equal (r, v) -> MapRef.add r v map
  | _            -> error 805 ""
;;

(** for each clause of global constraints DNF, create a dummy action **)
let create_global_actions globalConstraint actions =
  let pre =
    MapRef.add Variable.reference_of_dummy (Boolean false) MapRef.empty
  in
  let eff =
    MapRef.add Variable.reference_of_dummy (Boolean true) MapRef.empty
  in
  let params = MapStr.empty in
  let counter = ref 0 in (* a counter to uniquify action's name *)
  match globalConstraint with
  | True -> actions
  | Or clauses ->
    begin
      List.fold_left begin fun acc clause ->
        let name = ["!global" ^ (string_of_int !counter)] in
        counter := !counter + 1;
        begin match clause with
        | And css ->
          begin
            let action =
              {
                name          = name;
                parameters    = params;
                cost          = 0;
                preconditions = List.fold_left map_of_atoms pre css;
                effects       = eff
              }
            in
            add action acc
          end

        | Equal (r, v) ->
          begin
            let action =
              {
                name          = name;
                parameters    = params;
                cost          = 0;
                preconditions = MapRef.add r v pre;
                effects       = eff
              }
            in
            add action acc
          end

        | _ -> error 806 "Invalid DNF global constraints"
        end
      end actions clauses
    end

  | And clauses ->
    begin
      let action =
        {
          name          = ["!global"];
          parameters    = params;
          cost          = 0;
          preconditions = List.fold_left map_of_atoms pre clauses;
          effects       = eff
        }
      in
      add action actions
    end

  | _ -> error 807 "Invalid DNF global constraints"
;;


let has_atom reference value map =
    (MapRef.mem reference map) && (MapRef.find reference map) = value
;;

let negate_atom reference value map =
    (MapRef.mem reference map) && (MapRef.find reference map) <> value
;;

(**
 * Compile simple implication of global constraints by modifying precondition
 * of grounded action. Note that the precondition is in the form of
 * conjunction of atoms.
 *
 * @param pre        precondition of an action
 * @param eff        effect of an action
 * @param vars       data structure that holds the variables
 * @param g_implies  a list of simple implication formula
 *
 * @return a list of preconditions, each of which is for an action
 *)
let compile_simple_implication preconditions effects variables
                               globalImplications =
  (* flag : 'true' means some actions have been modified
            'false' means no action has been modified *)
  let modified = ref false in

  (* A recursive-loop that visits each action, and then applies any
     necessary modification on its preconditions to maintain the global
     constraints. Flat 'modified' is set 'true' when at least one action
     has been modified. *)
  let rec iter acc implications = match acc, implications with
    | [], _ | _, [] -> acc (* empty actions or no global constraints *)

    | _, Imply (Equal (rp, vp), Equal (rc, vc)) :: clauses ->
      begin
        let evaluate map pre =
          if has_atom rp vp map then (* 'map' models premise *)
            begin
              (* Since 'map' models premise, then we have to check 'pre' and
                 do any necessary action. *)

              if MapRef.mem rc pre then
                begin
                  (* Since 'pre' falsifies conclusion, then remove it from
                     the final results. *)
                  if (MapRef.find rc pre) <> vc then []

                  (* Since 'pre' models conclusion, then include it into the
                     final results. *)
                  else [pre]
                end
              else
                begin
                  modified := true;
                  (* add extra precondition so that 'pre' models conclusion *)
                  [ MapRef.add rc vc pre ]
                end
            end

          else if negate_atom rc vc map then (* 'map' falsifies conclusion *)
            begin
              (* Since 'map' falsifies conclusion, then we have to check 'pre'
                 and do any necessary action. *)

              (* Since 'pre' models premise, then remove it from the final
                 results. *)
              if has_atom rp vp pre then []

              (* Since 'pre' falsifies premise, then include it into the
                 final results *)
              else if negate_atom rp vp pre then [pre]

              (* If the premise's variable is not exist in the preconditions,
                 then add extra preconditions from all possible values except
                 the one that models premise (this will falsify the
                 constraints). *)
              else
                begin
                  Array.fold_left (fun acc1 v1 -> match v1 with
                    | Basic v2 when v2 <> vp ->
                      begin
                        modified := true;
                        (* Add extra precondition so that 'pre' falsifies
                           premise. *)
                        (MapRef.add rp v2 pre) :: acc1
                      end

                    | _ -> acc1

                  ) [] (Variable.values_of rp variables)
                end
            end

          (* Variables in premise and conclusion are not exist in 'map'.
             Thus, it is safe to include 'pre' into the final results. *)
          else [ pre ]
        in
        (* evaluate effects of an action *)
        let eval_effects accumulator preconditions =
            List.append (evaluate effects preconditions) accumulator
        in
        (* evaluate preconditions & effects of an action *)
        let eval_action accumulator preconditions =
            let eval_preconditions = evaluate preconditions preconditions in
            List.fold_left eval_effects accumulator eval_preconditions
        in
        (* evaluate all-actions in 'acc' *)
        let actions = List.fold_left eval_action [] acc in
        (* evaluate next clauses *)
        iter actions clauses
      end

    | _ -> error 808 "Invalid (simple) global constraints."
  in

  (* compile until there's no modification *)
  let rec compile acc =
    let acc1 = iter acc globalImplications in
    if !modified then
      begin
        modified := false;
        compile acc1
      end
    else
      acc1
  in

  compile [preconditions]
;;

(**
 * Ground a Nuri action.
 * 1. substitute the parameters
 * 2. convert the preconditions into the DNF-formula
 * 3. foreach DNF clause, copy the original action and set the clause as
 *    the preconditions
 * 4. apply simple implication compilation
 *
 * @returns a list of grounded actions
 *)
let ground_action_of (name, params, cost, pre, eff) typeEnvironment variables
                     typeValues addDummy globalImplications actions =
  (* Generate grounded parameters *)
  let parameters = ground_parameters params name typeValues in
  List.fold_left (fun acc parameters ->
    let eff1 = List.fold_left (fun eff (r, v) ->
        let r1 = substitute_parameter_of_reference r parameters in
        let v1 = substitute_parameter_of_basic_value v parameters in
        MapRef.add r1 v1 eff
      ) MapRef.empty eff
    in
    let eff2 =
      if addDummy then MapRef.add Variable.reference_of_dummy (Boolean false) eff1
      else eff1
    in
    let pre1 = Constraint.substitute_free_variables_of pre parameters in
    let pre2 =
      if addDummy
        then MapRef.add Variable.reference_of_dummy (Boolean true) MapRef.empty
      else MapRef.empty
    in
    match Constraint.dnf_of pre1 variables typeEnvironment with
    | False -> acc
    | Or cs -> List.fold_left (fun acc1 c ->
        let pre3 =
          match c with
          | Equal (r, v) -> MapRef.add r v pre2
          | And css   -> List.fold_left map_of_atoms pre2 css
          | _         -> error 809 ""
        in
        let preconditions = compile_simple_implication pre3 eff2
          variables globalImplications
        in
        List.fold_left (fun acc2 pre ->
          let action = {
              name          = name;
              parameters    = parameters;
              cost          = cost;
              preconditions = pre;
              effects       = eff2
            }
          in
          add action acc2
        ) acc1 preconditions
      ) acc cs
    | And css ->
      let pre3 = List.fold_left map_of_atoms pre2 css in
      let preconditions = compile_simple_implication pre3 eff2 variables
        globalImplications
      in
      List.fold_left (fun acc1 pre ->
        let action = {
            name          = name;
            parameters    = parameters;
            cost          = cost;
            preconditions = pre;
            effects       = eff2
          }
        in
        add action acc1
      ) acc preconditions
    | Equal (r, v) ->
      let pre3 = MapRef.add r v pre2 in
      let preconditions = compile_simple_implication pre3 eff2 variables
        globalImplications
      in
      List.fold_left (fun acc1 pre ->
        let action = {
            name          = name;
            parameters    = parameters;
            cost          = cost;
            preconditions = pre;
            effects       = eff2
          }
        in
        add action acc1
      ) acc preconditions
    | True ->
      let preconditions = compile_simple_implication pre2 eff2 variables
        globalImplications
      in
      List.fold_left (fun acc1 pre ->
        let action = {
            name = name;
            parameters = parameters;
            cost = cost;
            preconditions = pre;
            effects = eff2
          }
        in
        add action acc1
      ) acc preconditions
    | _ -> error 810 ""
  ) actions parameters
;;

(** ground a set of actions - returns a list of grounded actions **)
let ground_actions typeEnvironment variables typeValues globalConstraints
                   globalImplications =

  let addDummy = not (globalConstraints = True) in

  (* create dummy actions for global constraints *)
  let groundedActions = create_global_actions globalConstraints empty_ts in

  (* ungrounded-actions *)
  let ungroundedActions = Type.values_of Syntax.T_Action typeValues in

  (* ground all actions *)
  let each_action value accumulator = match value with
    | Action action -> ground_action_of action
                                        typeEnvironment
                                        variables
                                        typeValues
                                        addDummy
                                        globalImplications
                                        accumulator

    | _ -> error 811 "Invalid action." (* a non-action value *)
  in
  SetValue.fold each_action ungroundedActions groundedActions
;;
