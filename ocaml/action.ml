(* Author: Herry (herry13@gmail.com) *)

open Common
open Domain

(**
 * This module represents a grounded action.
 *)

(** type of an action **)
type t = {
	name:          reference;
	parameters:    basic MapStr.t;
	cost:          cost;
	preconditions: basic MapRef.t;
	effects:       basic MapRef.t
}

(** type of a collection of actions **)
type ts = { total: int; actions: t list }


let make name params cost pre eff = {
		name = name;
		parameters = params;
		cost = cost;
		preconditions = pre;
		effects = eff
	}
;;
	
let name a = a.name;;
let parameters a = a.parameters;;
let cost a = a.cost;;
let preconditions a = a.preconditions;;
let effects a = a.effects;;


(****************************************************************************
 * Functions to operate over a collection of actions (ts)
 ****************************************************************************)

let iter (f: t -> unit) (acts: ts) : unit = List.iter f acts.actions;;

let fold (f: 'a -> t -> 'a) (acc: 'a) (acts: ts) : 'a =
	List.fold_left f acc acts.actions
;;

let add (a: t) (acts: ts) : ts = {
		total = acts.total + 1;
		actions = a :: acts.actions
	}
;;

let empty : ts = { total = 0; actions = [] };;

let to_array (acts: ts) : t array = Array.of_list acts.actions;;


(****************************************************************************
 * Functions to generate JSON of actions.
 ****************************************************************************)

let json_of_parameters buffer parameters =
	Buffer.add_char buffer '{';
	let first = ref 1 in
	MapStr.iter (fun id v ->
        if id <> "this" && id <> "parent" then (
    		if !first = 0 then Buffer.add_char buffer ',';
    		Buffer.add_char buffer '"';
    		Buffer.add_string buffer id;
    		Buffer.add_string buffer "\":";
    		Buffer.add_string buffer (Json.of_value (Basic v));
    		first := 0
        )
	) parameters;
	Buffer.add_char buffer '}'
;;

let json_of_preconditions buffer preconditions =
	Buffer.add_char buffer '{';
	let first = ref 1 in
	MapRef.iter (fun r v ->
		if !first = 0 then Buffer.add_char buffer ',';
		Buffer.add_char buffer '"';
		Buffer.add_string buffer !^r;
		Buffer.add_string buffer "\":";
		Buffer.add_string buffer (Json.of_value (Basic v));
		first := 0
	) preconditions;
	Buffer.add_char buffer '}'
;;

let json_of_effects = json_of_preconditions;;

let json_of_elements buffer action =
	Buffer.add_string buffer "\"name\":\"";
	Buffer.add_string buffer !^(action.name);
	Buffer.add_string buffer "\",\"parameters\":";
	json_of_parameters buffer action.parameters;
	Buffer.add_string buffer ",\"cost\":";
	Buffer.add_string buffer (string_of_int action.cost);
	Buffer.add_string buffer ",\"conditions\":";
	json_of_preconditions buffer action.preconditions;
	Buffer.add_string buffer ",\"effects\":";
	json_of_effects buffer action.effects
;;

let json_of_action buffer action =
	Buffer.add_char buffer '{';
	json_of_elements buffer action;
	Buffer.add_char buffer '}'
;;

let json_of action =
	let buffer = Buffer.create 42 in
	json_of_action buffer action;
	Buffer.contents buffer
;;

let json_of_parallel_action action before after =
	let buffer = Buffer.create 42 in
	let to_json elements =
		Buffer.add_char buffer '[';
		if elements <> [] then (
			Buffer.add_string buffer (string_of_int (List.hd elements));
			List.iter (fun i ->
				Buffer.add_char buffer ',';
				Buffer.add_string buffer (string_of_int i)
			) (List.tl elements)
		);
		Buffer.add_char buffer ']'
	in
	Buffer.add_char buffer '{';
	json_of_elements buffer action;
	Buffer.add_string buffer ",\"before\":";
	to_json before;
	Buffer.add_string buffer ",\"after\":";
	to_json after;
	Buffer.add_char buffer '}';
	Buffer.contents buffer
;;


let json_of_actions actions =
	let buffer = Buffer.create 42 in
	Buffer.add_char buffer '[';
	(
		match actions with
		| [] -> Buffer.add_string buffer ""
		| act :: [] -> json_of_action buffer act
		| act :: acts ->
			json_of_action buffer act;
			List.iter (fun a ->
				Buffer.add_char buffer ',';
				json_of_action buffer a
			) acts
	);
	Buffer.add_char buffer ']';
	Buffer.contents buffer
;;

(****************************************************************************
 * Functions to encode/decode actions' name. This will be used in FDR.
 ****************************************************************************)

let encode_name id a =
	let buf = Buffer.create 42 in
	Buffer.add_string buf (string_of_int id);
	Buffer.add_string buf " \"";
	Buffer.add_string buf !^(a.name);
	Buffer.add_string buf "\" ";
	json_of_parameters buf a.parameters;
	Buffer.contents buf
;;

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
				| Basic (Ref r) -> r
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

(****************************************************************************
 * Functions to ground actions.
 ****************************************************************************)

(**
 * convert a list of (identifier => type) to a list of maps
 * (identifier => value)
 *)
let ground_parameters parameters name typeValues =
    let this = prefix name in
	let map1 = MapStr.add "this" [Ref this] MapStr.empty in
    let map1 = match this with
        | [] -> map1
        | _  -> MapStr.add "parent" [Ref (prefix this)] map1
    in
	let map2 = List.fold_left (fun acc (id, t) ->
			let values = SetValue.fold (fun v acc ->
					match v with
					| Basic bv -> bv :: acc
					| _        -> acc
				) (Type.values_of t typeValues) []
			in
			MapStr.add id values acc
		) map1 parameters
	in
	MapStr.fold (fun id values acc1 ->
		List.fold_left (fun acc2 v ->
			if acc1 = [] then (MapStr.add id v MapStr.empty) :: acc2
			else List.fold_left (
					fun acc3 map -> (MapStr.add id v map) :: acc3
				) acc2 acc1
		) [] values
	) map2 []
;;

(** convert a conjunction of atoms to a map **)
let map_of_atoms = fun map _constraint ->
	match _constraint with
	| Eq (r, v) -> MapRef.add r v map
	| _         -> error 805 ""
;;

(** for each clause of global constraints DNF, create a dummy action **)
let create_global_actions globalConstraint actions =
	let pre = MapRef.add Variable.r_dummy (Boolean false) MapRef.empty in
	let eff = MapRef.add Variable.r_dummy (Boolean true) MapRef.empty in
	let params = MapStr.empty in
	let counter = ref 0 in
	match globalConstraint with
	| True  -> actions
	| Or clauses ->
		List.fold_left (fun acc c ->
			let name = ["!global" ^ (string_of_int !counter)] in
			counter := !counter + 1;
			match c with
			| And css ->
				let pre1 = List.fold_left map_of_atoms pre css in
				let action = {
						name = name;
						parameters = params;
						cost = 0;
						preconditions = pre1;
						effects = eff
					}
				in
				add action acc
			| Eq (r, v) ->
				let pre1 = MapRef.add r v pre in
				let action = {
						name = name;
						parameters = params;
						cost = 0;
						preconditions = pre1;
						effects = eff
					}
				in
				add action acc
			| _ -> error 806 ""
		) actions clauses
	| And clauses ->
		let name = ["!global"] in
		let pre1 = List.fold_left map_of_atoms pre clauses in
		let action = {
				name = name;
				parameters = params;
				cost = 0;
				preconditions = pre1;
				effects = eff
			}
		in
		add action actions
	| _ -> error 807 ""
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
let compile_simple_implication preconditions effects variables globalImplications =
	let modified = ref false in
	let has_atom r v map = (MapRef.mem r map) && (MapRef.find r map) = v in
	let negate_atom r v map = (MapRef.mem r map) && (MapRef.find r map) <> v in
	let rec iter acc implications = match acc, implications with
        | [], _ -> acc
        | _, [] -> acc
        | _, Imply (Eq (rp, vp), Eq (rc, vc)) :: css ->
            (
                let evaluate map pre =
                    if has_atom rp vp map then (
                        if MapRef.mem rc pre then (  (* 'map' models premise *)
                            if (MapRef.find rc pre) <> vc then (  (* 'pre' falsifies conclusion *)
                                []
                            )
                            else (       (* 'pre' models conclusion *)
                                [ pre ]
                            )
                        ) else (
                            modified := true;         (* add extra precondition so that 'pre' models conclusion *)
                            [ MapRef.add rc vc pre ]
                        )
                    )
                    else if negate_atom rc vc map then (  (* 'map' falsifies conclusion *)
                        if has_atom rp vp pre then (      (* 'pre' models premise       *)
                            []
                        )
                        else if negate_atom rp vp pre then (  (* 'pre' falsifies premise *)
                            [ pre ]
                        )
                        else (  (* for other values do ... *)
                            Array.fold_left (fun acc1 v1 ->
                                match v1 with
                                | Basic v2 when v2 <> vp ->
                                    (
                                        modified := true;   (* add extra precondition so that 'pre' models conclusion *)
                                        (MapRef.add rp v2 pre) :: acc1
                                    )
                                | _ -> acc1
                            ) [] (Variable.values_of rp variables)
                        )
                    )
                    else (
                        [ pre ]
                    )
                in
                let acc2 = List.fold_left (fun acc3 pre ->
                        (* evaluate preconditions *)
                        let acc4 = evaluate pre pre in

                        (* evaluate effects *)
                        List.fold_left (fun acc5 pre ->
                            List.append (evaluate effects pre) acc5
                        ) acc3 acc4
                    ) [] acc
                in
                iter acc2 css
            )
        | _ -> error 808 ""  (* invalid input *)
    in
	let rec compile acc =  (* compile until there's no modification *)
		let acc1 = iter acc globalImplications in
		if !modified then (
			modified := false;
			compile acc1
		) else (
            acc1
        )
	in
	compile [preconditions]
;;

(**
 * Ground a Nuri action.
 * 1. substitute the parameters
 * 2. convert the preconditions into the DNF-formula
 * 3. foreach DNF clause, copy the original action and set the clause as the preconditions
 * 4. apply simple implication compilation
 *
 * returns a list of grounded actions
 *)
let ground_action_of (name, params, cost, pre, eff) typeEnvironment variables
		typeValues addDummy globalImplications actions =
	let parameters = ground_parameters params name typeValues in
	List.fold_left (fun acc parameters ->
		let eff1 = List.fold_left (fun eff (r, v) ->
				let r1 = substitute_parameter_of_reference r parameters in
				let v1 = substitute_parameter_of_basic_value v parameters in
				MapRef.add r1 v1 eff
			) MapRef.empty eff
		in
		let eff2 =
			if addDummy then MapRef.add Variable.r_dummy (Boolean false) eff1
			else eff1
		in
		let pre1 = Constraint.substitute_free_variables_of pre parameters in
		let pre2 =
			if addDummy
				then MapRef.add Variable.r_dummy (Boolean true) MapRef.empty
			else MapRef.empty
		in
		match Constraint.dnf_of pre1 variables typeEnvironment with
		| False -> acc
		| Or cs -> List.fold_left (fun acc1 c ->
				let pre3 =
					match c with
					| Eq (r, v) -> MapRef.add r v pre2
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
		| Eq (r, v) ->
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
	let actions = create_global_actions globalConstraints empty in
	(* ground all actions *)
	SetValue.fold (fun value actions ->
		match value with
		| Action action ->
			ground_action_of action typeEnvironment variables typeValues
				addDummy globalImplications actions
		| _ -> error 811 "not an action" (* a non-action value *)
	) (Type.values_of Syntax.TAction typeValues) actions
;;
