(* Author: Herry (herry13@gmail.com) *)

open Common
open Action

(**
 * This module is for handling a sequential or parallel plan.
 *)

(** type of sequential plan **)
type sequential = Action.t array

(** type of parallel plan **)
type parallel = {
	actions : Action.t array;
	before  : SetInt.t array;
	after   : SetInt.t array
}

let string_of_sequential sequentialPlan =
    let buffer = Buffer.create 42 in
    Array.iteri (fun index action ->
        if index > 0 then Buffer.add_char buffer '\n';
        Buffer.add_string buffer (string_of_int (index + 1));
        Buffer.add_string buffer ") ";
        Action.to_string_buffer action buffer
    ) sequentialPlan;
    Buffer.contents buffer
;;

(* TODO *)
let sequential_of (plan: parallel) : sequential =
	raise (Failure "not implemented")

(** generate a JSON of a sequential plan **)
let json_of_sequential plan =
	let buf = Buffer.create 42 in
	Buffer.add_string buf
		"{\"type\":\"sequential\",\"version\":1,\"actions\":[";
    Array.iteri (fun i a ->
		if i > 0 then Buffer.add_char buf ',';
		Buffer.add_string buf (json_of a);
	) plan;
	Buffer.add_string buf "]}";
	Buffer.contents buf

(** convert a sequential to a parallel (partial-order) plan **)
let parallel_of sequentialPlan =
	let actions = sequentialPlan in
	let before = Array.make (Array.length actions) SetInt.empty in
	let after = Array.make (Array.length actions) SetInt.empty in
	let prev_indexes = ref [] in
	Array.iteri (fun i a ->
		(* add precedence for supporting actions *)
		MapRef.iter (fun r v ->
			try
				let k = List.find (fun j ->
					let eff = actions.(j).effects in
					(MapRef.mem r eff) && ((MapRef.find r eff) = v)
				) !prev_indexes in
				before.(i) <- SetInt.add k before.(i);
				after.(k) <- SetInt.add i after.(k)
			with Not_found -> ();
		) a.preconditions;
		(* add precedence of threatened actions *)
		MapRef.iter (fun r v ->
			List.iter (fun j ->
				let pre = actions.(j).preconditions in
				if (MapRef.mem r pre) && ((MapRef.find r pre) <> v) then
					before.(i) <- SetInt.add j before.(i)
			) !prev_indexes
		) a.effects;
		prev_indexes := i :: !prev_indexes;
	) actions;
	{ actions = actions; before = before; after = after }

(** generate a JSON of a parallel plan **)
let json_of_parallel plan =
	let buf = Buffer.create 42 in
	Buffer.add_string buf "{\"type\":\"parallel\",\"version\":2,\"actions\":[";
	if (Array.length plan.actions) > 0 then (
		Array.iteri (fun i a ->
			if i > 0 then Buffer.add_char buf ',';
			let before = SetInt.elements plan.before.(i) in
			let after = SetInt.elements plan.after.(i) in
			Buffer.add_string buf (json_of_parallel_action a before after)
		) plan.actions
	);
	Buffer.add_string buf "]}";
	Buffer.contents buf
