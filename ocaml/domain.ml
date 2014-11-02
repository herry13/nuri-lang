(* Author: Herry (herry13@gmail.com) *)

open Common

(*******************************************************************
 * semantics primary and secondary domains
 *******************************************************************)
(** core elements **)
type vector   = basic list
and basic     = Boolean of bool
              | Int of int
              | Float of float
              | String of string
              | Null
              | Vector of vector
              | Ref of reference
              | EnumElement of string * string
and value     = Basic of basic
              | Store of store
              | Global of _constraint
              | Link of reference
              | Action of action
              | TBD
              | Unknown
              | Nothing
              | Enum of string list
and _value    = Val of value
              | Undefined
and cell      = ident * value
and store     = cell list
and reference = ident list
and ident     = string

(** constraint elements **)
and _constraint = Eq of reference * basic
                | Ne of reference * basic
				| Greater of reference * basic
				| GreaterEqual of reference * basic
				| Less of reference * basic
				| LessEqual of reference * basic
                | Not of _constraint
                | Imply of _constraint * _constraint
                | And of _constraint list
                | Or of _constraint list
                | In of reference * vector
                | True
                | False

(** action elements **)
and action         = reference * parameter_type list * int * _constraint *
                     effect list
and parameter_type = ident * Syntax.t
and cost           = int
and effect         = reference * basic

(*******************************************************************
 * helpers
 *******************************************************************)

(** exception for any error on semantics algebra **)
exception SfError of int * string

(**
 * receive and print semantics error message
 * @code int error code
 *)
let error code message =
	if message = "" then
		raise (SfError (code, "[err" ^ (string_of_int code) ^ "]"))
	else
		raise (SfError (code, "[err" ^ (string_of_int code) ^ "] " ^
			message)) ;;

(*******************************************************************
 * semantics algebras
 *******************************************************************)

(* identifier and reference functions *)

let (!^) r = String.concat "." r

let rec prefix reference =
	match reference with
	| []           -> []
	| head :: tail -> if tail = [] then []
	                  else head :: (prefix tail)
;;

let (!-) reference = prefix reference ;;

let ref_plus_ref reference1 reference2 = List.append reference1 reference2 ;;

let (@++) reference1 reference2 = ref_plus_ref reference1 reference2 ;;

let ref_plus_id reference identifier = ref_plus_ref reference [identifier] ;;

let (@+.) reference identifier = ref_plus_id reference identifier ;;

let rec ref_minus_ref reference1 reference2 =
	if reference1 = [] then []
	else if reference2 = [] then reference1
	else if (List.hd reference1) = (List.hd reference2)
		then ref_minus_ref (List.tl reference1) (List.tl reference2)
	else reference1
;;

let (@--) reference1 reference2 = ref_minus_ref reference1 reference2 ;;

let ref_prefixeq_ref reference1 reference2 =
	(reference1 @-- reference2) = [] ;;

let (@<=) reference1 reference2 = ref_prefixeq_ref reference1 reference2 ;;

let ref_prefix_ref reference1 reference2 =
	((ref_prefixeq_ref reference1 reference2) &&
		not (reference1 = reference2)) ;;

let (@<) reference1 reference2 = ref_prefix_ref reference1 reference2 ;;

let rec trace baseReference reference =
	match reference with
	| [] -> baseReference
	| "this" :: rs -> trace baseReference rs
	| "root" :: rs -> trace [] rs
	| "parent" :: rs ->
		if baseReference = [] then
			error 501 ("invalid reference " ^ !^reference)
		else
			trace (prefix baseReference) rs
	| id :: rs -> trace (baseReference @+. id) rs
;;

let (@<<) baseReference reference = trace baseReference reference ;;

let simplify reference = trace [] reference ;;

let (!!) reference = simplify reference ;;


(** store functions **)

let rec find store reference : _value =
	match (store, reference) with
	| _, []                      -> Val (Store store)
	| [], _                      -> Undefined
	| (ids,vs) :: tail, id :: rs ->
		if ids = id then
			if rs = [] then Val vs
			else
				match vs with
				| Store child -> find child rs
				| _           -> Undefined
		else find tail reference
;;

(**
 * TODOC
 * unlike 'find', function 'find_follow' can resolve a nested reference
 *)
let find_follow store reference : _value =
	let rec search s r : _value =
		match (s, r) with
		| _, [] -> Val (Store s)
		| [], _ -> Undefined
		| (ids, vs) :: tail, id :: rs ->
			if ids = id then
				if rs = [] then Val vs
				else
					match vs with
					| Store child -> search child rs
					| Basic (Ref r) -> search store (r @++ rs)
					| _ -> Undefined
			else search tail r
	in
	search store reference

let rec resolve store baseReference reference =
	match reference with
	| "root" :: rs   -> ([], find store !!rs)
	| "parent" :: rs ->
		if baseReference = [] then
			error 502 ("invalid reference " ^ !^reference)
		else (
			prefix baseReference,
			find store !!((prefix baseReference) @++ rs)
		)
	| "this" :: rs   -> (baseReference, find store !!(baseReference @++ rs))
	| _              ->
		if baseReference = [] then ([], find store !!reference)
		else
			let value = find store (baseReference @<< reference) in
			match value with
			| Undefined -> resolve store (prefix baseReference) reference
			| _         -> (baseReference, value)
;;

let rec get_link store baseReference reference linkReference accumulator =
	let (baseRef, ref) =
		match linkReference with
		| "root" :: rs -> ([], rs)
		| "parent" :: rs ->
			if baseReference = [] then
				error 515 ("invalid link-reference " ^ !^linkReference)
			else
				([], (prefix baseReference) @++ rs)
		| "this" :: rs -> ([], baseReference @++ rs)
		| _ -> (baseReference, linkReference)
	in
	if SetRef.exists (fun r -> r = ref) accumulator then
		error 503 ("cyclic link-reference " ^ !^ref)
	else
		match (resolve store baseRef ref) with
		| nsp, value -> (
				let r = nsp @++ ref in
				match value with
				| Val Link lr
                | Val Basic Ref lr -> get_link store (prefix r) reference lr (SetRef.add r accumulator)
				| _ -> if r @<= reference then
				           error 504 ("implicit cyclic link-reference " ^ !^r)
				       else
				           (r, value)
			)
;;

let resolve_link store baseReference reference linkReference =
	match linkReference with
	| Link lr -> get_link store baseReference reference lr SetRef.empty
	| _       -> error 505 "invalid link reference"
;;

let rec put store identifier value =
	match store with
	| []              -> (identifier, value) :: []
	| (id, v) :: tail ->
		if id = identifier then
			match v, value with (* merge semantics?? *)
			| Store destination, Store source ->
				(identifier, Store (copy destination source [])) :: tail
			| _,_ -> (identifier, value) :: tail
		else
			(id, v) :: put tail identifier value

and copy destination source prefix =
	match source with
	| []              -> destination
	| (id, v) :: tail -> copy (bind destination (prefix @+. id) v) tail prefix

and bind store reference value =
	match reference with
	| []       -> error 506 "invalid reference"
	| id :: rs ->
		if rs = [] then put store id value
		else
			match store with
			| []               -> error 507 "invalid reference"
			| (ids, vs) :: tail ->
				if ids = id then
					match vs with
					| Store child -> (id, Store (bind child rs value)) :: tail
					| _           -> error 508 "invalid reference"
				else
					(ids,vs) :: bind tail reference value
;;

let inherit_proto store baseReference prototypeReference reference =
	match resolve store baseReference prototypeReference with
	| _, Val (Store s) -> copy store s reference
	| _, Val (Link lr) -> (
			match resolve_link store baseReference reference (Link lr) with
			| _, Val (Store s) -> copy store s reference
			| _, _             -> error 509 ""
		)
	| _, _             -> error 510 ""
;;

let rec replace_link store baseReference identifier value baseReference1 =
	let rp = baseReference @+. identifier in
	match value with
	| Link rl  -> (
			match resolve_link store baseReference1 rp (Link rl) with
			| _, Undefined -> error 511 ""
			| nsp, Val vp  -> (
					let sp = bind store rp vp in
					match vp with
					| Store ssp -> accept sp rp ssp nsp
					| _         -> sp
				)
		)
    | Basic Ref r ->
        (
            match resolve_link store baseReference1 rp (Link r) with
            | nsp, Val Basic vp -> bind store rp (Basic vp)
            | _, _ -> store
        )
	| Store vs -> accept store rp vs rp
	| _        -> store
		
and accept store baseReference store1 baseReference1 =
	match store1 with
	| []      -> store
	| (id, v) :: sp ->
		let sq = replace_link store baseReference id v baseReference1 in
		accept sq baseReference sp baseReference1
;;

let rec value_TBD_exists ns store =
	match store with
	| []            -> []
	| (id, v) :: ss ->
		match v with
		| TBD      -> ns @+. id
		| Store sp -> (
                match value_TBD_exists (ns @+. id) sp with
                | []  -> value_TBD_exists ns ss
                | ref -> ref
            )
		| _        -> value_TBD_exists ns ss
;;


(*******************************************************************
 * convert reference (list of string) to string
 *******************************************************************)

(* let (!^) reference = "$." ^ String.concat "." reference ;; *)

let string_of_ref reference = !^reference ;;


(*******************************************************************
 * convert JSON to the semantics domain
 *******************************************************************)

let rec from_json store =
	let reference_separator = Str.regexp "\\." in
	let rec make_vector acc vec =
		match vec with
		| []           -> acc
		| head :: tail -> (
				match nuri_of head with
				| Basic bv -> (make_vector (bv :: acc) tail)
				| _        -> error 512 ""
			)
	and make_store acc s =
		match s with
		| []              -> acc
		| (id, v) :: tail -> make_store ((id, (nuri_of v)) :: acc) tail
	and nuri_of = function
		| `String str
			when (String.length str) > 2 && str.[0] = '$' && str.[1] = '.' ->
				Basic (Ref (List.tl (Str.split reference_separator str)))
		| `String str -> Basic (String str)
		| `Bool b     -> Basic (Boolean b)
		| `Float f    -> Basic (Float f)
		| `Int i      -> Basic (Int i)
		| `Null       -> Basic Null
		| `List vec   -> Basic (Vector (make_vector [] vec))
		| `Assoc s    -> Store (make_store [] s)
	in
	nuri_of (Yojson.Basic.from_string store)
;;


(*******************************************************************
 * Flat-Store domain
 *******************************************************************)

type flatstore = value MapRef.t

let static_object = Store [] ;;

let normalise store =
	let rec visit store baseReference flatstore =
		match store with
		| [] -> flatstore
		| (id, v) :: tail ->
			let r = baseReference @+. id in
			let fss =
				match v with
				| Store child ->
					visit child r (MapRef.add r static_object flatstore)
				| Action (_, ps, c, pre, post) ->
					MapRef.add r (Action (r, ps, c, pre, post)) flatstore
				| _ -> 
					MapRef.add r v flatstore
			in
			visit tail baseReference fss
	in
	visit store [] MapRef.empty
;;


(*******************************************************************
 * set of values
 *******************************************************************)

module SetValue = Set.Make (
	struct
		type t = value
		let compare = Pervasives.compare
	end
)


(*******************************************************************
 * parameters
 *******************************************************************)

type ground_parameters = basic MapStr.t

(**
 * substitute each left-hand side reference with a reference as
 * specified in the parameters table
 *)
let substitute_parameter_of_reference reference groundParameters =
	match reference with
	| id :: tail ->
		if MapStr.mem id groundParameters then
			match MapStr.find id groundParameters with
			| Ref r1 -> r1 @++ tail
			| _      -> error 513 ("cannot replace left-hand side " ^
				"reference with a non-reference value")
			            (* cannot replace left-hand side reference with
						   a non-reference value *)
		else reference
	| _ -> reference

(**
 * substitute each right-hand side reference of basic value
 * with a value as specified in the parameters table
 *)
let substitute_parameter_of_basic_value basicValue groundParameters =
	match basicValue with
	| Ref (id :: tail) ->
		if MapStr.mem id groundParameters then
			match MapStr.find id groundParameters with
			| Ref r1            -> Ref (r1 @++ tail)
			| v1 when tail = [] -> v1
			| _                 -> error 514 ""
		else basicValue
	| _ -> basicValue

