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
              | Lazy of (store -> reference -> value)
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

(* TODO: update documentation of the formal semantics *)
let rec resolve ?follow_ref:(follow=false) store baseReference reference =
    let findx s r = if follow then find_follow s r else find s r in
	match reference with
	| "root" :: rs   -> ([], findx store !!rs)
	| "parent" :: rs ->
		if baseReference = [] then
			error 502 ("invalid reference " ^ !^reference)
		else (
			prefix baseReference,
			findx store !!((prefix baseReference) @++ rs)
		)
	| "this" :: rs   -> (baseReference, findx store !!(baseReference @++ rs))
	| _              ->
		if baseReference = [] then ([], findx store !!reference)
		else
			let value = findx store (baseReference @<< reference) in
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

module SetFunc = Set.Make
(
    struct
	    type t = (store -> reference -> value)
	    let compare = Pervasives.compare
    end
)

let rec resolve_function ?visited:(bucket=SetFunc.empty) store baseReference func =
    if SetFunc.exists (fun f -> f = func) bucket then
        error 525 "Cyclic function detected."
    else
        match func store baseReference with
        | Lazy f1 -> resolve_function ~visited:(SetFunc.add func bucket) store baseReference f1
        | value -> value
;;

let rec replace_lazy store baseReference identifier value baseReference1 =
	let rp = baseReference @+. identifier in
	match value with
	| Link rl  -> (
			match resolve_link store baseReference1 rp (Link rl) with
            | _  , Undefined     -> error 511 ""
            | nsp, Val Store ssp -> accept (bind store rp (Store ssp)) rp ssp nsp
            | _  , Val Lazy func ->
                (
                    let vp = resolve_function store baseReference1 func in
                    let sp = bind store rp vp in
                    replace_lazy sp baseReference identifier vp baseReference1
                )
            | _  , Val vp        -> bind store rp vp
		)
    | Basic Ref r ->
        (
            match resolve_link store baseReference1 rp (Link r) with
            | _, Val Basic vp  -> bind store rp (Basic vp)
            | _, Val Lazy func ->
                (
                    let vp = resolve_function store baseReference1 func in
                    let sp = bind store rp vp in
                    replace_lazy sp baseReference identifier vp baseReference1
                )
            | _, _             -> store
        )
	| Store vs -> accept store rp vs rp
    | Lazy func ->
        (
            let vp = resolve_function store baseReference1 func in
            let sp = bind store rp vp in
            replace_lazy sp baseReference identifier vp baseReference1
        )
	| _        -> store
		
and accept store baseReference store1 baseReference1 =
	match store1 with
	| []      -> store
	| (id, v) :: sp ->
		let sq = replace_lazy store baseReference id v baseReference1 in
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
 * domain convertion functions to string
 *******************************************************************)

(* TODO: update semantics algebra in the documentation *)
let rec string_of_basic_value bv = match bv with
    | Boolean b -> if b then "true" else "false"
    | Int i     -> string_of_int i
    | Float f   -> string_of_float f
    | String s  -> s
    | Null      -> "null"
    | Vector v  ->
        (
            let buf = Buffer.create 15 in
            let rec iter v = match v with
                | [] -> ()
                | head :: [] -> Buffer.add_string buf (string_of_basic_value head)
                | head :: tail -> Buffer.add_string buf (string_of_basic_value head); iter tail
            in
            Buffer.add_char buf '[';
            iter v;
            Buffer.add_char buf ']';
            Buffer.contents buf
        )
    | Ref r     -> (!^) r
    | EnumElement (enum, symbol) -> enum ^ "." ^ symbol
;;


(*******************************************************************
 * Helper functions for module valuation
 *******************************************************************)

(* TODO: update semantics algebra in the documentation *)
let rec add ?store:(s=[]) ?namespace:(ns=[]) left right = match left, right with
    | Ref r1, Ref r2       ->
        (
            match (resolve ~follow_ref:true s ns r1),
                  (resolve ~follow_ref:true s ns r2)
            with
            | (_, Val Basic v1), (_, Val Basic v2) -> add ~store:s ~namespace:ns v1 v2
            | (_, Undefined), (_, Undefined) -> error 516 "Both references of '+' are not exist."
            | (_, Undefined), _              -> error 517 "Left reference of '+' is not exist."
            | _             , (_, Undefined) -> error 518 "Right reference of '+' is not exist."
            | _ -> error 519 "Operands of '+' are not basic values."
        )
    | Ref r1, v2 ->
        (
            match resolve ~follow_ref:true s ns r1 with
            | (_, Val Basic v1) -> add ~store:s ~namespace:ns v1 v2
            | (_, Undefined)    -> error 520 "Left reference of '+' is not exist."
            | _                 -> error 521 "Left operand of '+' is not a basic value."
        )
    | v1, Ref r2 ->
        (
            match resolve ~follow_ref:true s ns r2 with
            | (_, Val Basic v2) -> add ~store:s ~namespace:ns v1 v2
            | (_, Undefined)    -> error 522 "Right reference of '+' is not exist."
            | _                 -> error 523 "Right operand of '+' is not a basic value."
        )
    | Int i, Float f
    | Float f, Int i       -> Float ((float_of_int i) +. f)
    | Float f1, Float f2   -> Float (f1 +. f2)
    | Int i1, Int i2       -> Int (i1 + i2)
    | String s1, String s2 -> String (s1 ^ s2)
    | String s, v          -> String (s ^ (string_of_basic_value v))
    | v, String s          -> String ((string_of_basic_value v) ^ s)
    | v1, v2 -> error 524 ("Invalid operands '+': " ^ (string_of_basic_value v1) ^
                           ", " ^ (string_of_basic_value v2))
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
			| Ref r1 -> simplify (r1 @++ tail)
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
			| Ref r1            -> Ref (simplify (r1 @++ tail))
			| v1 when tail = [] -> v1
			| _                 -> error 514 ""
		else basicValue
	| _ -> basicValue

