(* Author: Herry (herry13@gmail.com) *)

open Common

(*******************************************************************
 * semantics domain
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

(** action elements : name * parameters * cost * preconditions * effects **)
and action         = reference * parameter_type list * cost * _constraint *
                     effect list
and parameter_type = ident * Syntax.t
and cost           = int
and effect         = reference * basic

(*******************************************************************
 * semantics algebras
 *******************************************************************)

(** exception for any error on semantics algebra **)
exception SfError of int * string

(** a function that raise an SfError **)
val error : int -> string -> 'a

(* identifier-reference functions *)

val prefix : reference -> reference

val (!-) : reference -> reference

val (@++) : reference -> reference -> reference

val (@+.) : reference -> string -> reference

val (@--) : reference -> reference -> reference

val (@<=) : reference -> reference -> bool

val (@<) : reference -> reference -> bool

val (@<<) : reference -> reference -> reference

val (!!) : reference -> reference

(* store functions *)

val find : store -> reference -> _value

val find_follow : store -> reference -> _value

val resolve : ?follow_ref:bool -> store -> reference -> reference -> (reference * _value)

val put : store -> string -> value -> store

val bind : store -> reference -> value -> store

val copy : store -> store -> reference -> store

val inherit_proto : store -> reference -> reference -> reference -> store

val accept : store -> reference -> store -> reference -> store

val value_TBD_exists : reference -> store -> reference

val add : ?store:store -> ?namespace:reference -> basic -> basic -> basic


module SetFunc : Set.S with type elt = (store -> reference -> value)

val resolve_function : ?visited:SetFunc.t -> store -> reference -> (store -> reference -> value) -> value


(*******************************************************************
 * domain convertion functions to string
 *******************************************************************)

val (!^) : reference -> string

val string_of_basic_value : basic -> string

(*******************************************************************
 * Flat-Store domain
 *******************************************************************)

type flatstore = value MapRef.t

val static_object : value

val normalise : store -> flatstore


(*******************************************************************
 * set of values
 *******************************************************************)

module SetValue : Set.S with type elt = value


(*******************************************************************
 * parameters
 *******************************************************************)

type ground_parameters = basic MapStr.t

val substitute_parameter_of_reference : reference -> ground_parameters ->
	reference

val substitute_parameter_of_basic_value : basic -> ground_parameters -> basic


(*******************************************************************
 * utility functions
 *******************************************************************)

val to_state : store -> store
