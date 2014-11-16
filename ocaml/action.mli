(* Author: Herry (herry13@gmail.com) *)

open Common
open Domain

(**
 * This module represents a grounded Nuri action.
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

(** create an action **)
val make : reference -> basic MapStr.t -> int -> basic MapRef.t ->
           basic MapRef.t -> t

(** return given action's name **)
val name : t -> reference

(** return given action's parameters **)
val parameters : t -> basic MapStr.t

(** return given action's cost **)
val cost : t -> int

(** return given action's preconditions **)
val preconditions : t -> basic MapRef.t

(** return given action's effects **)
val effects : t -> basic MapRef.t

(** generate JSON of an action **)
val json_of : t -> string

(** generate JSON of an action with precedence constraints **)
val json_of_parallel_action : t -> int list -> int list -> string

(** generate JSON of a list of actions **)
val json_of_actions : t list -> string

(** ground a collection of actions **)
val ground_actions : Type.map -> Variable.ts -> Type.type_values ->
                     _constraint -> _constraint list -> ts

(** iterates a collection of actions **)
val iter : (t -> unit) -> ts -> unit

(** folds a collection of actions **)
val fold : ('a -> t -> 'a) -> 'a -> ts -> 'a

(** add an action to the collection **)
val add : t -> ts -> ts

(** return an empty collection of actions **)
val empty : ts

(** convert a collection of actions to an array of actions **)
val to_array : ts -> t array


val encode_name : int -> t -> string

val decode_name : string -> (int * reference * basic MapStr.t)


val to_string_buffer : t -> Buffer.t -> unit
