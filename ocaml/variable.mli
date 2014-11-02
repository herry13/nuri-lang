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

val make : reference -> int -> value array -> value -> value -> t

val iteri_values : (int -> value -> unit) -> t -> unit

val string_of_values : value array -> string

val string_of_variable : t -> string

val r_dummy : reference

val dummy : t

val index : t -> int

val name : t -> reference

val init : t -> value

val goal : t -> value

val size : t -> int

val values : t -> value array

val index_of_value : value -> t -> int 


(*****************************************************************
 * variables functions
 *****************************************************************)

val make_ts : Type.map -> flatstore -> Type.map -> flatstore ->
	Type.type_values -> ts

val mem : reference -> ts -> bool

val find : reference -> ts -> t

val values_of : reference -> ts -> value array

val total : ts -> int

val iter : (t -> unit) -> ts -> unit

val sort : ts -> unit

val intersection_with_values : reference -> vector -> ts -> ts

val intersection_with_value : reference -> value -> ts -> ts

val remove_value_from : reference -> value -> ts -> ts

val remove_values_from : reference -> vector -> ts -> ts

val string_of_variables : ts -> string
