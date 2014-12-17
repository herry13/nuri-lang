(* Author: Herry (herry13@gmail.com) *)

open Common

(*******************************************************************
 * type environment
 *******************************************************************)

type map = Syntax.t MapRef.t

(*******************************************************************
 * functions of type environment
 *******************************************************************)

exception TypeError of int * string

val string_of_map : map -> string

val nuriSpecification : ?main:string list -> Syntax.nuri -> map

val type_of : Domain.reference -> map -> Syntax.t

val subtype : Syntax.t -> Syntax.t -> bool


(*******************************************************************
 * a map from type to set of values
 *******************************************************************)

module MapType : Map.S with type key = Syntax.t

type type_values = Domain.SetValue.t MapType.t

val values_of : Syntax.t -> type_values -> Domain.SetValue.t

val add_value : Syntax.t -> Domain.value -> type_values -> type_values

val make_type_values : map -> Domain.flatstore -> map ->
	Domain.flatstore -> Domain.SetValue.t MapType.t
