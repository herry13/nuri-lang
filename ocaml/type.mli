(** Module Type contains the type environment (list and map)
    as well as its algebra functions.

    Module dependencies:
    - Common

    @author Herry (herry13\@gmail.com)
    @since 2014
*)

open Common
open Syntax

type environment = variable_type list

and variable_type = Domain.reference * t

and map = t MapRef.t

exception Error of int * string

val error : int -> string -> 'a


val string_of_map : ?buffer:Buffer.t -> map -> Buffer.t

val type_of : Domain.reference -> map -> t

val well_formed : map -> bool



val string_of_environment : ?buffer:Buffer.t -> environment -> Buffer.t

val initial_environment : environment

val find : Domain.reference -> environment -> t

val (@:) : Domain.reference -> environment -> t

val subtype : t -> t -> bool

val (<:) : t -> t -> bool

val (=:=) : t -> t -> bool



val bind : t -> t -> Domain.reference -> environment -> environment

val variables_with_prefix : ?remove_prefix:bool -> Domain.reference ->
                            environment -> environment

val copy : Domain.reference -> Domain.reference -> environment -> environment

val resolve : Domain.reference -> Domain.reference -> environment ->
              (Domain.reference * t)

val _inherit : Domain.reference -> Domain.reference -> Domain.reference ->
               environment -> environment



val main_of : Domain.reference -> environment -> environment

val replace_forward_type : Domain.reference -> environment -> environment



module MapType : Map.S with type key = t

type type_values = Domain.SetValue.t MapType.t

val values_of : t -> type_values -> Domain.SetValue.t

(*val add_value : t -> Domain.value -> type_values -> type_values*)

val make_type_values : map -> Domain.flatstore -> map -> Domain.flatstore ->
                       type_values
