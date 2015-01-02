(** Module Type contains the type environment (list and map)
    as well as its algebra functions.

    Module dependencies:
    - Common

    @author Herry (herry13\@gmail.com)
    @since 2014
*)

open Common
open Syntax


type environment = t list MapRef.t

and variable_type = Domain.reference * t

and map = t MapRef.t


exception Error of int * string

val error : ?env:environment -> ?map:map -> int -> string -> 'a


val string_of_map : map -> string

val type_of : Domain.reference -> map -> t

val map_of : environment -> map

val well_formed : map -> map -> map


val string_of_environment : environment -> string

val empty : environment

val find : Domain.reference -> environment -> t

val (@:) : Domain.reference -> environment -> t

val subtype : t -> t -> bool

val (<:) : t -> t -> bool

val (=:=) : t -> t -> bool

val bind : environment -> Domain.reference -> ?t_variable:t -> t -> t ->
           environment

val copy : Domain.reference -> Domain.reference -> environment -> environment

val resolve : Domain.reference -> Domain.reference -> environment ->
              (Domain.reference * t)

val inherit_ : Domain.reference -> Domain.reference -> Domain.reference ->
               environment -> environment

val at : ?env:environment -> string list -> t -> t


val main_of : Domain.reference -> environment -> environment

val replace_forward_type : Domain.reference -> environment -> environment

val merge_types : Domain.reference -> environment -> map

val symbol_of_enum : string -> string -> environment -> bool


module MapType : Map.S with type key = t

type type_values = Domain.SetValue.t MapType.t

val values_of : t -> type_values -> Domain.SetValue.t

(*val add_value : t -> Domain.value -> type_values -> type_values*)

val make_type_values : map -> Domain.flatstore -> map -> Domain.flatstore ->
                       type_values
