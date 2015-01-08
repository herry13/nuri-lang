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

exception Error of int * string

val error : ?env:environment -> int -> string -> 'a

val well_formed : environment -> environment -> environment

val string_of_environment : environment -> string

val empty : environment

val find : Domain.reference -> environment -> t

val find_ : Domain.reference -> environment -> t option

val (@:) : Domain.reference -> environment -> t

val subtype : t -> t -> bool

val (<:) : t -> t -> bool

val (=:=) : t -> t -> bool

val bind : environment -> Domain.reference -> ?t_variable:t -> t -> t ->
           environment

val copy : Domain.reference -> Domain.reference -> environment -> environment

val resolve : Domain.reference -> Domain.reference -> environment ->
              (Domain.reference * t) option

val inherit_ : Domain.reference -> Domain.reference -> Domain.reference ->
               environment -> environment

val at : ?env:environment -> string list -> t -> t


val main_of : Domain.reference -> environment -> environment

val replace_forward_type : Domain.reference -> environment -> environment

val merge_types : Domain.reference -> environment -> environment

val symbol_of_enum : string -> string -> environment -> bool



module MapType : Map.S with type key = t

type type_values = Domain.SetValue.t MapType.t

val values_of : t -> type_values -> Domain.SetValue.t

(*val add_value : t -> Domain.value -> type_values -> type_values*)

val make_type_values : environment -> Domain.flatstore -> environment ->
                       Domain.flatstore -> type_values
