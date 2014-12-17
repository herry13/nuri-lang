(** Module Type contains the type environment (list and map)
    as well as its algebra functions.

    Module dependencies:
    - Common

    @author Herry (herry13\@gmail.com)
    @since 2014
*)

open Common
open Syntax

type system = { types : types ; environment : environment }

and types = t list

and environment = variable_type list

and variable_type = Domain.reference * t

and map = t MapRef.t

exception Error of int * string


val string_of_map : ?buffer:Buffer.t -> map -> Buffer.t

val type_of : Domain.reference -> map -> t


val string_of_system : ?buffer:Buffer.t -> system -> Buffer.t

val string_of_environment : ?buffer:Buffer.t -> environment -> Buffer.t

val string_of_types : ?buffer:Buffer.t -> types -> Buffer.t

val initial_system : system

val (@:) : Domain.reference -> system -> t

val (<:) : t -> t -> bool

val has : t -> system -> bool

val well_formed : system -> bool


val assign : t -> t -> Domain.reference -> system -> system

val variables_with_prefix : Domain.reference -> environment -> environment

val copy : Domain.reference -> Domain.reference -> system -> system

val resolve : Domain.reference -> Domain.reference -> system -> (Domain.reference * t)

val extends : Domain.reference -> Domain.reference -> Domain.reference -> system -> system

