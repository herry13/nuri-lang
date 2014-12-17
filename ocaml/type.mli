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

and variable_type = variable * t

and variable = string list

and map = t MapRef.t

exception Error of int * string


val string_of_map : ?buffer:Buffer.t -> map -> Buffer.t

val type_of : variable -> map -> t


val string_of_system : ?buffer:Buffer.t -> system -> Buffer.t

val string_of_environment : ?buffer:Buffer.t -> environment -> Buffer.t

val string_of_types : ?buffer:Buffer.t -> types -> Buffer.t

val initial_system : system

val (@:) : variable -> system -> bool

val (-:) : variable -> system -> t

val (<:) : t -> t -> bool

val has : t -> system -> bool

val well_formed : system -> bool


val assign : t -> t -> variable -> system -> system
