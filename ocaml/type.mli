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



val string_of_map : ?buffer:Buffer.t -> map -> Buffer.t

val type_of : Domain.reference -> map -> t

val well_formed : map -> bool



val string_of_environment : ?buffer:Buffer.t -> environment -> Buffer.t

val initial_environment : environment

val (@:) : Domain.reference -> environment -> t

val (<:) : t -> t -> bool



val assign : t -> t -> Domain.reference -> environment -> environment

val variables_with_prefix : ?remove_prefix:bool -> Domain.reference -> environment -> environment

val copy : Domain.reference -> Domain.reference -> environment -> environment

val resolve : Domain.reference -> Domain.reference -> environment -> (Domain.reference * t)

val _inherit : Domain.reference -> Domain.reference -> Domain.reference -> environment -> environment



val main_of : ?main_reference:Domain.reference -> environment -> environment

val replace_forward_type : Domain.reference -> environment -> environment
