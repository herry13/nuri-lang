(** Module Nuri_json contains the functions to serialise/deserialise any type or
    value to/from JSON format.

    Module dependencies
    - Common
    - Syntax
    - Domain

    @author Herry (herry13\@gmail.com)
    @since 2014
*)

exception Nuri_jsonError of int * string

val of_store : Type.map -> Domain.store -> string

val of_constraint : Domain._constraint -> string

val of_value : ?no_lazy:bool -> Domain.value -> string

val of_flatstore : Domain.flatstore -> string

val of_type : Syntax.t -> string


val to_store : string -> Type.map * Domain.store
