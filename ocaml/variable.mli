(** Module Variable contains the data structures and functions of variables
    of Finite Domain Representation (FDR).

    Module dependencies:
    - Common
    - Syntax
    - Domain
    - Type

    @author Herry (herry13\@gmail.com)
    @since 2014
*)

open Common
open Domain

(** A data structure of a variable. *)
type t =
{
  name   : reference;   (** variable's name *)
  index  : int;         (** variable's index *)
  values : value array; (** domain of values *)
  init   : value;       (** initial value *)
  goal   : value        (** goal value *)
}

(** A collection of variables. The variables are kept in two data-structures.
    First is a map where the keys are the variables' name. Second is an array
    where the indexes are the variables' index.
*)
type ts =
{
  map : t MapRef.t;
  arr : t array
}

exception Error of int * string


(*****************************************************************
 * Variable operation functions
 *****************************************************************)

val make : reference -> int -> value array -> value -> value -> t

val iteri_values : (int -> value -> unit) -> t -> unit

val string_of_variable : t -> string

val reference_of_dummy : reference

val dummy : t

val index : t -> int

val name : t -> reference

val init : t -> value

val goal : t -> value

val size : t -> int

val values : t -> value array

val index_of_value : value -> t -> int 

val intersect : value list -> t -> t


(*****************************************************************
 * Collection of variables operation functions
 *****************************************************************)

val make_ts : Type.environment -> flatstore -> Type.environment ->
              flatstore -> Type.type_values -> ts

val mem : reference -> ts -> bool

val find : reference -> ts -> t

val values_of : reference -> ts -> value array

val total : ts -> int

val iter : (t -> unit) -> ts -> unit

val sort : ts -> unit

val intersection_with_values : reference -> vector -> ts -> ts

val intersection_with_value : reference -> value -> ts -> ts

val intersect_ts : value list -> reference -> ts -> ts

val remove_value_from : reference -> value -> ts -> ts

val remove_values_from : reference -> vector -> ts -> ts

val string_of_variables : ts -> string
