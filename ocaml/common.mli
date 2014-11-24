(* Author: Herry (herry13@gmail.com)
   Common data structures used by other modules. *)

module MapStr : Map.S with type key = string

module MapRef : Map.S with type key = string list

module SetRef : Set.S with type elt = string list

module SetInt : Set.S with type elt = int

val read_file : string -> string

val write_file : string -> string -> unit

val get_process_output : string -> string
