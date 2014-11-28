(*******************************************************************
 * interface of module Common
 * Author: Herry (herry13@gmail.com)
 *
 * Common data structures and functions used by other modules.
 *******************************************************************)

(** a map where strings are the key **)
module MapStr : Map.S with type key = string

(** a map where lists of strings (references) are the key **)
module MapRef : Map.S with type key = string list

(** a set of references **)
module SetRef : Set.S with type elt = string list

(** a set of integers **)
module SetInt : Set.S with type elt = int

(** read a string from given filename **)
val read_file : string -> string

(** write a string to given filename **)
val write_file : string -> string -> unit

(** execute a command using 'Unix' module and then return the output from STDOUT **)
val get_process_output : string -> string


(*** helper operators for string buffer **)

(** add a string to a buffer **)
val (<<) : Buffer.t -> string -> unit

(** add a character to a buffer **)
val (<.) : Buffer.t -> char -> unit


(*** helper operator for references ***)

(** return a string of reference by concatenating all strings delimited by '.' **)
val (!^) : string list -> string
