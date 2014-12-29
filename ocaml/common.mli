(** Module Common contains common data structures and functions used by other
    modules.

    @author Herry (herry13\@gmail.com)
    @since 2014
*)

(** a map where strings are the key *)
module MapStr : Map.S with type key = string

(** a map where lists of strings (references) are the key *)
module MapRef : Map.S with type key = string list

(** a set of references *)
module SetRef : Set.S with type elt = string list

(** a set of integers *)
module SetInt : Set.S with type elt = int

(** Read a text file, and then return its contents. *)
val read_file : string -> string

(** Write a string to given filename. *)
val write_file : string -> string -> unit

(** Execute a command using 'Unix' module and then return the output
    from STDOUT. *)
val get_process_output : string -> string


(*** helper operators for string buffer **)

(** Add a string to a buffer. *)
val (<<) : Buffer.t -> string -> unit

(** Add a string to a buffer, and then return the buffer. *)
val (<<|) : Buffer.t -> string -> Buffer.t

(** Add a character to a buffer. *)
val (<.) : Buffer.t -> char -> unit

(** Add a character to a buffer, and then return the buffer. *)
val (<.|) : Buffer.t -> char -> Buffer.t

(*** helper operator for references ***)

(** Return a string of reference by concatenating all strings delimited
    by '.'. *)
val (!^) : string list -> string

val reference_of_echo : string list

val reference_of_global : string list

val reference_of_main : string list
