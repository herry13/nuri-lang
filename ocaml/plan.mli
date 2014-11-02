(* Author: Herry (herry13@gmail.com) *)

open Common

(** type of sequential plan **)
type sequential = Action.t array

(** type of parallel plan **)
type parallel = {
	actions : Action.t array;
	before  : SetInt.t array;
	after   : SetInt.t array
}

val sequential_of : parallel -> sequential

val json_of_sequential : sequential -> string

val parallel_of : sequential -> parallel

val json_of_parallel : parallel -> string