(*******************************************************************
 * module Common
 * Author: Herry (herry13@gmail.com)
 *
 * Common data structures and functions used by other modules.
 *******************************************************************)

(** a map where strings are the key **)
module MapStr = Map.Make(String)

(** a map where lists of strings (references) are the key **)
module MapRef = Map.Make
(
    struct
	    type t = string list
	    let compare = Pervasives.compare
    end
)

(** a set of references **)
module SetRef = Set.Make
(
    struct
	    type t = string list
	    let compare = Pervasives.compare
    end
)

(** a set of integers **)
module SetInt = Set.Make
(
    struct
	    type t = int
	    let compare = Pervasives.compare
    end
)

(** read a string from given filename **)
let read_file file =
	let channel = open_in file in
	let n       = in_channel_length channel in
	let s       = String.create n in
	really_input channel s 0 n;
	close_in channel;
	s
;;

(** write a string to given filename **)
let write_file file content =
	let channel = open_out file in
	output_string channel content;
	close_out channel
;;

(** execute 'command' using 'Unix' module and then return the output from STDOUT **)
let get_process_output command =
    let in_channel = Unix.open_process_in command in
    let buf = Buffer.create 17 in
    try 
        Buffer.add_string buf (input_line in_channel);
        while true do
            let line = input_line in_channel in
            Buffer.add_char buf '\n';
            Buffer.add_string buf line
        done;
        Buffer.contents buf
    with End_of_file -> match Unix.close_process_in in_channel with
        | Unix.WEXITED _ -> Buffer.contents buf 
        | _ -> raise (Failure ("Command '" ^ command ^ "' was exiting with error."))
;;


(*** helper operators for string buffer **)

(** add a string to a buffer **)
let (<<) buffer str = Buffer.add_string buffer str ;;

(** add a character to a buffer **)
let (<.) buffer character = Buffer.add_char buffer character ;;


(*** helper operator for references ***)

(** return a string of reference by concatenating all strings delimited by '.' **)
let (!^) r = String.concat "." r
