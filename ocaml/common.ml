(* Author: Herry (herry13@gmail.com)            
   Common data structures and functions used by other modules. *)

module MapStr = Map.Make(String)

module MapRef = Map.Make ( struct
	type t = string list
	let compare = Pervasives.compare
end )

module SetRef = Set.Make ( struct
	type t = string list
	let compare = Pervasives.compare
end )

module SetInt = Set.Make ( struct
	type t = int
	let compare = Pervasives.compare
end )

let read_file file =
	let channel = open_in file in
	let n       = in_channel_length channel in
	let s       = Bytes.create n in
	really_input channel s 0 n;
	close_in channel;
	s
;;

let write_file file content =
	let channel = open_out file in
	output_string channel content;
	close_out channel
;;
