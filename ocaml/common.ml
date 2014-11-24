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
	let s       = String.create n in
	really_input channel s 0 n;
	close_in channel;
	s
;;

let write_file file content =
	let channel = open_out file in
	output_string channel content;
	close_out channel
;;

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
        | Unix.WEXITED code when code = 0 -> Buffer.contents buf 
        | _ -> raise (Failure ("Command '" ^ command ^ "' was exiting with error."))
;;
