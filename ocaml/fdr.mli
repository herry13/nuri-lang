(* Author: Herry (herry13@gmail.com) *)

type t = {
	variables : Variable.ts;
	actions   : Action.ts;
	global    : Domain.constraint_
}

val of_nuri : Syntax.nuri -> Syntax.nuri -> t

val of_files : string -> string -> t

val string_of : t -> string

val variables_of : t -> Variable.ts

val actions_of : t -> Action.ts

val to_nuri_plan : string -> t -> Plan.sequential

val to_raw_nuri_plan : string -> t -> Plan.sequential
