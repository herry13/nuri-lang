(** Module Json contains the functions to serialise/deserialise any type or
    value to/from JSON format.

    Module dependencies
    - Common
    - Syntax
    - Domain

    @author Herry (herry13\@gmail.com)
    @since 2014
*)

open Common
open Domain


(*******************************************************************
 * Exception and its helper function
 *******************************************************************)

(** A JsonError exception is thrown whenever there is an error during
    the serialisation/deserialisation process.
    @param int the error code
    @param string the error message
*)
exception JsonError of int * string

let error code msg =
    raise (JsonError (code, "[err" ^ (string_of_int code) ^ "] " ^ msg))
;;



(*******************************************************************
 * Function to generate JSON of a type-syntax
 *******************************************************************)

let rec of_type t = match t with
    | Syntax.T_Bool               -> "bool"
    | Syntax.T_Int                -> "int"
    | Syntax.T_Float              -> "float"
    | Syntax.T_String             -> "string"
    | Syntax.T_Null               -> "null"
    | Syntax.T_Any                -> "any"
    | Syntax.T_Action             -> "action"
    | Syntax.T_Constraint         -> "global"
    | Syntax.T_Enum (id, _)       -> "{}" ^ id
    | Syntax.T_List t             -> "[]" ^ (of_type t)
    | Syntax.T_Schema t_object    -> "schema:" ^ (of_type_object t_object)
    | Syntax.T_Object t_object    -> of_type_object t_object
    | Syntax.T_Reference t_object -> "*" ^ (of_type_object t_object)
    | _                           -> error 1302 "invalid type"

and of_type_object = function
    | Syntax.T_Plain        -> "object"
    | Syntax.T_User (id, _) -> id
;;


(*******************************************************************
 * Helper functions to generate JSON of basic value
 *******************************************************************)

let rec json_of_basic_value buf v =
    let json_of_string str : unit =
        buf <. '"' ;
        String.iter (fun c ->
            match c with
            | '"'    -> buf << "\\\""
            | '/'    -> buf << "\\/"
            | '\\'   -> buf << "\\\\"
            | '\b'   -> buf << "\\b"
            | '\012' -> buf << "\\f"
            | '\n'   -> buf << "\\n"
            | '\r'   -> buf << "\\r"
            | '\t'   -> buf << "\\t"
            | _      -> buf <. c
        ) str;
        buf <. '"'
    in
    let json_vector vector =
        let rec iter vec = match vec with
            | [] -> ()
            | head :: [] -> json_of_basic_value buf head
            | head :: tail ->
                begin
                    json_of_basic_value buf head;
                    buf <. ',';
                    iter tail
                end
        in
        buf <. '[';
        iter vector;
        buf <. ']'
    in
    match v with
    | Boolean b -> buf << (string_of_bool b)
    | Int i     -> buf << (string_of_int i)
    | Float f   ->
        begin
            let s = string_of_float f in
            buf << s;
            if s.[(String.length s) - 1] = '.' then buf <. '0'
            else ()
        end
    | String s    -> json_of_string s
    | Null        -> buf << "null"
    | Reference r ->
        begin
            buf << "\"$";
            buf << !^r;
            buf <. '"'
        end
    | Vector vec -> json_vector vec
    | Symbol sym ->
        begin
            buf << "§";
            buf << sym
        end
;;


(*******************************************************************
 * Functions to generate JSON of value, constraint, or action
 *******************************************************************)

let of_basic_value v =
    let buf = Buffer.create 42 in
    json_of_basic_value buf v;
    Buffer.contents buf
;;

let rec json_constraint buf c =
    match c with
    | Equal (r, v) -> 
        begin
            constraint_left_side buf "=" r;
            json_of_basic_value buf v;
            buf <. ']'
        end
    | NotEqual (r, v) ->
        begin
            constraint_left_side buf "!=" r;
            json_of_basic_value buf v;
            buf <. ']'
        end
    | Greater (r, v) ->
        begin
            constraint_left_side buf ">" r;
            json_of_basic_value buf v;
            buf <. ']'
        end
    | GreaterEqual (r, v) -> 
        begin
            constraint_left_side buf ">=" r;
            json_of_basic_value buf v;
            buf <. ']'
        end
    | Less (r, v) ->
        begin
            constraint_left_side buf "<" r;
            json_of_basic_value buf v;
            buf <. ']'
        end
    | LessEqual (r, v) ->
        begin
            constraint_left_side buf "<=" r;
            json_of_basic_value buf v;
            buf <. ']'
        end
    | In (r, vec) ->
        begin
            constraint_left_side buf "in" r;
            json_of_basic_value buf (Vector vec);
            buf <. ']'
        end
    | Not c ->
        begin
            constraint_label buf "not";
            buf <. ',';
            json_constraint buf c;
            buf <. ']'
        end
    | Imply (c1, c2) ->
        begin
            constraint_label buf "imply";
            buf <. ',';
            json_constraint buf c1;
            buf <. ',';
            json_constraint buf c1;
            buf <. ']'
        end
    | And [] | Or  [] -> buf << "true"
    | And cs ->
        begin
            constraint_label buf "and";
            List.iter (fun c ->
                buf <. ',';
                json_constraint buf c
            ) cs;
            buf <. ']'
        end
    | Or cs ->
        begin
            constraint_label buf "or";
            List.iter (fun c ->
                buf <. ',';
                json_constraint buf c
            ) cs;
            buf <. ']'
        end
    | True -> buf << "true"
    | False -> buf << "false"
and constraint_label buf operator =
    buf << "[\"";
    buf << operator;
    buf <. '"'
and constraint_left_side buf operator r =
    constraint_label buf operator;
    buf << ",\"";
    buf << !^r;
    buf << "\","
;;

let of_constraint c =
    let buf = Buffer.create 42 in
    json_constraint buf c;
    Buffer.contents buf
;;

let add_ident ?_type:(t="") buf id =
    buf <. '"';
    buf << id;
    if t <> "" then begin
        buf <. ':';
        buf << t
    end;
    buf << "\":"

let json_action buf (_, parameters, cost, conditions, effects) =
    (* parameters *)
    let rec json_parameters ps = match ps with
        | [] -> ()
        | (id, t) :: [] ->
            begin
                add_ident buf id;
                buf <. '"';
                buf << (of_type t);
                buf <. '"'
            end
        | (id, t) :: tail ->
            begin
                add_ident buf id;
                buf <. '"';
                buf << (of_type t);
                buf << "\",";
                json_parameters tail
            end
    in
    buf << "{\".type\":\"action\",\"parameters\":{";
    json_parameters parameters;
    buf << "},\"cost\":";

    (* cost *)
    buf << (string_of_int cost);

    (* conditions *)
    buf << ",\"conditions\":";
    json_constraint buf conditions;

    (* effects *)
    let rec json_effects effs = match effs with
        | [] -> ()
        | (r, v) :: [] ->
            begin
                constraint_left_side buf "=" r;
                json_of_basic_value buf v;
                buf <. ']'
            end
        | (r, v) :: tail ->
            begin
                constraint_left_side buf "=" r;
                json_of_basic_value buf v;
                buf <. ',';
                json_effects tail;
                buf <. ']'
            end
    in
    buf << ",\"effects\":";
    buf <. '[';
    json_effects effects;
    buf << "]}"
;;

let rec of_value ?no_lazy:(no_lazy=true) value =
    let buf = Buffer.create 42 in
    begin match value with
    | Basic bv -> json_of_basic_value buf bv
    | Link r ->
        begin
            buf << "\"§";
            buf << !^r;
            buf <. '"'
        end
    | TBD -> buf << "\"$TBD\""
    | Unknown -> buf << "\"$unknown\""
    | None -> buf << "\"$none\""
    | Store child -> buf << "{}"
    | Global c -> json_constraint buf c
    | Action a -> json_action buf a
    | Lazy _ ->
        if no_lazy then
            error 1305 "Lazy value has not been evaluated."
        else
            buf << "lazy"
    end;
    Buffer.contents buf
;;

let of_store typeEnv store =
    let buf = Buffer.create 42 in
    let json_variable_type ns id =
        let r = (@+.) ns id in
        match MapRef.find r typeEnv with
        | Syntax.T_Undefined -> error 1303 ("type of " ^ !^r ^ " is undefined")
        | t -> of_type t
    in
    let set_json_object_type ns id =
        let r = (@+.) ns id in
        match MapRef.find r typeEnv with
        | Syntax.T_Undefined -> error 1304 ("type of " ^ !^r ^ " is undefined")
        | Syntax.T_Schema t ->
            begin
                buf << "\".type\":\"schema\"";
                match t with
                | Syntax.T_Plain -> ()
                | Syntax.T_User (id, _) ->
                    begin
                        buf << "\".super\":\"";
                        buf << id;
                        buf <. '"'
                    end
            end
        | t ->
            begin
                buf << "\".type\":\"";
                buf << (of_type t);
                buf <. '"'
            end
    in
    let rec json_store ns s = match s with
        | [] -> ()
        | (id, v) :: tail ->
            begin
                buf <. ',';
                json_cell ns id v;
                json_store ns tail
            end
    and json_cell ns id v = match v with
        | Basic Null ->
            begin
                add_ident ~_type:(json_variable_type ns id) buf id;
                json_of_basic_value buf Null
            end
        | Basic (Vector vec) ->
            begin
                add_ident ~_type:(json_variable_type ns id) buf id;
                json_of_basic_value buf (Vector vec)
            end
        | Basic bv ->
            begin
                add_ident buf id;
                json_of_basic_value buf bv
            end
        | Link r ->
            begin
                add_ident buf id;
                buf << "\"§";
                buf << !^r;
                buf <. '"'
            end
        | TBD ->
            begin
                add_ident ~_type:(json_variable_type ns id) buf id;
                buf << "\"$TBD\""
            end
        | Unknown ->
            begin
                add_ident ~_type:(json_variable_type ns id) buf id;
                buf << "\"$unknown\""
            end
        | None ->
            begin
                add_ident ~_type:(json_variable_type ns id) buf id;
                buf << "\"$none\""
            end
        | Store child ->
            begin
                add_ident buf id;
                buf <. '{';
                set_json_object_type ns id;
                json_store ((@+.) ns id) child;
                buf <. '}'
            end
        | Global c ->
            begin
                add_ident buf id;
                json_constraint buf c
            end
        | Action a ->
            begin
                add_ident buf id;
                json_action buf a
            end
        | Lazy _ ->
            error 1306 (!^ns ^ "." ^ id ^ "Lazy value has not been evaluated.")
    in
    buf << "{\".type\":\"object\"";
    json_store [] store;
    buf <. '}';
    Buffer.contents buf
;;

let of_flatstore flatstore =
    let buf = Buffer.create 42 in
    let first = ref true in
    buf <. '{';
    MapRef.iter (
        fun r v ->
            if !first then begin
                buf <. '\n';
                first := false
            end else begin
                buf << ",\n"
            end;
            buf << "  \"";
            buf << !^r;
            buf << "\": ";
            buf << (of_value v);
    ) flatstore;
    buf << "\n}";
    Buffer.contents buf
;;


(**
 * TODO: implement this function that does deserialisation from JSON to Nuri.
 *)
let to_store json =
    let typeEnv = MapRef.empty in
    let store = [] in
(*
    let reference_separator = Str.regexp "\\." in
    let rec make_vector acc vec =
        match vec with
        | []           -> acc
        | head :: tail -> (
                match nuri_of head with
                | Basic bv -> (make_vector (bv :: acc) tail)
                | _        -> error 1035 ""
            )
    and make_store acc s =
        match s with
        | []              -> acc
        | (id, v) :: tail -> make_store ((id, (nuri_of v)) :: acc) tail
    and nuri_of = function
        | `String str when (String.length str) > 2 &&
                           str.[0] = '$' && str.[1] = '.' ->
            Basic (Ref (List.tl (Str.split reference_separator
                str)))
        | `String str -> Basic (String str)
        | `Bool b     -> Basic (Boolean b)
        | `Float f    -> Basic (Float f)
        | `Int i      -> Basic (Int i)
        | `Null       -> Basic Null
        | `List vec   -> Basic (Vector (make_vector [] vec))
        | `Assoc s    -> Store (make_store [] s)
    in
    let _ = nuri_of (Yojson.Basic.from_string json) in
*)
    (typeEnv, store)
