(** Module Syntax contains the Abstract Syntax Tree (AST) of
    the Nuri language with some functions to convert the AST
    or its element to string.

    Module dependencies:
    - Common

    @author Herry (herry13\@gmail.com)
    @since 2014 *)

open Common

(** Abstract Syntax Tree of the Nuri language. *)

(** core syntax **)
type nuri          = context
and  context       = AssignmentContext of assignment * context
                   | SchemaContext     of schema * context
                   | EnumContext       of enum * context
                   | TrajectoryContext of trajectory * context
                   | EmptyContext
and  block         = AssignmentBlock   of assignment * block
                   | TrajectoryBlock   of trajectory * block
                   | EmptyBlock
and  assignment    = reference * t * value
and  expression    = Basic           of basicValue
                   | Shell           of string
                   | Exp_Eager       of expression  (** Eager-evaluation expression *)
                   | Exp_Not         of expression
                   | Exp_Equal       of expression * expression
                   | Exp_And         of expression * expression
                   | Exp_Or          of expression * expression
                   | Exp_Imply       of expression * expression
                   | Exp_Add         of expression * expression
                   | Exp_MatchRegexp of expression * string
                   | Exp_IfThenElse  of expression * expression * expression
and  value         = Expression of expression
                   | Link       of reference
                   | Prototype  of super * prototype
                   | Action     of action
                   | TBD
                   | Unknown
                   | None
and  prototype     = ReferencePrototype of reference * prototype
                   | BlockPrototype     of block * prototype
                   | EmptyPrototype
and  basicValue    = Boolean   of string
                   | Int       of string
                   | Float     of string
                   | String    of string
                   | Null
                   | Vector    of vector
                   | Reference of reference
and  vector        = basicValue list
and  reference     = string list

(** schema syntax **)
and schema = string * super * block
and super  = SID of string
           | EmptySchema

(** enum syntax **)
and enum = string * string list

(** type syntax **)
and t        = T_Bool
             | T_Int
             | T_Float
             | T_String
             | T_Null
             | T_Undefined
             | T_Any
             | T_Action
             | T_Global
             | T_Enum      of string * string list
             | T_List      of t
             | T_Schema    of tSchema
             | T_Reference of tSchema
             | T_Forward   of tForward
    
and tSchema  = T_Object
             | T_RootSchema
             | T_UserSchema of string * tSchema
    
and tForward = T_LinkForward      of reference
             | T_ReferenceForward of reference

(** state-trajectory syntax **)
and trajectory = Global of _constraint

(** constraint syntax **)
and _constraint = C_Equal        of reference * basicValue
                | C_NotEqual     of reference * basicValue
				| C_Greater      of reference * basicValue
				| C_GreaterEqual of reference * basicValue
				| C_Less         of reference * basicValue
				| C_LessEqual    of reference * basicValue
                | C_Not          of _constraint
                | C_Imply        of _constraint * _constraint
                | C_And          of _constraint list
                | C_Or           of _constraint list
                | C_In           of reference * vector

(** action syntax **)
and action     = parameter list * cost * conditions * effect list
and parameter  = string * t
and cost       = Cost of string
               | EmptyCost
and conditions = Condition of _constraint
               | EmptyCondition
and effect     = reference * basicValue


(*******************************************************************
 * exception and error handling function
 *******************************************************************)

exception SyntaxError of int * string

let error code message =
    match message with
    | "" -> raise (SyntaxError (code, "[err" ^ (string_of_int code) ^ "]"))
    | _  -> raise (SyntaxError (code, "[err" ^ (string_of_int code) ^ "] - " ^ message))
;;


(*******************************************************************
 * functions to convert elements of abstract syntax tree to string
 *******************************************************************)

(** convert a Nuri abstract syntax tree into a string **)
let rec string_of nuri =
    let buf = Buffer.create 40 in
    let rec context ctx = match ctx with
        | AssignmentContext (a, c) -> assignment a; buf <. '\n'; context c
        | SchemaContext (s, c)     -> schema s; buf <. '\n'; context c
        | EnumContext (e, c)       -> enum e; buf <. '\n'; context c
        | TrajectoryContext (t, c) -> trajectory t; buf <. '\n'; context c
        | EmptyContext             -> ()

    and block b = match b with
        | AssignmentBlock (a, b) -> assignment a; buf <. '\n'; block b
        | TrajectoryBlock (t, b) -> trajectory t; buf <. '\n'; block b
        | EmptyBlock             -> ()

    and assignment (r, t, v) = reference r; buf <. ':'; _type t; value v

    and expression e = match e with
        | Basic v            -> basic_value v
                                (* TODO: use escape (\) for every backtick character *)
        | Shell s            -> buf << " `"; buf << s; buf << "`;"
        | Exp_Eager e        -> buf <. '$'; expression e
        | Exp_Equal (e1, e2) -> buf <. ' '; expression e1; buf << " = "; expression e2
        | Exp_Not e          -> buf << " not "; expression e
        | Exp_And (e1, e2)   -> buf <. ' '; expression e1; buf << " && "; expression e2
        | Exp_Or (e1, e2)    -> buf <. ' '; expression e1; buf << " || "; expression e2
        | Exp_Imply (e1, e2) -> buf <. ' '; expression e1; buf << " => "; expression e2
        | Exp_Add (e1, e2)   -> buf <. ' '; expression e1; buf << " + "; expression e2
        | Exp_IfThenElse (e1, e2, e3)   -> buf << " if "; expression e1; buf << " then ";
                                           expression e2; buf << " else "; expression e3
        | Exp_MatchRegexp (exp, regexp) -> buf <. ' '; expression exp; buf << " =~ /";
                                           buf << regexp; buf <. '/'

    and value v = match v with
        | Expression e       -> buf <. ' '; expression e; buf <. ';'
        | Link lr            -> buf <. ' '; reference lr; buf <. ';'
        | Prototype (sid, p) -> super_schema sid; prototype p
        | Action a           -> action a
        | TBD                -> buf << " TBD"
        | Unknown            -> buf << " Unknown"
        | None               -> buf << " None"

    and prototype proto = match proto with
        | ReferencePrototype (r, p) -> buf << " extends "; reference r; prototype p
        | BlockPrototype (b, p)     -> buf << " extends {\n"; block b; prototype p
        | EmptyPrototype            -> ()

    and basic_value bv = match bv with
        | Boolean x | Int x | Float x | String x -> buf << x
        | Null        -> buf << "null"
        | Vector vec  -> buf <. '['; vector vec; buf <. ']'
        | Reference r -> buf <. ' '; reference r

    and vector vec = match vec with
        | []           -> ()
        | head :: []   -> basic_value head
        | head :: tail -> basic_value head; buf <. ','; vector tail

    and reference r = buf << !^r

    and _type t = match t with
        | T_Bool           -> buf << "bool"
        | T_Int            -> buf << "int"
        | T_Float          -> buf << "float"
        | T_String         -> buf << "string"
        | T_Null           -> buf << "null"
        | T_Undefined      -> buf << "undefined"
        | T_Any            -> buf << "any"
        | T_Action         -> buf << "action"
        | T_Global         -> buf << "global"
        | T_Enum (id, _)   -> buf << id
        | T_List t         -> buf << "[]"; _type t
        | T_Schema t       -> type_schema t
        | T_Reference t    -> buf <. '*'; type_schema t
        | T_Forward T_LinkForward r      -> buf << "~"; reference r
        | T_Forward T_ReferenceForward r -> buf << "~*"; reference r

    and type_schema t = match t with
        | T_Object             -> buf << "object"
        | T_RootSchema         -> ()
        | T_UserSchema (id, _) -> buf << id;

    and super_schema ss = match ss with
        | SID id      -> buf << " isa "; buf << id
        | EmptySchema -> ()

    and schema (sid, super, b) = buf << "schema "; buf << sid; super_schema super;
                                 buf << " {\n"; block b; buf <. '}'

    and enum (id, symbols) = buf << "enum "; buf << id; buf << " {\n  ";
                             buf << (String.concat ",  " symbols); buf << "\n}"

    (*** constraints ***)

    and trajectory t = match t with
        | Global g -> global g

    and global g = buf << "global "; constraints g; buf <. '\n'

    and constraints c = match c with
        | C_Equal (r, bv)       -> reference r; buf << " = "; basic_value bv; buf <. ';'
        | C_NotEqual (r, bv)    -> reference r; buf << " != "; basic_value bv; buf <. ';'
        | C_Not c               -> buf << "not "; constraints c;
        | C_Imply (c1, c2)      -> buf << "if "; constraints c1; buf << " then "; constraints c2
        | C_And cs              -> buf << "{\n"; List.iter (fun c -> constraints c; buf <. '\n') cs; buf <. '}'
        | C_Or cs               -> buf << "(\n"; List.iter (fun c -> constraints c; buf <. '\n') cs; buf <. ')'
        | C_In (r, vec)         -> reference r; buf << " in "; vector vec; buf <. ';'
        | C_Greater (r, v)      -> reference r; buf << " > "; basic_value v; buf <. ';'
        | C_GreaterEqual (r, v) -> reference r; buf << " >= "; basic_value v; buf <. ';'
        | C_Less (r, v)         -> reference r; buf << " < "; basic_value v; buf <. ';'
        | C_LessEqual (r, v)    -> reference r; buf << " <= "; basic_value v; buf <. ';'

    and effect (r, v) = reference r; buf << " = "; basic_value v; buf <. ';'

    and effects effs = buf << "{\n"; List.iter (fun e -> effect e; buf <. '\n') effs; buf <. '}'

    and conditions c = match c with
        | EmptyCondition -> ()
        | Condition c    -> buf << "conditions "; constraints c;

    and cost c = match c with
        | EmptyCost -> ()
        | Cost n    -> buf << "cost = "; buf << n; buf <. ';'

    and parameter (id, t) = buf << id; buf <. ':'; _type t

    and parameters params = match params with
        | [] -> ()
        | head :: [] -> buf <. '('; parameter head; buf <. ')'
        | head :: tail -> buf <. '('; parameter head; List.iter (fun p -> buf <. ','; parameter p) tail; buf <. ')'

    and action (params, c, cond, effs) : unit =
        buf << "def "; parameters params; buf << " {\n"; cost c; conditions cond; effects effs; buf <. '}'

    in
    context nuri;
    Buffer.contents buf
;;

(** convert a Nuri type into a string **)
let string_of_type t =
    let buf = Buffer.create 5 in
    let rec _type t = match t with
        | T_Bool           -> buf << "bool"
        | T_Int            -> buf << "int"
        | T_Float          -> buf << "float"
        | T_String         -> buf << "string"
        | T_Null           -> buf << "null"
        | T_Undefined      -> buf << "undefined"
        | T_Any            -> buf << "any"
        | T_Action         -> buf << "action"
        | T_Global         -> buf << "global"
        | T_Enum (id, _)   -> buf << "enum~"; buf << id
        | T_List t         -> buf << "[]"; _type t
        | T_Schema t       -> type_schema t
        | T_Reference t    -> buf <. '*'; type_schema t
        | T_Forward T_LinkForward r      -> buf << "forward~"; buf << !^r
        | T_Forward T_ReferenceForward r -> buf << "forward~*"; buf << !^r

    and type_schema t = match t with
        | T_Object                 -> buf << "object"
        | T_RootSchema             -> ()
        | T_UserSchema (id, super) -> buf << id; buf <. '<'; type_schema super

    in
    _type t;
    Buffer.contents buf
;;
