(* Author: Herry (herry13@gmail.com) *)

(*******************************************************************
 * abstract syntax tree
 *******************************************************************)
type nuri          = context
and  context       = AssignmentContext of assignment * context
                   | SchemaContext     of schema * context
                   | EnumContext       of enum * context
                   | TrajectoryContext of trajectory * context
                   | EmptyContext
and  block         = AssignmentBlock of assignment * block
                   | TrajectoryBlock of trajectory * block
                   | EmptyBlock
and  assignment    = reference * t * value
and  expression    = Basic      of basicValue
                   | Shell      of string
                   | Equal      of expression * expression
                   | Exp_Not    of expression
                   | Add        of expression * expression
                   | IfThenElse of expression * expression * expression
and  value         = Expression of expression
                   | Link       of reference
                   | Prototype  of superSchema * prototype
                   | Action     of action
                   | TBD
                   | Unknown
                   | Nothing
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
and schema      = string * superSchema * block
and superSchema = SID of string
                | EmptySchema

(** enum syntax **)
and enum = string * string list

(** type syntax **)
and t        = TBool
             | TInt
             | TFloat
             | TString
             | TNull
             | TUndefined
             | TAny
             | TAction
             | TGlobal
             | TEnum    of string * string list
             | TList    of t
             | TSchema  of tSchema
             | TRef     of tSchema
             | TForward of reference * tForward
    
and tSchema  = TObject
             | TRootSchema
             | TUserSchema of string * tSchema
    
and tForward = TLinkForward
             | TRefForward

(** trajectory **)
and trajectory = Global of _constraint

(** constraint syntax **)
and _constraint = Eq           of reference * basicValue
                | Ne           of reference * basicValue
				| Greater      of reference * basicValue
				| GreaterEqual of reference * basicValue
				| Less         of reference * basicValue
				| LessEqual    of reference * basicValue
                | Not          of _constraint
                | Imply        of _constraint * _constraint
                | And          of _constraint list
                | Or           of _constraint list
                | In           of reference * vector

(** action syntax **)
and action     = parameter list * cost * conditions * effect list
and parameter  = string * t
and cost       = Cost of string
               | EmptyCost
and conditions = Condition of _constraint
               | EmptyCondition
and effect     = reference * basicValue

(*******************************************************************
 * functions to convert elements of abstract syntax tree to string
 *******************************************************************)

val string_of_nuri : nuri -> string

val string_of_type : t -> string

val json_of_nuri : nuri -> string

val nuri_of_json : string -> nuri
