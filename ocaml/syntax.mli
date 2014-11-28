(** Module Syntax contains the Abstract Syntax Tree (AST) of
    the Nuri language with some functions to convert the AST
    or its element to string.

    Module dependencies:
    - Common

    @author Herry (herry13\@gmail.com)
    @since 2014 *)


(** Abstract Syntax Tree of the Nuri language. *)

(* core syntax *)
type nuri          = context

and  context       = AssignmentContext of assignment * context  (** Assignment and next context *)
                   | SchemaContext     of schema * context      (** Schema and next context *)
                   | EnumContext       of enum * context        (** Enum and next context *)
                   | TrajectoryContext of trajectory * context  (** Trajectory constraints and next context *)
                   | EmptyContext                               (** Empty context *)

and  block         = AssignmentBlock   of assignment * block    (** Assignment and next block *)
                   | TrajectoryBlock   of trajectory * block    (** Trajector constraints and next block *)
                   | EmptyBlock                                 (** Empty block *)

and  assignment    = reference * t * value  (** Variable, user-defined type, and value *)

and  expression    = Basic           of basicValue              (** Basic value *)
                   | Shell           of string                  (** External-command-value (via shell) *)
                   | Exp_Eager       of expression              (** Eager-evaluation expression *)
                   | Exp_Not         of expression              (** Negation (unary) *)
                   | Exp_Equal       of expression * expression (** Equality (binary) *)
                   | Exp_And         of expression * expression (** Conjunction (binary) *)
                   | Exp_Or          of expression * expression (** Disjunction (binary) *)
                   | Exp_Imply       of expression * expression (** Implication (binary) *)
                   | Exp_MatchRegexp of expression * string     (** Matching (binary) *)
                   | Exp_Add         of expression * expression (** Addition (binary) *)
                   | Exp_Subtract    of expression * expression
                   | Exp_Multiply    of expression * expression
                   | Exp_Divide      of expression * expression
                   | Exp_Modulo      of expression * expression
                   | Exp_IfThenElse  of expression * expression * expression (** Conditional (ternary) *)

and  value         = Expression of expression
                   | Link       of reference          (** Link-reference *)
                   | Prototype  of super * prototype  (** Prototype-object *)
                   | Action     of action
                   | TBD        (** To Be Defined *)
                   | Unknown    (** Unknown : used when the variable's value is indeterminate *)
                   | None       (** None : the variable is not exist *)

and  prototype     = ReferencePrototype of reference * prototype    (** Reference prototype *)
                   | BlockPrototype     of block * prototype        (** Anonymous prototype *)
                   | EmptyPrototype                                 (** No prototype *)

and  basicValue    = Boolean   of string
                   | Int       of string
                   | Float     of string
                   | String    of string
                   | Null
                   | Vector    of vector
                   | Reference of reference

and  vector        = basicValue list

and  reference     = string list

(* schema syntax *)
and schema = string * super * block     (** User defined schema *)
and super  = SID of string
           | EmptySchema

(* enum syntax *)
and enum = string * string list     (** User-defined Enum *)

(* type syntax *)
and t        = T_Bool           (** bool *)
             | T_Int            (** int *)
             | T_Float          (** float *)
             | T_String         (** string *)
             | T_Null           (** null-type *)
             | T_Undefined      (** It is used when a reference's type is indeterminate. *)
             | T_Any            (** Value 'Unknown' & 'None' has type 'T_Any' *)
             | T_Action         (** Every action has this type *)
             | T_Global         (** The global constraints *)
             | T_Enum      of string * string list  (** Enum *)
             | T_List      of t                     (** Every vector has this type *)
             | T_Schema    of tSchema               (** For object or schema *)
             | T_Reference of tSchema               (** Reference *)
             | T_Forward   of tForward              (** For forward references *)
    
and tSchema  = T_Object                         (** Plain built-in object *)
             | T_RootSchema                     (** Every schema is the sub-type of this *)
             | T_UserSchema of string * tSchema (** User-defined schema *)
    
and tForward = T_LinkForward      of reference  (** Forward link-reference *)
             | T_ReferenceForward of reference  (** Forward (data) reference *)

(* state-trajectory syntax *)
and trajectory = Global of _constraint

(* constraint syntax *)
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

(* action syntax *)
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

(** Convert a Nuri abstract syntax tree into a string. *)
val string_of : nuri -> string

(** Convert a Nuri type into a string. *)
val string_of_type : t -> string
