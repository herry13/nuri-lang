(** Module Syntax contains the Abstract Syntax Tree (AST) of
    the Nuri language with some functions to convert the AST
    or its element to string.

    Module dependencies:
    - Common

    @author Herry (herry13\@gmail.com)
    @since 2014
*)


(** Abstract Syntax Tree of the Nuri language. *)

(* core syntax *)
type nuri          = context

and  context       = AssignmentContext of assignment * context
                     (** Assignment and next context *)
                   | SchemaContext     of schema * context
                     (** Schema and next context *)
                   | EnumContext       of enum * context
                     (** Enum and next context *)
                   | TrajectoryContext of trajectory * context
                     (** Trajectory constraints and next context *)
                   | EmptyContext
                     (** Empty context *)

and  block         = AssignmentBlock   of assignment * block
                     (** Assignment and next block *)
                   | TrajectoryBlock   of trajectory * block
                     (** Trajector constraints and next block *)
                   | EmptyBlock
                     (** Empty block *)

and  assignment    = TypeValue of reference * t * value
                     (** Variable, type, and value *)
                   | RefIndexValue of reference * string list * value

and  expression    = Basic           of basic_value (** Basic value *)
                   | Exp_Index       of expression * string list
                   | Shell           of string
                     (** External-command-value (via shell) *)
                   | Exp_Eager       of expression
                     (** Eager-evaluation expression *)
                   | Exp_IString     of string
                     (** String with variable interpolation *)
                   | Exp_Not         of expression (** Negation *)
                   | Exp_Equal       of expression * expression
                     (** Equality *)
                   | Exp_NotEqual    of expression * expression
                     (** Inequality *)
                   | Exp_And         of expression * expression
                     (** Conjunction *)
                   | Exp_Or          of expression * expression
                     (** Disjunction *)
                   | Exp_Imply       of expression * expression
                     (** Implication *)
                   | Exp_MatchRegexp of expression * string
                     (** String pattern-matching *)
                   | Exp_Add         of expression * expression
                     (** Numeric addition or string concatenation *)
                   | Exp_Subtract    of expression * expression
                     (** Numeric subtraction *)
                   | Exp_Multiply    of expression * expression
                     (** Numeric multiplication *)
                   | Exp_Divide      of expression * expression
                     (** Numeric division *)
                   | Exp_Modulo      of expression * expression
                     (** Numeric modulo *)
                   | Exp_IfThenElse  of expression * expression * expression
                     (** Conditional branch *)

and  value         = Expression of expression
                   | Link       of reference          (** Link-reference *)
                   | Prototype  of super * prototype  (** Prototype-object *)
                   | Action     of action
                   | TBD        (** To Be Defined *)
                   | Unknown    (** Unknown : used when the variable's value
                                    is indeterminate *)
                   | None       (** None : the variable is not exist *)

and  prototype     = ReferencePrototype of reference * prototype 
                     (** Reference prototype *)
                   | BlockPrototype     of block * prototype
                     (** Anonymous prototype *)
                   | EmptyPrototype (** No prototype *)

and  basic_value   = Boolean   of string
                   | Int       of string
                   | Float     of string
                   | String    of string
                   | Null
                   | Vector    of vector
                   | Reference of reference
                   | RefIndex  of reference * string list

and  vector        = basic_value list

and  reference     = string list

(* schema syntax *)
and schema = string * super * block  (** User defined schema *)
and super  = SID of string
           | EmptySchema

(* enum syntax *)
and enum = string * string list  (** User-defined Enum *)

(* type syntax *)
and t = T_Bool        (** bool *)
      | T_Int         (** int *)
      | T_Float       (** float *)
      | T_String      (** string *)
      | T_Null        (** null-type *)
      | T_Undefined   (** It is used when the type is indeterminate. *)
      | T_Any         (** Value 'Unknown' & 'None' has type 'T_Any' *)
      | T_Action      (** Every action has this type *)
      | T_Constraint  (** The elements of global constraints has this type. *)
      | T_Enum      of string * string list  (** Enum *)
      | T_Symbol    of string
      | T_List      of t          (** Every vector has this type *)
      | T_Schema    of t_object   (** Static schema *)
      | T_Object    of t_object   (** Object *)
      | T_Reference of t_object   (** Reference of object *)
      | T_Forward   of t_forward  (** For forward references *)

and t_object = T_Plain                      (** Plain built-in object *)
             | T_User of string * t_object  (** User-defined schema *)

and t_forward = T_Link      of reference (** Forward link-reference *)
              | T_Ref       of reference (** Forward (data) reference *)

(* state-trajectory syntax *)
and trajectory = Global of _constraint

(* constraint syntax *)
and _constraint = C_Equal        of reference * basic_value
                | C_NotEqual     of reference * basic_value
				| C_Greater      of reference * basic_value
				| C_GreaterEqual of reference * basic_value
				| C_Less         of reference * basic_value
				| C_LessEqual    of reference * basic_value
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
and effect     = reference * basic_value


(*******************************************************************
 * functions to convert elements of abstract syntax tree to string
 *******************************************************************)

(** Convert an abstract syntax tree into a string. *)
val string_of : nuri -> string

(** Convert a type into a string. *)
val string_of_type : t -> string
