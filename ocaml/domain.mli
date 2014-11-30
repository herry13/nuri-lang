(** Module Domain contains the definition of the semantics' domains
    with their semantics algebras (functions to manipulate the domains).

    Module dependencies:
    - Common

    @author Herry (herry13\@gmail.com)
    @since 2014 *)

open Common

(*******************************************************************
 * semantics domain
 *******************************************************************)

(** Basic Value domain *)
type basic    = Boolean   of bool       (** Boolean domain *)
              | Int       of int        (** Integer domain *)
              | Float     of float      (** Float domain *)
              | String    of string     (** String domain *)
              | Null                    (** Null domain *)
              | Vector    of vector
              | Reference of reference

(** Vector domain *)
and vector   = basic list

(** Value domain *)
and value     = Basic  of basic
              | Store  of store
              | Global of _constraint
              | Link   of reference
              | Action of action
              | TBD
              | Unknown
              | None
              | Enum of string list
              | Lazy of func

(** Lifted-Value domain *)
and _value    = Val of value
              | Undefined

(** Cell domain *)
and cell      = ident * value

(** Store domain *)
and store     = cell list

(** Reference domain *)
and reference = ident list

(** Identifier domain *)
and ident     = string

(** Function domain : receive a store and a namespace, return a value *)
and func = store -> reference -> value

(** constraint domain *)
and _constraint = Equal        of reference * basic
                | NotEqual     of reference * basic
				| Greater      of reference * basic
				| GreaterEqual of reference * basic
				| Less         of reference * basic
				| LessEqual    of reference * basic
                | Not          of _constraint
                | Imply        of _constraint * _constraint
                | And          of _constraint list
                | Or           of _constraint list
                | In           of reference * vector
                | True
                | False

(** action domain *)
and action         = reference * parameter_type list * cost * _constraint * effect list
and parameter_type = ident * Syntax.t
and cost           = int
and effect         = reference * basic


(*******************************************************************
 * exception and error handling function
 *******************************************************************)

(** Exception for any error on semantics algebra. *)
exception Error of int * string

(** A function that raise an SfError. *)
val error : int -> string -> 'a


(*******************************************************************
 * semantics algebras for identifier and reference domains
 *******************************************************************)

(** Return a prefix of a reference. *)
val (!--) : reference -> reference

(** Concat the second reference to the last position of the first
    reference. *)
val (@++) : reference -> reference -> reference

(** Add an identifier to the last position of the reference. *)
val (@+.) : reference -> string -> reference

(** Remove a common (of both references) prefix from the first
    reference. *)
val (@--) : reference -> reference -> reference

(** 'true' if the second reference is equal or the prefix of the
    first one, otherwise 'false'. *)
val (@<=) : reference -> reference -> bool

(** 'true' if the second reference is not equal and the prefix of
    the first one, otherwise 'false'. *)
val (@<) : reference -> reference -> bool

(** Given a namespace (first), remove keyword 'root', 'parent',
    and 'this' from a reference (second). *)
val (@<<) : reference -> reference -> reference

(** Similar with '@<<' but the namespace is root. *)
val (!<<) : reference -> reference


(*******************************************************************
 * semantics algebras for store domain
 *******************************************************************)

(** Find a reference in a store and then return its value. *)
val find : store -> reference -> _value

(** Similar with 'find', but if the value is another reference
    (nested reference) then it will follow it. *)
val find_follow : store -> reference -> _value

(** Resolve a reference in a store within given namespace, and then
    return its value set '~follow:true' (default 'false') if you
    want to use 'find_follow' instead of 'find'. *)
val resolve : ?follow:bool -> store -> reference -> reference ->
              (reference * _value)

(** Add a pair identifier-value into a store if the identifier is
    exist, then the old-value will be replaced note that in the
    replacement, the position of the pair within the store must be
    maintained. *)
val put : store -> string -> value -> store

(** Add a pair reference-value into a store if the identifier is
    exist, then the old-value will be replaced note that in the
    replacement, the position of the pair within the store must be
    maintained. *)
val bind : store -> reference -> value -> store

(** Copy the content of a store to a particular location (referred
    by given reference) within given a store. *)
val copy : store -> store -> reference -> store

(** Similar with 'copy' by the location of the source store is
    referred by a reference. *)
val inherit_proto : store -> reference -> reference ->
                    reference -> store

(** Visit every element of a store to replace all lazy values. *)
val accept : store -> reference -> store -> reference -> store

(** Return a first variable that has value equal to 'value'. *)
val find_value : reference -> store -> value -> reference

(** Evaluate a function, and then return the evaluation result. *)
val eval_function : store -> reference -> (store -> reference -> value) -> value

(** Return a string of given basic value. *)
val string_of_basic_value : basic -> string


(****************************************************************
 * Expressions evaluation functions
 ****************************************************************)

(* TODO: update semantics algebra in the documentation *)

(** A binary operator that adds two operands. The result will be:
    - add Int Int -> Int
    - add Int Float -> Float
    - add Float Int -> Float
    - add Float Float -> Float
    - add String basic -> String
    - add basic String -> String *)
val add : ?store:store -> ?namespace:reference -> value -> value -> value

val equals : ?store:store -> ?namespace:reference -> value -> value -> value

val logic : ?operator:string -> ?store:store -> ?namespace:reference ->
            (bool -> bool -> bool) -> value -> value -> value

val math : ?store:store -> ?namespace:reference ->
           (int -> int -> int) -> (float -> float -> float) -> value -> value -> value

val unary : ?store:store -> ?namespace:reference -> value ->
            (value -> value) -> value

val binary : ?store:store -> ?namespace:reference ->
             (value -> value -> value) -> value -> value -> value


(*******************************************************************
 * Flat-store domain and its semantics algebras.
 *******************************************************************)

(** Flat-store domain *)
type flatstore = value MapRef.t

(** A value that represents a static-object. *)
val static_object : value

(** Convert a store into a flat-store. *)
val normalise : store -> flatstore


(*******************************************************************
 * set of values
 *******************************************************************)

(** Set of values *)
module SetValue : Set.S with type elt = value


(*******************************************************************
 * Action parameter domain.
 *******************************************************************)

(** The type of the grounded-action's parameters. *)
type ground_parameters = basic MapStr.t

(** Substitute each left-hand side reference with a reference as
    specified in the parameters table *)
val substitute_parameter_of_reference : reference -> ground_parameters ->
	reference

(** Substitute each right-hand side reference of basic value
    with a value as specified in the parameters table. *)
val substitute_parameter_of_basic_value : basic -> ground_parameters -> basic


(*******************************************************************
 * Utility functions
 *******************************************************************)

(** Remove actions and global constraints from store. *)
val to_state : store -> store

val interpolate_string : string -> store -> reference -> string
