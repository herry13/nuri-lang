(** Module Constraint contains functions that operates over constraints
    domain.

    Module dependencies:
    - Common
    - Domain

    @author Herry (herry13\@gmail.com)
    @since 2014
*)

open Common
open Domain

(** evaluate the constraints based a given store **)
val apply : store -> constraint_ -> bool

(** convert the constraints into DNF formula **)
val dnf_of : constraint_ -> Variable.ts -> Type.environment -> constraint_

(** substitute free variables of given constraints **)
val substitute_free_variables_of : constraint_ -> basic MapStr.t -> constraint_

val global_of : Type.environment -> Domain.flatstore -> Variable.ts ->
	            (constraint_ * constraint_ list * Variable.ts)
