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
val apply : store -> _constraint -> bool

(** convert the constraints into DNF formula **)
val dnf_of : _constraint -> Variable.ts -> Type.map -> _constraint

(** substitute free variables of given constraints **)
val substitute_free_variables_of : _constraint -> basic MapStr.t -> _constraint

val global_of : Type.map -> Domain.flatstore -> Variable.ts ->
	            (_constraint * _constraint list * Variable.ts)
