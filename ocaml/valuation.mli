(* Author: Herry (herry13@gmail.com) *)

open Syntax

val nuriSpecificationFirstPass : nuri -> Domain.store

val nuriSpecificationSecondPass : ?main:string list -> nuri -> Domain.store

val nuriSpecificationThirdPass : ?main:string list -> nuri -> Domain.store

(** default valuation function (3 passes) **)
val nuriSpecification : ?main:string list -> nuri -> Domain.store
