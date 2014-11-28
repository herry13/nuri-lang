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
 * helpers
 *******************************************************************)

(** Exception for any error on semantics algebra. *)
exception Error of int * string

(** A function that raise an SfError. *)
let error code message =
    if message = "" then
        raise (Error (code, "[err" ^ (string_of_int code) ^ "]"))
    else
        raise (Error (code, "[err" ^ (string_of_int code) ^ "] " ^
            message)) ;;


(*******************************************************************
 * semantics algebras for identifier and reference domains
 *******************************************************************)

(** Return a prefix of a reference. *)
let (!^) r = String.concat "." r

(** Concat the second reference to the last position of the first
    reference. *)
let rec (!--) reference =
    match reference with
    | []           -> []
    | head :: tail -> if tail = [] then []
                      else head :: (!-- tail)
;;

(** Add an identifier to the last position of the reference. *)
let (@++) reference1 reference2 = List.append reference1 reference2 ;;

(** Remove a common (of both references) prefix from the first
    reference. *)
let (@+.) reference identifier = reference @++ [identifier] ;;

(** Remove a common (of both references) prefix from the first
    reference. *)
let rec (@--) reference1 reference2 =
    if reference1 = [] then []
    else if reference2 = [] then reference1
    else if (List.hd reference1) = (List.hd reference2)
         then (List.tl reference1) @-- (List.tl reference2)
    else reference1
;;

(** 'true' if the second reference is equal or the prefix of the
    first one, otherwise 'false'. *)
let (@<=) reference1 reference2 = (reference1 @-- reference2) = [] ;;

(** 'true' if the second reference is not equal and the prefix of
    the first one, otherwise 'false'. *)
let (@<) reference1 reference2 =
    (reference1 @<= reference2) && not (reference1 = reference2)
;;

(** Given a namespace (first), remove keyword 'root', 'parent',
    and 'this' from a reference (second). *)
let rec (@<<) namespace reference =
    match reference with
    | [] -> namespace
    | "this" :: rs -> namespace @<< rs
    | "root" :: rs -> [] @<< rs
    | "parent" :: rs ->
        if namespace = [] then
            error 501 ("invalid reference " ^ !^reference)
        else
            (!-- namespace) @<< rs
    | id :: rs -> (namespace @+. id) @<< rs
;;

(** Similar with '@<<' but the namespace is root. *)
let (!<<) reference = [] @<< reference ;;


(*******************************************************************
 * semantics algebras for store domain
 *******************************************************************)

(** Find a reference in a store and then return its value. *)
let rec find store reference : _value =
    match (store, reference) with
    | _, []                      -> Val (Store store)
    | [], _                      -> Undefined
    | (ids,vs) :: tail, id :: rs ->
        if ids = id then
            if rs = [] then Val vs
            else
                match vs with
                | Store child -> find child rs
                | _           -> Undefined
        else find tail reference
;;

(* TODO: add this into the document of the formal semantics *)
(** Similar with 'find', but if the value is another reference
    (nested reference) then it will follow it. *)
let find_follow store reference : _value =
    let rec search s r = match s, r with
        | _, [] -> Val (Store s)
        | [], _ -> Undefined
        | (ids, vs) :: tail, id :: rs ->
            if id = ids then
                match vs, rs with
                | _, [] -> Val vs
                | Store child, _ -> search child rs
                | Basic (Reference r), _ -> search store (r @++ rs)
                | _ -> Undefined
            else
                search tail r
    in
    search store reference
;;

(* TODO: update documentation of the formal semantics *)
(** Resolve a reference in a store within given namespace, and then
    return its value set '~follow:true' (default 'false') if you
    want to use 'find_follow' instead of 'find'. *)
let rec resolve ?follow:(ff=false) store namespace reference =
    let search s r = if ff then find_follow s r else find s r in
    match reference with
    | "root" :: rs   -> ([], search store !<<rs)
    | "parent" :: rs ->
        if namespace = [] then
            error 502 ("Invalid reference: " ^ !^reference)
        else
            (!-- namespace, search store !<<(!--namespace @++ rs))
    | "this" :: rs   -> (namespace, search store !<<(namespace @++ rs))
    | _              ->
        if namespace = [] then ([], search store !<<reference)
        else
            let value = search store (namespace @<< reference) in
            match value with
            | Undefined -> resolve store !--namespace reference
            | _         -> (namespace, value)
;;

(** Add a pair identifier-value into a store if the identifier is
    exist, then the old-value will be replaced note that in the
    replacement, the position of the pair within the store must be
    maintained. *)
let rec put store identifier value =
    match store with
    | []              -> (identifier, value) :: []
    | (id, v) :: tail ->
        if id = identifier then
            match v, value with (* merge semantics?? *)
            | Store destination, Store source ->
                (identifier, Store (copy destination source [])) :: tail
            | _,_ -> (identifier, value) :: tail
        else
            (id, v) :: put tail identifier value

(** Add a pair reference-value into a store if the identifier is
    exist, then the old-value will be replaced note that in the
    replacement, the position of the pair within the store must be
    maintained. *)
and bind store reference value =
    match reference with
    | []       -> error 506 "invalid reference"
    | id :: rs ->
        if rs = [] then put store id value
        else
            match store with
            | []                -> error 507 "invalid reference"
            | (ids, vs) :: tail ->
                if ids = id then
                    match vs with
                    | Store child -> (id, Store (bind child rs value)) :: tail
                    | _           -> error 508 "invalid reference"
                else
                    (ids,vs) :: bind tail reference value

(** Copy the content of a store to a particular location (referred
    by given reference) within given a store. *)
and copy store source dest =
    match source with
    | []              -> store
    | (id, v) :: tail -> copy (bind store (dest @+. id) v) tail dest
;;

(** Similar with 'copy' by the location of the source store is
    referred by a reference. *)
let rec inherit_proto store namespace prototypeReference destReference =
    match resolve store namespace prototypeReference with
    | _, Val (Store prototype) -> copy store prototype destReference
    | _, Val (Link link) ->
        (
            match resolve_link store namespace destReference (Link link) with
            | _, Val (Store prototype) -> copy store prototype destReference
            | _, _             -> error 509 ""
        )
    | _, _ -> error 510 ""

(** Resolve a link-reference. This will detect any cyclic. *)
and get_link store namespace reference link accumulator =
    let (ns, ref) =
        match link with
        | "root" :: rs   -> ([], rs)
        | "parent" :: rs ->
            if namespace = [] then
                error 515 ("invalid link-reference " ^ !^link)
            else
                ([], !--namespace @++ rs)
        | "this" :: rs -> ([], namespace @++ rs)
        | _ -> (namespace, link)
    in
    if SetRef.exists (fun r -> r = ref) accumulator then
        error 503 ("cyclic link-reference " ^ !^ref)
    else
        let (nsp, value) = resolve store ns ref in
        let r = nsp @++ ref in
        match value with
        | Val Link lr | Val Basic Reference lr ->
            get_link store !--r reference lr (SetRef.add r accumulator)
        | _ ->
            if r @<= reference then error 504 ("Implicit cyclic link-reference " ^ !^r)
            else (r, value)

(** Resolve a link-reference *)
and resolve_link store namespace reference value =
    match value with
    | Link link -> get_link store namespace reference link SetRef.empty
    | _         -> error 505 "invalid link reference"
;;

(** Set of functions *)
module SetFunc = Set.Make
(
    struct
        type t = (store -> reference -> value)
        let compare = Pervasives.compare
    end
)

(** Evaluate a function, and then return the evaluation result. *)
let rec eval_function ?visited:(bucket=SetFunc.empty) store baseReference func =
    if SetFunc.exists (fun f -> f = func) bucket then
        error 525 "Cyclic function detected."
    else
        match func store baseReference with
        | Lazy f -> eval_function ~visited:(SetFunc.add func bucket) store baseReference f
        | value -> value
;;

(** Find and replace all lazy values (link-reference, reference of basic
    values, and functions) by the result of the lazy value evaluation. *)
let rec eval_lazy store namespace identifier value namespace1 =
    let rp = namespace @+. identifier in
    let rec replace_link link =
        match resolve_link store namespace1 rp (Link link) with
        | _, Undefined -> error 511 (!^link ^ " is not found.")
        | nsp, Val Store ssp -> accept (bind store rp (Store ssp)) rp ssp nsp
        | _, Val Lazy func -> replace_func func
        | _, Val vp -> bind store rp vp
    and replace_ref ref =
        match resolve_link store namespace1 rp (Link ref) with
        | _, Val Basic vp -> bind store rp (Basic vp)
        | _, Val Lazy func -> replace_func func
        | _ -> store
    and replace_func func =
        let vp = eval_function store namespace1 func in
        let sp = bind store rp vp in
        eval_lazy sp namespace identifier vp namespace1
    in
    match value with
    | Link link -> replace_link link
    | Basic Reference ref -> replace_ref ref
    | Lazy func -> replace_func func
    | Store vs -> accept store rp vs rp
    | _ -> store

(** Visit every element of a store to replace all lazy values. *)
and accept store baseReference store1 baseReference1 =
    match store1 with
    | []      -> store
    | (id, v) :: sp ->
        let sq = eval_lazy store baseReference id v baseReference1 in
        accept sq baseReference sp baseReference1
;;

(** Return the first variable that has value equal to 'value'. *)
let rec find_value ns store value =
    match store with
    | [] -> []
    | (id, v) :: _ when v = value -> ns @+. id
    | (id, Store s) :: ss ->
        (
            match find_value (ns @+. id) s value with
            | [] -> find_value ns ss value
            | r -> r
        )
    | _ :: ss -> find_value ns ss value
;;

(* TODO: update semantics algebra in the documentation *)
(** Return a string of given basic value. *)
let rec string_of_basic_value bv = match bv with
    | Boolean b -> if b then "true" else "false"
    | Int i     -> string_of_int i
    | Float f   -> string_of_float f
    | String s  -> s
    | Null      -> "null"
    | Vector v  ->
        (
            let buf = Buffer.create 15 in
            let rec iter v = match v with
                | [] -> ()
                | head :: [] -> Buffer.add_string buf (string_of_basic_value head)
                | head :: tail -> Buffer.add_string buf (string_of_basic_value head); iter tail
            in
            Buffer.add_char buf '[';
            iter v;
            Buffer.add_char buf ']';
            Buffer.contents buf
        )
    | Reference r     -> (!^) r
;;

(* TODO: update semantics algebra in the documentation *)
(** A binary operator that adds two operands. The result will be:
    - add Int Int -> Int
    - add Int Float -> Float
    - add Float Int -> Float
    - add Float Float -> Float
    - add String basic -> String
    - add basic String -> String *)
let rec add ?store:(s=[]) ?namespace:(ns=[]) left right =
    match left, right with
    | Reference r1, v2 ->
        (
            match resolve ~follow:true s ns r1 with
            | (_, Val Basic v1) -> add ~store:s ~namespace:ns v1 v2
            | (_, Undefined)    -> error 520 "Left reference of '+' is not exist."
            | _                 -> error 521 "Left operand of '+' is not a basic value."
        )
    | v1, Reference r2 ->
        (
            match resolve ~follow:true s ns r2 with
            | (_, Val Basic v2) -> add ~store:s ~namespace:ns v1 v2
            | (_, Undefined)    -> error 522 "Right reference of '+' is not exist."
            | _                 -> error 523 "Right operand of '+' is not a basic value."
        )
    | Int i, Float f
    | Float f, Int i       -> Float ((float_of_int i) +. f)
    | Float f1, Float f2   -> Float (f1 +. f2)
    | Int i1, Int i2       -> Int (i1 + i2)
    | String s1, String s2 -> String (s1 ^ s2)
    | String s, v          -> String (s ^ (string_of_basic_value v))
    | v, String s          -> String ((string_of_basic_value v) ^ s)
    | v1, v2 -> error 524 ("Invalid operands '+': " ^ (string_of_basic_value v1) ^
                           ", " ^ (string_of_basic_value v2))
;;


(*******************************************************************
 * Flat-Store domain and its semantics algebras
 *******************************************************************)

(** Flat-store domain *)
type flatstore = value MapRef.t

(** A value that represents a static-object. *)
let static_object = Store [] ;;

(** Convert a store into a flat-store. *)
let normalise store =
    let rec visit store baseReference flatstore =
        match store with
        | [] -> flatstore
        | (id, v) :: tail ->
            let r = baseReference @+. id in
            let fss =
                match v with
                | Store child ->
                    visit child r (MapRef.add r static_object flatstore)
                | Action (_, ps, c, pre, post) ->
                    MapRef.add r (Action (r, ps, c, pre, post)) flatstore
                | _ -> 
                    MapRef.add r v flatstore
            in
            visit tail baseReference fss
    in
    visit store [] MapRef.empty
;;


(*******************************************************************
 * set of values
 *******************************************************************)

(** Set of values *)
module SetValue = Set.Make
(
    struct
        type t = value
        let compare = Pervasives.compare
    end
)


(*******************************************************************
 * Action parameter domain.
 *******************************************************************)

(** The type of the grounded-action's parameters. *)
type ground_parameters = basic MapStr.t

(** Substitute each left-hand side reference with a reference as
    specified in the parameters table *)
let substitute_parameter_of_reference reference groundParameters =
    match reference with
    | id :: tail ->
        if MapStr.mem id groundParameters then
            match MapStr.find id groundParameters with
            | Reference r1 -> !<<(r1 @++ tail)
            | _      -> error 513 ("cannot replace left-hand side " ^
                "reference with a non-reference value")
                        (* cannot replace left-hand side reference with
                           a non-reference value *)
        else reference
    | _ -> reference

(** Substitute each right-hand side reference of basic value
    with a value as specified in the parameters table. *)
let substitute_parameter_of_basic_value basicValue groundParameters =
    match basicValue with
    | Reference (id :: tail) ->
        if MapStr.mem id groundParameters then
            match MapStr.find id groundParameters with
            | Reference r1            -> Reference !<<(r1 @++ tail)
            | v1 when tail = [] -> v1
            | _                 -> error 514 ""
        else basicValue
    | _ -> basicValue


(*******************************************************************
 * Utility functions
 *******************************************************************)

(** Remove actions and global constraints from store. *)
let to_state store =
    let rec accept store store1 = match store with
        | [] -> store1
        | (_, Action _) :: s
        | (_, Global _) :: s -> accept s store1
        | (id, Store sp) :: s -> accept s ((id, Store (accept sp [])) :: store1)
        | (id, v) :: s -> accept s ((id, v) :: store1)
    in
    accept store []
;;
