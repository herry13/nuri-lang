(** Module Domain contains the definition of the semantics' domains
    with their semantics algebras (functions to manipulate the domains).

    Module dependencies:
    - Common

    @author Herry (herry13\@gmail.com)
    @since 2014
*)

open Common

(*******************************************************************
 * semantics domain
 *******************************************************************)

(** Basic Value domain *)
type basic = Boolean   of bool       (** Boolean domain *)
           | Int       of int        (** Integer domain *)
           | Float     of float      (** Float domain *)
           | String    of string     (** String domain *)
           | Null                    (** Null domain *)
           | Vector    of vector
           | Reference of reference
           | Symbol    of string

(** Vector domain *)
and vector = basic list

(** Value domain *)
and value = Basic  of basic
          | Store  of store
          | Global of constraint_
          | Link   of reference
          | Action of action
          | TBD
          | Unknown
          | Lazy of func

(** Lifted-value domain *)
and value_lifted = Val of value
                 | Undefined

(** Cell domain *)
and cell = ident * value

(** Store domain *)
and store = cell list

(** Reference domain *)
and reference = ident list

(** Lifted-reference domain *)
and reference_ = Valid of reference
               | Invalid

(** Identifier domain *)
and ident = string

(** Function domain : receive a store and a namespace, return a value *)
and func = store -> reference -> value

(** Constraint domain *)
and constraint_ = Equal        of reference * basic
                | NotEqual     of reference * basic
				| Greater      of reference * basic
				| GreaterEqual of reference * basic
				| Less         of reference * basic
				| LessEqual    of reference * basic
                | Not          of constraint_
                | Imply        of constraint_ * constraint_
                | And          of constraint_ list
                | Or           of constraint_ list
                | In           of reference * vector
                | True
                | False

(** Action domain *)
and action         = reference * parameter_type list * cost * constraint_ *
                     effect list
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
    raise (Error (code, "[err" ^ (string_of_int code) ^ "] " ^ message))
;;

type namespaceReference = { namespace : reference ; ref : reference } ;;


(*******************************************************************
 * semantics algebras for identifier and reference domains
 *******************************************************************)

(** Return a prefix of a reference. *)
let (!^) r = String.concat "." r ;;

(** Concat the second reference to the last position of the first
    reference.
*)
let rec (!-) = function
  | []           -> []
  | head :: []   -> []
  | head :: tail -> head :: (!- tail)
;;

(** Concat the second reference to the last position of the first
    reference. *)
let (@+) = List.append ;;

(** Add an identifier to the last position of the reference. *)
let (@+.) reference identifier = reference @+ [identifier] ;;

(** Remove a common (of both references) prefix from the first reference. *)
let rec (@-) reference1 reference2 = match reference1, reference2 with
  | [], _                                     -> []
  | _, []                                     -> reference1
  | id1 :: tail1, id2 :: tail2 when id1 = id2 -> tail1 @- tail2
  | _                                         -> reference1
;;

(** 'true' if the second reference is equal or the prefix of the
    first one, otherwise 'false'.
*)
let (@<=) reference1 reference2 = (reference1 @- reference2) = [] ;;

(** 'true' if the second reference is not equal and the prefix of
    the first one, otherwise 'false'.
*)
let (@<) reference1 reference2 =
  (reference1 @<= reference2) && not (reference1 = reference2)
;;

(** Given a namespace (first), remove keyword 'root', 'parent',
    and 'this' from a reference (second). Note that any keyword
    should not be exist in the namespace.
*)
let rec (@<<) namespace reference = match namespace, reference with
  | _, []                -> Valid namespace
  | _, "this" :: tail    -> namespace @<< tail
  | _, "root" :: tail    -> [] @<< tail
  | [], "parent" :: tail -> Invalid
  | _, "parent" :: tail  -> !-namespace @<< tail
  | _, id :: tail        -> (namespace @+. id) @<< tail
;;

(** Similar with '@<<' but the namespace is root. *)
let (!<<) reference = [] @<< reference ;;


(*******************************************************************
 * semantics algebras for store domain
 *******************************************************************)

(** Find a reference in a store and then return its value. *)
let rec find reference store =
  match store, reference with
  | _                , []                     -> Val (Store store)
  | []               , _                      -> Undefined
  | (ids, _) :: tail , id :: _ when id <> ids -> find reference tail
  | (_, value) :: _  , _ :: []                -> Val value
  | (_, Store s) :: _, _ :: rs                -> find rs s
  | _                                         -> Undefined
;;

(****************************************************************************
(* TODO: add this into the document of the formal semantics *)
(** Similar with 'find', but if the value is another reference
    (nested reference) then it will follow it.

    !!! Note that when using this function, any cyclical reference should
    not be exist in the store (this can be guaranteed by the type-system).
    If there is a cyclical, then this function will never end. !!!
*)
let find_follow store reference : value_lifted =
    let rec search s r = match s, r with
        | _, []                                    -> Val (Store s)
        | [], _                                    -> Undefined
        | (ids, _) :: tail, id :: _ when id <> ids -> search tail r
        | (ids, vs) :: _, id :: []                 -> Val vs
        | (ids, Store child) :: _, id :: rs        -> search child rs
        | (ids, Basic Reference rp) :: _, id :: rs -> search store (rp @+ rs)
        | _                                        -> Undefined
    in
    search store reference
;;

(* TODO: update documentation of the formal semantics *)
(** Resolve a reference in a store within given namespace, and then
    return its value set '~follow:true' (default 'false') if you
    want to use 'find_follow' instead of 'find'.

    !!! Note that this function is using 'find_follow' to resolve nested
    references. See the description of 'find_follow' to guarantee the
    resolution process will terminate. !!!
*)
let rec resolve ?follow:(ff=false) store namespace reference =
    let finder = if ff then find_follow else find in
    match namespace, namespace @<< reference with
    | [], Invalid -> ([], Undefined)
    | _, Invalid  -> resolve ~follow:ff store !-namespace reference
    | [], Valid r -> ([], finder store r)
    | _, Valid r  ->
        match finder store r with
        | Undefined -> resolve ~follow:ff store !-namespace reference
        | value     -> (namespace, value)
;;
****************************************************************************)

let rec resolve reference namespace store =
  match namespace, namespace @<< reference with
  | [], Invalid   -> ([], Undefined)
  | _, Invalid    -> resolve reference !-namespace store
  | [], Valid ref -> ([], find ref store)
  | _, Valid ref  ->
    begin match find ref store with
    | Undefined -> resolve reference !-namespace store
    | value     -> (namespace, value)
    end
;;

(** Add a pair identifier-value into a store if the identifier is
    exist, then the old-value will be replaced note that in the
    replacement, the position of the pair within the store must be
    maintained. *)
let rec put store identifier value =
  match store, value with
  | ((id, _) as head) :: tail, _ when id <> identifier ->
    head :: (put tail identifier value)

  | (_, Store dest) :: tail, Store src ->
    (* this is using merge semantics *)
    (identifier, Store (copy dest src [])) :: tail

  | _ :: tail, _ -> (identifier, value) :: tail

  | [], _ -> (identifier, value) :: []

(** Add a pair reference-value into a store if the identifier is
    exist, then the old-value will be replaced note that in the
    replacement, the position of the pair within the store must be
    maintained. *)
and bind store reference value =
  match store, reference with
  | _, [] -> error 506 "Invalid reference."

  | _, id :: [] -> put store id value

  | [], _ -> error 507 "Invalid reference."

  | ((ids, _) as head) :: tail, id :: _ when ids <> id ->
    head :: (bind tail reference value)

  | (_, Store child) :: tail, id :: rs ->
    (id, Store (bind child rs value)) :: tail

  | _ -> error 508 "Invalid reference."

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
  match resolve prototypeReference namespace store with
  | _, Val (Store prototype) -> copy store prototype destReference
  | _, Val (Link link) ->
    begin match resolve_follow link destReference namespace store with
    | _, Val (Store prototype) -> copy store prototype destReference
    | _, _                     -> error 509 ""
    end
  | _, _ -> error 510 ""

and resolve_follow ?visited:(accumulator=SetRef.empty) reference
                   destReference namespace store =
  if SetRef.exists (fun r -> r = reference) accumulator then
    error 503 "Cyclic reference is detected."
  else
    match resolve reference namespace store with
    | (_, Undefined) as value -> value
    | ns, Val Link nextRef | ns, Val Basic Reference nextRef ->
      begin match ns @<< reference with
      | Invalid -> error 510 ("Invalid reference: " ^ !^(ns @+ reference))
      | Valid ref -> resolve_follow ~visited:(SetRef.add ref accumulator)
                                    nextRef destReference !-ref store
      end
    | ns, value ->
      begin match ns @<< reference with
      | Invalid -> error 511 ("Invalid reference: " ^ !^(ns @+ reference))
      | Valid ref when ref @<= destReference ->
        error 512 "Inner-cyclic reference is detected."

      | Valid ref -> (ref, value)
      end

(*
(** get_link : reference -> SetRef.t -> store -> reference -> reference ->
               value_lifted
*)
and get_link reference accumulator store namespace destReference =
    if SetRef.exists (fun r -> r = reference) accumulator then
        error 503 "Cyclical reference is detected."
    else
        match resolve reference namespace store with
        | _, Undefined -> ([], Undefined)
        | ns, Val Link nextRef | ns, Val Basic Reference nextRef ->
            begin match ns @<< reference with
            | Invalid ->
                error 510 ("Invalid reference: " ^ !^(ns @+ reference))
            | Valid ref ->
                get_link nextRef (SetRef.add ref accumulator) store !-ref
                         destReference
            end
        | ns, value ->
            begin match ns @<< reference with
            | Invalid ->
                error 511 ("Invalid reference: " ^ !^(ns @+ reference))
            | Valid ref when ref @<= destReference ->
                error 505 "Inner-cyclical reference is detected."
            | Valid ref ->
                (ref, value)
            end

and resolve_link = function
    | Link ref -> get_link ref SetRef.empty
    | _        -> error 505 "Invalid link-reference."
*)
;;

(* TODO: need to detect cyclic functions *)
(** Evaluate a function, and then return the evaluation result. *)
let rec eval_function store namespace func =
  match func store namespace with
  | Lazy f -> eval_function store namespace f
  | value  -> value
;;

(** Find and replace all lazy values (link-reference, reference of basic
    values, and functions) by the result of the lazy value evaluation.
*)
let rec eval_lazy store namespace identifier value namespace1 =
  let rp = namespace @+. identifier in
  let rec replace_link link =
    begin match resolve_follow link rp namespace1 store with
    | _, Undefined -> error 511 (!^link ^ " is not found.")
    | nsp, Val ((Store ssp) as s) -> accept (bind store rp s) rp ssp nsp
    | _, Val Lazy func -> replace_func func
    | _, Val vp -> bind store rp vp
    end
  and replace_ref ref =
    begin match resolve_follow ref rp namespace1 store with
    | _, Val ((Basic vp) as bv) -> bind store rp bv
    | _, Val Lazy func -> replace_func func
    | _ -> store
    end
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
and accept store namespace store1 namespace1 =
  match store1 with
  | []            -> store
  | (id, v) :: sp -> accept (eval_lazy store namespace id v namespace1)
                            namespace sp namespace1
;;

(** Return the first variable that has value equal to 'value'. *)
let rec find_value ns store value =
  match store with
  | [] -> []
  | (id, v) :: _ when v = value -> ns @+. id
  | (id, Store s) :: ss ->
    begin match find_value (ns @+. id) s value with
    | [] -> find_value ns ss value
    | r  -> r
    end
  | _ :: ss -> find_value ns ss value
;;

(* TODO: update semantics algebra in the documentation *)
(** Return a string of given basic value. *)
let rec string_of_vector vec =
  let buf = Buffer.create 15 in
  let rec iter vec = match vec with
    | [] -> ()
    | head :: [] -> buf << (string_of_basic_value head)
    | head :: tail -> buf << (string_of_basic_value head); iter tail
  in
  buf <. '[';
  iter vec;
  buf <. ']';
  Buffer.contents buf

and string_of_basic_value = function
  | Boolean b   -> if b then "true" else "false"
  | Int i       -> string_of_int i
  | Float f     -> string_of_float f
  | String s    -> s
  | Null        -> "null"
  | Vector vec  -> string_of_vector vec
  | Reference r -> !^r
  | Symbol sym  -> "$" ^ sym
;;

(****************************************************************
 * Expressions evaluation functions
 ****************************************************************)

(* TODO: add this function to semantics algebra *)
(** Evaluate a reference *)
let rec evalr ?follow_f:(follow=false) ?acc:(visited=SetRef.empty) s ns r =
  if SetRef.exists (fun rs -> r = rs) visited then
    error 525 ("Cyclic references are detected: " ^ !^r)
  else
    (*match resolve ~follow:true s ns r with*)
    match resolve_follow r [] ns s with
    | _, Val Link rr | _, Val Basic Reference rr ->
      evalr ~follow_f:follow ~acc:(SetRef.add r visited) s ns rr

    | _, Val Lazy f ->
      if follow then evalf ~follow_r:true ~acc:visited s ns f
      else Lazy f

    | _, Val v -> v

    | _, Undefined  -> error 526 ("'" ^ !^r ^ "' is not found.")

(* TODO: add this function to semantics algebra *)
(** Evaluate a function *)
and evalf ?follow_r:(follow=false) ?acc:(visited=SetRef.empty) s ns f =
  match eval_function s ns f with
  | ((Link r) as v) | ((Basic Reference r) as v) ->
    if follow then evalr ~follow_f:true ~acc:visited s ns r
    else v

  | Lazy ff ->
    evalf ~follow_r:follow ~acc:visited s ns ff

  | v -> v
;;

(* TODO: add this function to semantics algebra *)
let rec unary ?store:(s=[]) ?namespace:(ns=[]) map = function
  | Basic Reference r -> unary ~store:s ~namespace:ns map (evalr s ns r)
  | Lazy f -> Lazy (fun ss nss ->
      unary ~store:ss ~namespace:nss map (evalf ~follow_r:true ss nss f)
    )

  | v -> map v
;;

(* TODO: add this function to semantics algebra *)
let rec binary ?store:(s=[]) ?namespace:(ns=[]) map v1 v2 = match v1, v2 with
  | Basic Reference r1, _ ->
    binary ~store:s ~namespace:ns map (evalr s ns r1) v2

  | _, Basic Reference r2 ->
    binary ~store:s ~namespace:ns map v1 (evalr s ns r2)

  | Lazy f1, _ -> Lazy (fun ss nss ->
      binary ~store:ss ~namespace:nss map (evalf ~follow_r:true ss nss f1) v2
    )

  | _, Lazy f2 -> Lazy (fun ss nss ->
      binary ~store:ss ~namespace:nss map v1 (evalf ~follow_r:true ss nss f2)
    )

  | _ -> map v1 v2
;;

(* TODO: add this function to semantics algebra *)
let logic ?operator:(op="") ?store:(s=[]) ?namespace:(ns=[]) f_logic =
  binary ~store:s ~namespace:ns (fun xx yy -> match xx, yy with
    | Basic Boolean b1, Basic Boolean b2 -> Basic (Boolean (f_logic b1 b2))
    | Basic Boolean _, _ ->
      error 528 ("Right operand of '" ^ op ^ "' is not a boolean.")

    | _, Basic Boolean _ ->
      error 529 ("Left operand of '" ^ op ^ "' is not a boolean.")

    | _, _ ->
      error 530 ("Both operands of '" ^ op ^ "' are not a boolean.")
  )
;;

(* TODO: add this function to semantics algebra *)
let equals ?store:(s=[]) ?namespace:(ns=[]) =
  binary ~store:s ~namespace:ns (fun x y -> match x, y with
    | Basic Int i, Basic Float f | Basic Float f, Basic Int i ->
      Basic (Boolean (f = (float_of_int i)))

    | _ -> Basic (Boolean (x = y))
  )
;;


(* TODO: add this function to semantics algebra *)
let not_equals ?store:(s=[]) ?namespace:(ns=[]) =
  binary ~store:s ~namespace:ns (fun x y -> match x, y with
    | Basic Int i, Basic Float f | Basic Float f, Basic Int i ->
      Basic (Boolean (f <> (float_of_int i)))

    | _ -> Basic (Boolean (x <> y))
  )
;;

(* TODO: add this function to semantics algebra *)
let math ?store:(s=[]) ?namespace:(ns=[]) f_int f_float =
  binary ~store:s ~namespace:ns (fun xx yy -> match xx, yy with
    | Basic Int x, Basic Float y -> Basic (Float (f_float (float_of_int x) y))
    | Basic Float x, Basic Int y -> Basic (Float (f_float x (float_of_int y)))
    | Basic Float x, Basic Float y -> Basic (Float (f_float x y))
    | Basic Int x, Basic Int y -> Basic (Int (f_int x y))
    | _ -> error 527 ("Left or right operand is neither an integer nor a " ^
                      "float.")
  )

(* TODO: add this function to semantics algebra *)
(** A binary operator that adds two operands. The result will be:
    - add Int Int -> Int
    - add Int Float -> Float
    - add Float Int -> Float
    - add Float Float -> Float
    - add String basic -> String
    - add basic String -> String *)
let add ?store:(s=[]) ?namespace:(ns=[]) =
  binary ~store:s ~namespace:ns (fun x y -> match x, y with
    | Basic Int i, Basic Float f | Basic Float f, Basic Int i ->
      Basic (Float ((float_of_int i) +. f))

    | Basic Float f1, Basic Float f2 -> Basic (Float (f1 +. f2))
    | Basic Int i1, Basic Int i2 -> Basic (Int (i1 + i2))
    | Basic String s1, Basic v2 ->
      Basic (String (s1 ^ (string_of_basic_value v2)))

    | Basic v1, Basic String s2 ->
      Basic (String ((string_of_basic_value v1) ^ s2))

    | _ -> error 528 "Operands of '+' are neither int, float, or string"
  )
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
  let rec visit store namespace flatstore = match store with
    | [] -> flatstore
    | (id, v) :: tail ->
      begin
        let r = namespace @+. id in
        let fss = match v with
          | Store child -> visit child r (MapRef.add r static_object flatstore)
          | Action (_, ps, c, pre, post) ->
            MapRef.add r (Action (r, ps, c, pre, post)) flatstore

          | _ -> MapRef.add r v flatstore
        in
        visit tail namespace fss
      end
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
type ground_parameters = basic MapStr.t ;;

(** Substitute each left-hand side reference with a reference as
    specified in the parameters table *)
let substitute_parameter_of_reference reference groundParameters =
  let ref = match reference with
    | id :: tail when MapStr.mem id groundParameters ->
      begin match MapStr.find id groundParameters with
      | Reference rp -> rp @+ tail
      | _ -> error 513 ("Cannot replace left-hand side reference " ^
                        "with a non-reference value.")
      end
    | _ -> reference
  in
  (* normalize the reference *)
  match !<<ref with
  | Valid r -> r
  | Invalid -> error 514 ("Invalid left-hand side reference: " ^ !^ref)
;;

(** Substitute each right-hand side reference of basic value
    with a value as specified in the parameters table. *)
let substitute_parameter_of_basic_value basic_value groundParameters =
  let substitute_reference = function
    | id :: [] when MapStr.mem id groundParameters ->
      MapStr.find id groundParameters

    | id :: tail when MapStr.mem id groundParameters ->
      begin match MapStr.find id groundParameters with
      | Reference r ->
        begin match r @<< tail with
        | Valid rp -> Reference rp
        | _ -> error 515 ("Invalid right-hand side reference: " ^ !^r)
        end
      | _ -> error 514 "Cannot append a reference to a non-reference value"
      end

    | r -> Reference r
  in
  match basic_value with
  | Reference r -> substitute_reference r
  | _           -> basic_value
;;


(*******************************************************************
 * Utility functions
 *******************************************************************)

(** Remove actions and global constraints from store. *)
let to_state store =
  let rec accept store store1 = match store with
    | [] -> store1
    | (_, Action _) :: s | (_, Global _) :: s -> accept s store1
    | (id, Store sp) :: s -> accept s ((id, Store (accept sp [])) :: store1)
    | (id, v) :: s -> accept s ((id, v) :: store1)
  in
  accept store []
;;

let reference_delim_regexp = Str.regexp "\\." ;;

let reference_of_string = Str.split reference_delim_regexp ;;

(* TODO: add this function to semantics algebra *)
(** Substitute every variable in a string-interpolation *)
let interpolate_string str store namespace =
  let length = String.length str in
  let out = Buffer.create length in
  let var = Buffer.create 20 in
  let get_reference buf =
    let reference = reference_of_string (Buffer.contents buf) in
    Buffer.clear buf;
    reference
  in
  let substitute reference =
    match evalr ~follow_f:true store namespace reference with
    | Basic v -> out << (string_of_basic_value v)
    | _       -> error 1152 (!^reference ^ " is not found, or it is " ^
                             "not a basic value.")
  in
  let rec iter index state =
    if index >= length then
      begin
        if state = 0 then Buffer.contents out
        else error 1150 ("Invalid string: " ^ str)
      end
    else
      begin match state, str.[index] with
      | 0, '$' -> iter (index + 1) 1
      | 0, c ->
        begin
          out <. c;
          iter (index + 1) 0
        end
      | 1, '{' -> iter (index + 1) 2
      | 1, c ->
        begin
          out <.| '$' <. c;
          iter (index + 1) 0
        end
      | 2, '}' ->
        begin
          substitute (get_reference var);
          iter (index + 1) 0
        end
      | 2, c ->
        begin
          var <. c;
          iter (index + 1) 2
        end
      | _ -> error 1151 ("Invalid string: " ^ str)
      end
  in
  iter 0 0
;;
