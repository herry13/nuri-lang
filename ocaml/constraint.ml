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


let rec apply store constraint_ =
  let eval_ref reference = match resolve_follow reference [] [] store with
    | _, value -> value
  in
  let eval_val = function
    | Reference ref ->
      begin match eval_ref ref with
      | Undefined -> Basic (Reference ref)
      | Val value -> value
      end
    | value -> Basic value
  in
  let compare_values comparator (value1 : value) (value2 : basic) =
    begin match value1, eval_val value2 with
    | Basic Int i, ((Basic Float _) as f) ->
      comparator (Basic (Float (float_of_int i))) f

    | ((Basic Float _) as f), Basic Int i ->
      comparator f (Basic (Float (float_of_int i)))

    | v1, v2 ->
      comparator v1 v2
    end
  in
  let compare comparator reference value = match eval_ref reference with
    | Undefined -> false
    | Val v     -> compare_values comparator v value
  in
  match constraint_ with
  | Equal (ref, value)        -> compare (=) ref value
  | NotEqual (ref, value)     -> compare (<>) ref value
  | Greater (ref, value)      -> compare (>) ref value
  | GreaterEqual (ref, value) -> compare (>=) ref value
  | Less (ref, value)         -> compare (<) ref value
  | LessEqual (ref, value)    -> compare (<=) ref value
  | In (ref, vec) ->
    begin match eval_ref ref with
    | Undefined -> false
    | Val value -> List.exists (compare_values (=) value) vec
    end

  | Not c1         -> not (apply store c1)
  | Imply (c1, c2) -> not (apply store c1) || (apply store c2)
  | And [] | Or [] -> true
  | And clauses    -> List.for_all (fun c -> apply store c) clauses
  | Or clauses     -> List.exists (fun c -> apply store c) clauses
  | True           -> true
  | False          -> false
;;

let rec prevail_of (reference : reference) (variables : Variable.ts) =
  match reference with
  | [] -> error 702 "Invalid nested reference."
  | _ when Variable.mem reference variables -> reference
  | _ -> prevail_of !-reference variables
;;

(**
 * This function compiles a nested reference at the left-hand side of
 * a constraint into a set of constraints, each of which has a prevail
 * reference at the right-hand side.
 *
 * mode:
 * - 1 = Equality
 * - 2 = NotEquality
 * - 3 = Membership
 * - 4 = NotMembership
 * - 5 = GreaterThan
 * - 6 = GreaterOrEqualsThan
 * - 7 = LessThan
 * - 8 = LessOrEqualsThan
 *)
let rec nested_to_prevail reference value variables typeEnvironment mode =
  let rec iter r cs =
    let prevail = prevail_of r variables in
    if prevail = r then
      begin match mode, value with
      | 1, _          -> (Equal (r, value)) :: cs
      | 2, _          -> (NotEqual (r, value)) :: cs
      | 3, Vector vec -> (In (r, vec)) :: cs
      | 4, Vector vec -> (Not (In (r, vec))) :: cs
      | 5, _          -> (Greater (r, value)) :: cs
      | 6, _          -> (GreaterEqual (r, value)) :: cs
      | 7, _          -> (Less (r, value)) :: cs
      | 8, _          -> (LessEqual (r, value)) :: cs
      | _             -> error 703 "invalid mode"
      end
    else
      begin
        let cs1 = (NotEqual (prevail, Null)) :: cs in
        let rs1 = r @- prevail in
        let foreach_value accumulator value = match value with
          | Basic ((Reference ref) as v) ->
            begin
              let premise = Equal (prevail, v) in
              let conclusion = And (iter (ref @+ rs1) []) in
              (Imply (premise, conclusion)) :: accumulator
            end
          | Basic Null -> accumulator
          | _ -> error 704 "Invalid nested reference."
        in
        let prevailValues = Variable.values_of prevail variables in
        Array.fold_left foreach_value cs1 prevailValues
      end
  in
  And (iter reference [])

(**
 * simplify a conjunction formula
 * - remove duplications
 * - determine whether the formula is always false
 *)
and simplify_conjunction conjunction =
  let test c1 c2 = match c1, c2 with
    | Equal (r1, v1), NotEqual (r2, v2)
    | NotEqual (r1, v1), Equal (r2, v2) -> (r1 = r2) && (v1 = v2)
    | Equal (r1, v1), Equal (r2, v2) -> (r1 = r2) && (not (v1 = v2))
    | _ -> false
  in
  let rec iter clauses acc =
    match clauses with
    | []         -> And acc
    | c1 :: tail ->
      begin
        if List.exists (test c1) tail then False
        else if List.mem c1 tail then iter tail acc
        else iter tail (c1 :: acc)
      end
  in
  match conjunction with
  | And clauses -> iter clauses []
  | _           -> error 705 "the constraint is not a conjunction"

(**
 * simplify a disjunction formula
 * - remove duplications
 *)
and simplify_disjunction disjunction =
  let rec iter clauses acc =
    match clauses with
    | []             -> Or acc
    | clause :: tail -> if List.mem clause tail then iter tail acc
                        else iter tail (clause :: acc)
  in
  match disjunction with
  | Or clauses -> iter clauses []
  | _          -> error 706 "the constraint is not a disjunction"

and cross_product callback list1 list2 =
  List.fold_left (fun accumulator1 element1 ->
    List.fold_left (fun accumulator2 element2 ->
      (callback element1 element2) :: accumulator2
    ) accumulator1 list2
  ) [] list1

(**
 * cross product of disjunction clauses of a conjunction formula
 * @param ands conjunction clauses of the formula
 * @param ors  disjunction clauses of the formula
 *)
and cross_product_ands_ors andClauses orClauses =
  let cross_ors =
    cross_product (fun clause1 clause2 -> And [clause1; clause2])
  in
  let foreach_or_clause accumulator clause = match clause with
    | And css -> (simplify_conjunction (And (List.append css andClauses))) ::
                 accumulator
    | _ -> error 707 "Clause is not a conjunction."
  in
  let rec iter clauses =
    match clauses with
    | [] -> []
    | (Or cs) :: [] -> List.fold_left foreach_or_clause [] cs
    | (Or cs1) :: (Or cs2) :: tail -> iter ((Or (cross_ors cs1 cs2)) :: tail)
    | _ -> error 708 "Invalid constraint."
  in
  dnf_of (Or (iter orClauses))

(** convert a constraint formula to a DNF formula **)
and dnf_of constraint_ variables typeEnvironment =
  match constraint_ with
  | True -> True
  | False -> False
  | Equal (r, v)    -> dnf_of_equal r v variables typeEnvironment
  | NotEqual (r, v) -> dnf_of_not_equal r v variables typeEnvironment
  | Not c1          -> dnf_of_negation c1 variables typeEnvironment
  | Imply (c1, c2)  -> dnf_of_implication c1 c2 variables typeEnvironment
  | In (r, vec)     -> dnf_of_membership r vec variables typeEnvironment
  | And cs          -> dnf_of_conjunction cs variables typeEnvironment
  | Or cs           -> dnf_of_disjunction cs variables typeEnvironment
  | Greater (r, v)  ->
    dnf_of_numeric r v variables typeEnvironment (fun x1 x2 -> x1 > x2)

  | GreaterEqual (r, v) ->
    dnf_of_numeric r v variables typeEnvironment (fun x1 x2 -> x1 >= x2)

  | Less (r, v) ->
    dnf_of_numeric r v variables typeEnvironment (fun x1 x2 -> x1 < x2)

  | LessEqual (r, v) ->
    dnf_of_numeric r v variables typeEnvironment (fun x1 x2 -> x1 <= x2)

and dnf_of_numeric r v variables typeEnvironment comparator =
  let compare_with x accumulator value = match value with
    | Basic ((Int i) as v) when comparator (float_of_int i) x ->
      v :: accumulator

    | Basic ((Float f) as v) when comparator f x -> v :: accumulator
    | Basic (Int _) | Basic (Float _) -> accumulator
    | _ -> error 710 "Not an integer or float."
  in
  let convert x =
    let values = Array.fold_left (compare_with x)
                                 []
                                 (Variable.values_of r variables)
    in
(*
    let values =
      Array.fold_left (
        fun acc v1 -> (
          match v1 with
          | Basic (Int i) ->
            if comparator (float_of_int i) x then (Int i) :: acc
            else acc
          | Basic (Float f) ->
            if comparator f x then (Float f) :: acc
            else acc
          | _ ->
            error 710 "not an integer of float"
        )
      ) [] (Variable.values_of r variables)
    in
*)
    if values = [] then False
    else dnf_of (In (r, values)) variables typeEnvironment
  in
  if Variable.mem r variables then
    begin match v with
    | Int i   -> convert (float_of_int i)
    | Float f -> convert f
    | Reference r2  ->
      begin
      let conjunction r1 v1 r2 v2 = And [Equal (r1, v1); Equal (r2, v2)] in
      if Variable.mem r2 variables then
        let leftValues = Variable.values_of r variables in
        let rightValues = Variable.values_of r2 variables in
        let clauses =
          Array.fold_left (fun acc1 leftValue ->
            Array.fold_left (fun acc2 rightValue ->
              match leftValue, rightValue with
              | Basic (Int v1), Basic (Int v2) when
                comparator (float_of_int v1) (float_of_int v2) ->
                  (conjunction r (Int v1) r2 (Int v2)) :: acc2
              | Basic (Int v1), Basic (Float v2) when
                comparator (float_of_int v1) v2 ->
                  (conjunction r (Int v1) r2 (Float v2)) :: acc2
              | Basic (Float v1), Basic (Float v2) when
                comparator v1 v2 ->
                  (conjunction r (Float v1) r2 (Float v2)) ::
                    acc2
              | Basic (Float v1), Basic (Int v2) when
                comparator v1 (float_of_int v2) ->
                  (conjunction r (Float v1) r2 (Int v2)) ::
                    acc2
              | _ -> acc2
            ) acc1 rightValues
          ) [] leftValues
        in
        Or clauses
      else
        error 712 "right-hand side is a nested reference"
        (* right-hand is a nested reference *)
      end
    | _ -> error 713 ""
    end

  else
    dnf_of (nested_to_prevail r v variables typeEnvironment 5)
           variables
           typeEnvironment

(** convert equality to DNF, and convert a left-nested reference to prevail
    ones **)
and dnf_of_equal reference value variables typeEnvironment =
  if Variable.mem reference variables then
    Equal (reference, value)
  else
    dnf_of (nested_to_prevail reference value variables typeEnvironment 1)
           variables
           typeEnvironment

(** convert inequality to DNF, and convert a left-nested reference to
    prevail ones **)
and dnf_of_not_equal reference value variables typeEnvironment =
  if Variable.mem reference variables then
    begin
      let values = Variable.values_of reference variables in
      let filter accumulator v = match v with
        | Basic bv when bv = value -> accumulator
        | Basic bv -> bv :: accumulator
        | _ -> error 714 "Right-hand side of '=' is not a basic value."
      in
      match Array.fold_left filter [] values with
      | [] -> False
      | vector -> dnf_of (In (reference, vector)) variables typeEnvironment
    end
  else
    begin
      let c = nested_to_prevail reference value variables typeEnvironment 2 in
      dnf_of c variables typeEnvironment
    end

(** convert negation to DNF **)
and dnf_of_negation constraint_ variables typeEnvironment =
  match constraint_ with
  | True -> False
  | False -> True
  | Equal (r, v) -> dnf_of (NotEqual (r, v)) variables typeEnvironment
  | NotEqual (r, v) -> dnf_of (Equal (r, v)) variables typeEnvironment
  | Greater (r, v) -> dnf_of (LessEqual (r, v)) variables typeEnvironment
  | GreaterEqual (r, v) -> dnf_of (Less (r, v)) variables typeEnvironment
  | Less (r, v) -> dnf_of (GreaterEqual (r, v)) variables typeEnvironment
  | LessEqual (r, v) -> dnf_of (Greater (r, v)) variables typeEnvironment
  | Not c1 -> dnf_of c1 variables typeEnvironment
  | Imply (premise, conclusion) ->
    begin
      (* -(p -> q) = p ^ -q *)
      dnf_of (And [premise; (Not conclusion)]) variables typeEnvironment
    end

  | And cs ->
    begin
      (* De Morgan's laws *)
      let css = List.fold_left (fun acc c -> (Not c) :: acc) [] cs in
      dnf_of (Or css) variables typeEnvironment
    end

  | Or cs  ->
    begin
      (* De Morgan's laws *)
      let css = List.fold_left (fun acc c -> (Not c) :: acc) [] cs in
      dnf_of (And css) variables typeEnvironment
    end

  | In (r, vector) ->
    begin
      if Variable.mem r variables then
        begin
          let values = Variable.values_of r variables in
          let foreach_value accumulator value = match value with
            | Basic v when not (List.mem v vector) -> accumulator
            | Basic v -> (Equal (r, v)) :: accumulator
            | _ -> error 715 "Invalid in constraint."
          in
          match Array.fold_left foreach_value [] values with
          | [] -> False
          | clauses -> dnf_of (Or clauses) variables typeEnvironment
        end
      else
        begin
          let c = nested_to_prevail r
                                    (Vector vector)
                                    variables
                                    typeEnvironment
                                    4
          in
          dnf_of c variables typeEnvironment
        end
    end

(** convert implication to DNF **)
and dnf_of_implication premise conclusion =
  dnf_of (Or [(Not premise); conclusion])

(** convert membership constraint to DNF **)
and dnf_of_membership reference vector variables typeEnvironment =
  if Variable.mem reference variables then
    begin
      let values = Variable.values_of reference variables in
      let foreach_value accumulator value = match value with
        | Basic v when not (List.mem v vector) -> accumulator
        | Basic v -> (Equal (reference, v)) :: accumulator
        | _ -> error 716 "Invalid membership constraint."
      in
      match Array.fold_left foreach_value [] values with
      | []      -> False
      | clauses -> dnf_of (Or clauses) variables typeEnvironment
    end
  else
    begin
      let c = nested_to_prevail reference
                                (Vector vector)
                                variables
                                typeEnvironment
                                3
      in
      dnf_of c variables typeEnvironment
    end

(** convert conjunction to DNF, performs cross-products when it has
    disjunction clause *)
and dnf_of_conjunction clauses variables typeEnvironment =
  let rec fold clauses ands ors = match clauses with
    | [] -> (false, ands, ors)
    | head :: tail ->
      begin match dnf_of head variables typeEnvironment with
      | And cs       -> fold tail (List.append cs ands) ors
      | False        -> (true, ands, ors)
      | True         -> fold tail ands ors
      | (Or cs) as c -> fold tail ands (c :: ors)
      | c            -> fold tail (c :: ands) ors
      end
  in
  match clauses with
  | [] -> True
  | _  ->
    begin match fold clauses [] [] with
    | true, _, _ -> False
    | _, [], []  -> True
    | _, head :: [], [] -> head
    | _, [], head :: [] -> head
    | _, ands, []  -> simplify_conjunction (And ands)
    | _, ands, ors ->
      (cross_product_ands_ors ands ors) variables typeEnvironment
    end

(** convert disjunction to DNF *)
and dnf_of_disjunction clauses variables typeEnvironment =
  let rec fold accumulator clauses = match clauses with
    | []           -> (false, accumulator)
    | head :: tail ->
      begin match dnf_of head variables typeEnvironment with
      | Or cs -> fold (List.append cs accumulator) tail
      | False -> fold accumulator tail
      | True  -> (true, accumulator)
      | c     -> fold (c :: accumulator) tail
      end
  in
  match clauses with
  | [] -> True
  | _  ->
    begin match fold [] clauses with
    | true, _         -> True
    | _, clause :: [] -> clause
    | _, cs           -> simplify_disjunction (Or cs)
    end
;;

(** Substitute each parameter with a value as specified in the map of
    parameters *)
let rec substitute_free_variables_of constraint_ parameters =
  let sub_ref ref = substitute_parameter_of_reference ref parameters in
  let sub_val value = substitute_parameter_of_basic_value value parameters in
  match constraint_ with
  | Equal (r, v) -> Equal (sub_ref r, sub_val v)
  | NotEqual (r, v) -> NotEqual (sub_ref r, sub_val v)
  | Greater (r, v) -> Greater (sub_ref r, sub_val v)
  | GreaterEqual (r, v) -> GreaterEqual (sub_ref r, sub_val v)
  | Less (r, v) -> Less (sub_ref r, sub_val v)
  | LessEqual (r, v) -> LessEqual (sub_ref r, sub_val v)
  | Not c -> Not (substitute_free_variables_of c parameters)
  | Imply (c1, c2) -> Imply (
      substitute_free_variables_of c1 parameters,
      substitute_free_variables_of c2 parameters
    )
  | And cs -> And (List.fold_left (fun css c ->
        (substitute_free_variables_of c parameters) :: css
      ) [] cs
    )
  | Or cs -> Or (List.fold_left (fun css c ->
        (substitute_free_variables_of c parameters) :: css
      ) [] cs
    )
  | In (r, vec) -> In (sub_ref r, vec)
    (* TODO: every element of the vector should be evaluated as well! *)
  | True -> True
  | False -> False
;;


(************************************************************************
 * Functions for global constraints.
 ************************************************************************)

type data = {
  complex   : constraint_ list;
  simple    : constraint_ list;
  variables : Variable.ts
}

(** compile simple membership of global constraints *)
let compile_membership isNegation reference vector data =
  let prevail = prevail_of reference data.variables in
  if prevail = reference then
    {
      complex   = data.complex;
      simple    = data.simple;
      variables =
      if isNegation then
        Variable.remove_values_from reference vector
                      data.variables
      else
        Variable.intersection_with_values reference vector
                        data.variables
    }
  else
    let variables1 = Variable.remove_value_from prevail (Basic Null)
        data.variables
    in
    let rs = reference @- prevail in
    let accumulator = {
        complex   = data.complex;
        simple    = data.simple;
        variables = variables1
      }
    in
    let values = Variable.values_of prevail variables1 in
    Array.fold_left (fun acc v ->
      match v with
      | Basic (Reference r) ->
        let r1 = r @+ rs in
        let c =
          if isNegation then
            Imply (Equal (prevail, Reference r), Not (In (r1, vector)))
          else
            Imply (Equal (prevail, Reference r), In (r1, vector))
        in
          {
            complex   = c :: acc.complex;
            simple    = acc.simple;
            variables = acc.variables
          }
      | _ -> error 719 "not a basic value"
    ) accumulator values
;;

(** compile simple equality of global constraints *)
let compile_equality isNegation reference value data =
  let prevail = prevail_of reference data.variables in
  if prevail = reference then
    {
      complex   = data.complex;
      simple    = data.simple;
      variables =
        if isNegation then
          Variable.remove_value_from reference (Basic value)
                                     data.variables
        else
          Variable.intersection_with_value reference (Basic value)
                                           data.variables
    }
  else 
    let variables1 = Variable.remove_value_from prevail (Basic Null)
        data.variables
    in
    let rs = reference @- prevail in
    let accumulator = {
        complex   = data.complex;
        simple    = data.simple;
        variables = variables1
      }
    in
    let values = Variable.values_of prevail variables1 in
    Array.fold_left (fun acc v ->
      match v with
      | Basic (Reference r) ->
        let r1 = r @+ rs in
        let constraint_ =
          if isNegation then
            Imply (Equal (prevail, Reference r),
            NotEqual (r1, value))
          else
            Imply (Equal (prevail, Reference r),
            Equal (r1, value))
        in
          if not isNegation && (Variable.mem r1 acc.variables)
            then {
              complex = acc.complex;
              simple = constraint_ :: acc.simple;
              variables = acc.variables
            }
          else
            {
              complex = constraint_ :: acc.complex;
              simple = acc.simple;
              variables = acc.variables
            }
      | _ -> error 721 "invalid constraint"
    ) accumulator values
;;

(** compile simple equality and membership of global constraints *)
let compile_simple_constraints globalConstraints variables =
  let accumulator =
    {
      complex   = [];
      simple    = [];
      variables = variables
    }
  in
  let compile clauses =
    let data =
      List.fold_left (fun acc clause ->
        match clause with
        | Equal (r, v)         -> compile_equality false r v acc
        | Not (Equal (r, v))   -> compile_equality true r v acc
        | NotEqual (r, v)         -> compile_equality true r v acc
        | Not (NotEqual (r, v))   -> compile_equality false r v acc
        | In (r, vec)       -> compile_membership false r vec acc
        | Not (In (r, vec)) -> compile_membership true r vec acc
        | Imply (Equal (_, _), Equal (_, _)) -> {
            complex   = acc.complex;
            simple    = clause :: acc.simple;
            variables = acc.variables
          }
        | Imply (Equal (r, v), And clauses) -> (
          let eq = Equal (r, v) in
            List.fold_left (fun (acc: data) (c: constraint_) ->
              match c with
              | Equal (_, _) ->
                {
                  complex   = acc.complex;
                  simple    = (Imply (eq, clause)) ::
                                      acc.simple;
                  variables = acc.variables
                }
              | _ ->
                {
                  complex   = (Imply (eq, clause)) ::
                              acc.complex;
                  simple    = acc.simple;
                  variables = acc.variables
                }
            ) acc clauses
          )
        | _ ->
          {
            complex   = clause :: acc.complex;
            simple    = acc.simple;
            variables = acc.variables
          }
      ) accumulator clauses
    in
    (And data.complex, data.simple, data.variables)
  in
  match globalConstraints with
  | And clauses -> compile clauses
  | _  -> (globalConstraints, [], variables)
;;

(** This function performs:
    1. find the global constraints in given flat-store
    2. modify variables' domain based on 'membership' constraints
    3. separate simple implication clauses from the global constraints formula
       with others
    4. convert the other clauses (complex formula) into a DNF formula 
*)
let global_of typeEnvironment flatStore variables =
  if MapRef.mem reference_of_global flatStore then
    begin match MapRef.find reference_of_global flatStore with
    | Global globalConstraints ->
      begin
        let (globalConstraints1, globalImplications, variables1) =
          compile_simple_constraints globalConstraints variables
        in
        let dnfGlobalConstraints1 = dnf_of globalConstraints1
                                           variables
                                           typeEnvironment
        in
        (dnfGlobalConstraints1, globalImplications, variables1)
      end

    | _ -> error 722 "not a global constraint"
    end
  else
    (True, [], variables)
;;
