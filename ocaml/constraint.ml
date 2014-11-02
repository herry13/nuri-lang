(* Author: Herry (herry13@gmail.com) *)

open Common
open Domain

let rec apply store _constraint =
    let resolve_value v = match v with
        | Ref r -> (
                match find_follow store r with
                | Undefined -> Basic (Ref r)
                | Val v     -> v
            )
        | _ -> Basic v
    in
    let numeric_compare num1 num2 comparator = match num1, num2 with
        | Val (Basic (Int int1)), Basic Int int2 ->
            comparator (float_of_int int1) (float_of_int int2)
        | Val (Basic (Int int1)), Basic Float float2 ->
            comparator (float_of_int int1) float2
        | Val (Basic (Float float1)), Basic Int int2 ->
            comparator float1 (float_of_int int2)
        | Val (Basic (Float float1)), Basic Float float2 ->
            comparator float1 float2
        | _ ->
            false
    in
    match _constraint with
    | Eq (r, v) -> (
            let v2 = resolve_value v in
            match (find_follow store r), v2 with
            | Val Basic Int i, Basic Float f
            | Val Basic Float f, Basic Int i -> (float_of_int i) = f
            | Val v1, _ -> v1 = v2
            | _ -> false
        )
    | Ne (r, v) -> (
            let v2 = resolve_value v in
            match (find_follow store r), v2 with
            | Val Basic Int i, Basic Float f
            | Val Basic Float f, Basic Int i -> (float_of_int i) <> f
            | Val v1, _ -> v1 <> v2
            | _ -> false
        )
    | Greater (r, v) -> numeric_compare (find_follow store r) (resolve_value v) (>)
    | GreaterEqual (r, v) -> numeric_compare (find_follow store r) (resolve_value v) (>=)
    | Less (r, v) -> numeric_compare (find_follow store r) (resolve_value v) (<)
    | LessEqual (r, v) -> numeric_compare (find_follow store r) (resolve_value v) (<=)
    | In (r, vec) -> (
            match find_follow store r with
            | Val (Basic v1) ->
                List.exists (fun v ->
                    match v, v1 with
                    | Int i, Float f
                    | Float f, Int i -> (float_of_int i) = f
                    | _ -> v = v1
                ) vec
            | _ -> false
        )
    | Not c1 -> not (apply store c1)
    | Imply (c1, c2) -> not (apply store c1) || (apply store c2)
    | And clauses -> (
            let rec iter clauses =
                match clauses with
                | [] -> true
                | head :: tail ->
                    if not (apply store head) then false
                    else iter tail
            in
            iter clauses
        )
    | Or clauses -> (
            let rec iter cs =
                match cs with
                | [] -> false
                | head :: tail ->
                    if apply store head then true
                    else iter tail
            in
            if clauses = [] then true
            else iter clauses
        )
    | True -> true
    | False -> false
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
    let rec prevail_of r =
        if r = [] then error 702 "invalid nested reference"
        else if Variable.mem r variables then r
        else prevail_of (prefix r)
    in
    let rec iter r cs =
        let prevail = prevail_of r in
        if prevail = r then
            match mode, value with
            | 1, _          -> (Eq (r, value)) :: cs
            | 2, _          -> (Ne (r, value)) :: cs
            | 3, Vector vec -> (In (r, vec)) :: cs
            | 4, Vector vec -> (Not (In (r, vec))) :: cs
            | 5, _          -> (Greater (r, value)) :: cs
            | 6, _          -> (GreaterEqual (r, value)) :: cs
            | 7, _          -> (Less (r, value)) :: cs
            | 8, _          -> (LessEqual (r, value)) :: cs
            | _             -> error 703 "invalid mode"
        else
            let cs1 = (Ne (prevail, Null)) :: cs in
            let rs1 = r @-- prevail in
            Array.fold_left (
                fun acc vp ->
                    match vp with
                    | Basic (Ref rp) ->
                        let premise = Eq (prevail, Ref rp) in
                        let conclusion = And (iter (rp @++ rs1) []) in
                        (Imply (premise, conclusion)) :: acc
                    | Basic Null     -> acc
                    | _              -> error 704 ""
            ) cs1 (Variable.values_of prevail variables)
    in
    And (iter reference [])

(**
 * simplify a conjunction formula
 * - remove duplications
 * - determine whether the formula is always false
 *)
and simplify_conjunction conjunction =
    let rec iter clauses acc =
        match clauses with
        | []         -> And acc
        | c1 :: tail -> 
            if List.exists (fun c2 ->
                    (* return false if formula 'c1' negates 'c2'
                       (or vice versa), otherwise true *)
                    match c1, c2 with
                    | Eq (r1, v1), Eq (r2, v2) ->
                        if r1 = r2 then not (v1 = v2) else false
                    | Eq (r1, v1), Ne (r2, v2) ->
                        if r1 = r2 then v1 = v2 else false
                    | Ne (r1, v1), Eq (r2, v2) ->
                        if r1 = r2 then v1 = v2 else false
                    | _ -> false
                ) tail then False
            else if List.mem c1 tail then iter tail acc
            else iter tail (c1 :: acc)
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
        | [] -> Or acc
        | clause :: tail ->
            if List.mem clause tail then iter tail acc
            else iter tail (clause :: acc)
    in
    match disjunction with
    | Or clauses -> iter clauses []
    | _          -> error 706 "the constraint is not a disjunction"

(**
 * cross product of disjunction clauses of a conjunction formula
 * @param ands conjunction clauses of the formula
 * @param ors  disjunction clauses of the formula
 *)
and cross_product_of andClauses orClauses =
    let cross clauses1 clauses2 =
        List.fold_left (fun acc1 clause1 ->
            List.fold_left (fun acc2 clause2 ->
                (And [clause1; clause2]) :: acc2
            ) acc1 clauses2
        ) [] clauses1
    in
    let rec iter clauses =
        match clauses with
        | []            -> []
        | (Or cs) :: [] ->
            List.fold_left (
                fun acc c ->
                    match c with
                    | And css ->
                        (simplify_conjunction (
                            And (List.append css andClauses))
                        ) :: acc
                    | _       -> error 707 "not a conjunction"
            ) [] cs
        | (Or cs1) :: (Or cs2) :: tail -> iter ((Or (cross cs1 cs2)) :: tail)
        | _ -> error 708 "invalid constraint"
    in
    dnf_of (Or (iter orClauses))

(** convert a constraint formula to a DNF formula **)
and dnf_of _constraint variables typeEnvironment =
    match _constraint with
    | Eq (r, v)      -> dnf_of_equal r v variables typeEnvironment
    | Ne (r, v)      -> dnf_of_not_equal r v variables typeEnvironment
    | Not c1         -> dnf_of_negation c1 variables typeEnvironment
    | Imply (c1, c2) -> dnf_of_implication c1 c2 variables typeEnvironment
    | In (r, vec)    -> dnf_of_membership r vec variables typeEnvironment
    | And cs         -> dnf_of_conjunction cs variables typeEnvironment
    | Or cs          -> dnf_of_disjunction cs variables typeEnvironment
    | Greater (r, v) ->
        dnf_of_numeric r v variables typeEnvironment (fun x1 x2 -> x1 > x2)
    | GreaterEqual (r, v) ->
        dnf_of_numeric r v variables typeEnvironment (fun x1 x2 -> x1 >= x2)
    | Less (r, v) ->
        dnf_of_numeric r v variables typeEnvironment (fun x1 x2 -> x1 < x2)
    | LessEqual (r, v) ->
        dnf_of_numeric r v variables typeEnvironment (fun x1 x2 -> x1 <= x2)
    | True -> True
    | False -> False

and dnf_of_numeric r v variables typeEnvironment comparator =
    let convert x =
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
        if values = [] then False
        else dnf_of (In (r, values)) variables typeEnvironment
    in
    if Variable.mem r variables then (
        match v with
        | Int i   -> convert (float_of_int i)
        | Float f -> convert f
        | Ref r2  ->
            let conjunction r1 v1 r2 v2 = And [Eq (r1, v1); Eq (r2, v2)] in
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
                                  (conjunction r (Int v1) r2 (Float v2)) ::
                                      acc2
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
        | _       -> error 713 ""
    ) else
        dnf_of (nested_to_prevail r v variables typeEnvironment 5)
            variables typeEnvironment

(** convert equality to DNF, and convert a left-nested reference to prevail
    ones **)
and dnf_of_equal reference value variables typeEnvironment =
    if Variable.mem reference variables then
        Eq (reference, value)
    else
        dnf_of (nested_to_prevail reference value variables typeEnvironment 1)
            variables typeEnvironment

(** convert inequality to DNF, and convert a left-nested reference to
    prevail ones **)
and dnf_of_not_equal reference value variables typeEnvironment =
    if Variable.mem reference variables then
        let values =
            Array.fold_left (
                fun acc v ->
                    match v with
                    | Basic bv -> if bv = value then acc else bv :: acc
                    | _  -> error 714 "right-hand side is not a basic value"
                        (* the right-hand side is not a basic value *)
            ) [] (Variable.values_of reference variables)
        in
        if values = [] then False
        else dnf_of (In (reference, values)) variables typeEnvironment
    else
        dnf_of (nested_to_prevail reference value variables typeEnvironment 2)
            variables typeEnvironment

(** convert negation to DNF **)
and dnf_of_negation _constraint variables typeEnvironment =
    match _constraint with
    | True -> False
    | False -> True
    | Eq (r, v) -> dnf_of (Ne (r, v)) variables typeEnvironment
    | Ne (r, v) -> dnf_of (Eq (r, v)) variables typeEnvironment
    | Greater (r, v) -> dnf_of (LessEqual (r, v)) variables typeEnvironment
    | GreaterEqual (r, v) -> dnf_of (Less (r, v)) variables typeEnvironment
    | Less (r, v) -> dnf_of (GreaterEqual (r, v)) variables typeEnvironment
    | LessEqual (r, v) -> dnf_of (Greater (r, v)) variables typeEnvironment
    | Not c1 -> dnf_of c1 variables typeEnvironment
    | Imply (premise, conclusion) ->
        (* -(p -> q) = p ^ -q *)
        dnf_of (And [premise; (Not conclusion)]) variables typeEnvironment
    | And cs ->
        (* De Morgan's laws *)
        let cs1 = List.fold_left (fun acc c -> (Not c) :: acc) [] cs in
        dnf_of (Or cs1) variables typeEnvironment
    | Or cs  ->
        (* De Morgan's laws *)
        let cs1 = List.fold_left (fun acc c -> (Not c) :: acc) [] cs in
        dnf_of (And cs1) variables typeEnvironment
    | In (r, vector) ->
        if Variable.mem r variables then
            let cs = Array.fold_left (
                    fun acc v ->
                        match v with
                        | Basic v1 ->
                            if List.mem v1 vector then acc
                            else (Eq (r, v1)) :: acc
                        | _        -> error 715 ""
                ) [] (Variable.values_of r variables)
            in
            if cs = [] then False
            else dnf_of (Or cs) variables typeEnvironment
        else
            dnf_of (nested_to_prevail r (Vector vector) variables
                typeEnvironment 4) variables typeEnvironment

(** convert implication to DNF **)
and dnf_of_implication premise conclusion variables typeEnvironment =
    dnf_of (Or [(Not premise); conclusion]) variables typeEnvironment

(** convert membership constraint to DNF **)
and dnf_of_membership reference vector variables typeEnvironment =
    if Variable.mem reference variables then
        let clauses = Array.fold_left (
                fun acc value ->
                    match value with
                    | Basic v ->
                        if List.mem v vector then (Eq (reference, v)) :: acc
                        else acc
                    | _        -> error 716 ""
            ) [] (Variable.values_of reference variables)
        in
        if clauses = [] then False
        else dnf_of (Or clauses) variables typeEnvironment
    else
        dnf_of (nested_to_prevail reference (Vector vector) variables
            typeEnvironment 3) variables typeEnvironment

(** convert conjunction to DNF, performs cross-products when it has disjunction clause **)
and dnf_of_conjunction clauses variables typeEnvironment =
    let rec iter clauses ands ors =
        if clauses = [] then (false, ands, ors)
        else
            match dnf_of (List.hd clauses) variables typeEnvironment with
            | And css -> iter (List.tl clauses) (List.append css ands) ors
            | False   -> (true, ands, ors)
            | True    -> iter (List.tl clauses) ands ors
            | Or css  -> iter (List.tl clauses) ands ((Or css) :: ors)
            | css     -> iter (List.tl clauses) (css :: ands) ors
    in
    match clauses with
    | [] -> True
    | _  ->
        let (allFalse, andClauses, orClauses) = iter clauses [] [] in
        if allFalse then False
        else
            match andClauses, orClauses with
            | [], []         -> True
            | head :: [], [] -> head
            | [], head :: [] -> head
            | _, []          -> simplify_conjunction (And andClauses)
            | _, _           ->
                (cross_product_of andClauses orClauses) variables
                    typeEnvironment

(** convert disjunction to DNF **)
and dnf_of_disjunction clauses variables typeEnvironment =
    let rec iter clauses acc =
        if clauses = [] then (false, acc)
        else
            match dnf_of (List.hd clauses) variables typeEnvironment with
            | Or cs -> iter (List.tl clauses) (List.append cs acc)
            | False -> iter (List.tl clauses) acc
            | True  -> (true, acc)
            | c     -> iter (List.tl clauses) (c :: acc)
    in
    match clauses with
    | [] -> True
    | _ ->
        let (allTrue, clauses1) = iter clauses [] in
        if allTrue then True
        else
            match clauses1 with
            | clause :: [] -> clause
            | _            -> simplify_disjunction (Or clauses1)
;;

(**
 * substitute each parameter with a value as specified in the map of
 * parameters
 *)
let rec substitute_free_variables_of _constraint parameters =
    match _constraint with
    | Eq (r, v) ->
        let r1 = substitute_parameter_of_reference r parameters in
        let v1 = substitute_parameter_of_basic_value v parameters in
        Eq (r1, v1)
    | Ne (r, v) ->
        let r1 = substitute_parameter_of_reference r parameters in
        let v1 = substitute_parameter_of_basic_value v parameters in
        Ne (r1, v1)
    | Greater (r, v) ->
        let r1 = substitute_parameter_of_reference r parameters in
        let v1 = substitute_parameter_of_basic_value v parameters in
        Greater (r1, v1)
    | GreaterEqual (r, v) ->
        let r1 = substitute_parameter_of_reference r parameters in
        let v1 = substitute_parameter_of_basic_value v parameters in
        GreaterEqual (r1, v1)
    | Less (r, v) ->
        let r1 = substitute_parameter_of_reference r parameters in
        let v1 = substitute_parameter_of_basic_value v parameters in
        Less (r1, v1)
    | LessEqual (r, v) ->
        let r1 = substitute_parameter_of_reference r parameters in
        let v1 = substitute_parameter_of_basic_value v parameters in
        LessEqual (r1, v1)
    | Not c ->
        Not (substitute_free_variables_of c parameters)
    | Imply (c1, c2) ->
        Imply (
            substitute_free_variables_of c1 parameters,
            substitute_free_variables_of c2 parameters
        )
    | And cs ->
        And (
            List.fold_left (
                fun css c -> (substitute_free_variables_of c parameters) :: css
            ) [] cs
        )
    | Or cs ->
        Or (
            List.fold_left (
                fun css c -> (substitute_free_variables_of c parameters) :: css
            ) [] cs
        )
    | In (r, v) ->
        let r1 = substitute_parameter_of_reference r parameters in
        In (r1, v)
    | True -> True
    | False -> False
;;


(************************************************************************
 * Functions for global constraints.
 ************************************************************************)

type data = {
    complex   : _constraint list;
    simple    : _constraint list;
    variables : Variable.ts
}

(** compile simple membership of global constraints **)
let compile_membership isNegation reference vector data =
    let rec prevail_of r =
        if r = [] then error 718 "invalid nested reference"
        else if Variable.mem r data.variables then r
        else prevail_of (prefix r)
    in
    let prevail = prevail_of reference in
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
        let rs = reference @-- prevail in
        let accumulator = {
                complex   = data.complex;
                simple    = data.simple;
                variables = variables1
            }
        in
        let values = Variable.values_of prevail variables1 in
        Array.fold_left (fun acc v ->
            match v with
            | Basic (Ref r) ->
                let r1 = r @++ rs in
                let c =
                    if isNegation then
                        Imply (Eq (prevail, Ref r), Not (In (r1, vector)))
                    else
                        Imply (Eq (prevail, Ref r), In (r1, vector))
                in
                {
                    complex   = c :: acc.complex;
                    simple    = acc.simple;
                    variables = acc.variables
                }
            | _ -> error 719 "not a basic value"
        ) accumulator values
;;

(** compile simple equality of global constraints **)
let compile_equality isNegation reference value data =
    let rec prevail_of r =
        if r = [] then error 720 "invalid nested reference"
        else if Variable.mem r data.variables then r
        else prevail_of (prefix r)
    in
    let prevail = prevail_of reference in
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
        let rs = reference @-- prevail in
        let accumulator = {
                complex   = data.complex;
                simple    = data.simple;
                variables = variables1
            }
        in
        let values = Variable.values_of prevail variables1 in
        Array.fold_left (fun acc v ->
            match v with
            | Basic (Ref r) ->
                let r1 = r @++ rs in
                let _constraint =
                    if isNegation
                        then Imply (Eq (prevail, Ref r), Ne (r1, value))
                    else
                        Imply (Eq (prevail, Ref r), Eq (r1, value))
                in
                if not isNegation && (Variable.mem r1 acc.variables)
                    then {
                        complex = acc.complex;
                        simple = _constraint :: acc.simple;
                        variables = acc.variables
                    }
                else
                    {
                        complex = _constraint :: acc.complex;
                        simple = acc.simple;
                        variables = acc.variables
                    }
            | _ -> error 721 "invalid constraint"
        ) accumulator values
;;

(** compile simple equality and membership of global constraints **)
let compile_simple_constraints globalConstraints variables =
    match globalConstraints with
    | And clauses ->
        let accumulator = {
                complex   = [];
                simple    = [];
                variables = variables
            }
        in
        let data =
            List.fold_left (fun acc clause ->
                match clause with
                | Eq (r, v)         -> compile_equality false r v acc
                | Not (Eq (r, v))   -> compile_equality true r v acc
                | Ne (r, v)         -> compile_equality true r v acc
                | Not (Ne (r, v))   -> compile_equality false r v acc
                | In (r, vec)       -> compile_membership false r vec acc
                | Not (In (r, vec)) -> compile_membership true r vec acc
                | Imply (Eq (_, _), Eq (_, _)) -> {
                        complex   = acc.complex;
                        simple    = clause :: acc.simple;
                        variables = acc.variables
                    }
                | Imply (Eq (r, v), And clauses) -> (
                        let eq = Eq (r, v) in
                        List.fold_left (fun (acc: data) (c: _constraint) ->
                            match c with
                            | Eq (_, _) ->
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
    | _  -> (globalConstraints, [], variables)
;;

(**
 * This function performs:
 * 1. find the global constraints in given flat-store
 * 2. modify variables' domain based on 'membership' constraints
 * 3. separate simple implication clauses from the global constraints formula
 *    with others
 * 4. convert the other clauses (complex formula) into a DNF formula 
 *)
let global_of typeEnvironment flatStore variables =
    let referenceGlobal = ["global"] in
    if MapRef.mem referenceGlobal flatStore then
        match MapRef.find referenceGlobal flatStore with
        | Global globalConstraints ->
            let (globalConstraints1, globalImplications, variables1) =
                compile_simple_constraints globalConstraints variables
            in
            let dnfGlobalConstraints1 = dnf_of globalConstraints1
                variables typeEnvironment
            in
            (dnfGlobalConstraints1, globalImplications, variables1)
        | _ -> error 722 "not a global constraint"
    else (True, [], variables)
;;
