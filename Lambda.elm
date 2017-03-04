module Lambda exposing
    ( Term (..)
    , VarName
    , showTerm
    , evaluate
    , evaluateStepwise
    , multiVarLambda
    , multiApp
    , getFreeVars
    )

import Set exposing (Set)

----------------- Data Types ----------------------


type alias VarName = String

type Term = Var VarName | Lambda VarName Term | App Term Term

showVarName : VarName -> String
showVarName name =
    if String.length name == 1 then
        name
    else
        "`" ++ name ++ "`"

showTerm : Term -> String
showTerm t =
    case t of
        Var a ->
            showVarName a
        Lambda a t ->
            "\\" ++ showVarName a ++ "." ++ showTerm t
        App t1 t2 ->
            let
                showLeft =
                    case t1 of
                        Lambda _ _ ->
                            "(" ++ showTerm t1 ++ ")"
                        _ ->
                            showTerm t1
                showRight =
                    case t2 of
                        Var _ ->
                            showTerm t2
                        _ ->
                            "(" ++ showTerm t2 ++ ")"
                    in
                        showLeft ++ " " ++ showRight



--------------------------- Evaluation ----------------------------------


-- normal order beta reduction until nothing changes anymore, return list
-- of intermediate terms
evaluateStepwise : Term -> List Term
evaluateStepwise t =
    let
        ev = evaluationStep t
    in
        if ev == t then
            [t]
        else
            t :: (evaluateStepwise ev)

-- normal order beta reduction until nothing changes anymore
evaluate : Term -> Term
evaluate t =
    let
        ev = evaluationStep t
    in
        if ev == t then
            ev
        else
            evaluate ev

-- normal order beta reduction
evaluationStep : Term -> Term
evaluationStep t =
    case t of
        Var x ->
            Var x
        Lambda x e ->
            Lambda x (evaluationStep e)
        App (Lambda x e) r ->
            substitute x r e
        App e1 e2 ->
            let
                ev1 = evaluationStep e1
            in
                if ev1 == e1 then
                    App e1 (evaluationStep e2)
                else
                    App ev1 e2


------------------- Constructor Shortcuts --------------------------------

-- convert a list of bound vars and a contained term to multiple nested lambdas
multiVarLambda : List VarName -> Term -> Term
multiVarLambda =
    flip (List.foldr Lambda)


-- convert application of multi arg function to multiple applications
multiApp : Term -> List Term -> Term
multiApp = List.foldl (flip App)





----------------- Implementation Details ---------------------------


getInts : Set String -> Set Int
getInts set =
    Set.toList set
    |> List.map (String.toInt >> Result.toMaybe)
    |> List.filterMap (\x->x)
    |> Set.fromList

-- first variable name that is not contained in the given set
findUnusedVar : Set VarName -> VarName
findUnusedVar set =
    let
        ints = getInts set
        free = findUnusedInt ints
    in
        toString free

findUnusedInt : Set Int -> Int
findUnusedInt s =
    let
        find s x =
            if Set.member x s then
                find s (x+1)
            else
                x
    in
        find s 1

getFreeVars : Term -> Set VarName
getFreeVars t =
    case t of
        Var x ->
            Set.singleton x
        Lambda x t ->
            let
                ft = getFreeVars t
            in
                Set.diff ft (Set.singleton x)
        App t1 t2 ->
            Set.union (getFreeVars t1) (getFreeVars t2)

renameFreeVar : VarName -> VarName -> Term -> Term
renameFreeVar x y t =
    case t of
        Var k ->
            if k == x then
                Var y
            else
                Var k
        App e1 e2 ->
            App (renameFreeVar x y e1) (renameFreeVar x y e2)
        Lambda k e ->
            if k == x then
                Lambda k e
            else
                Lambda k (renameFreeVar x y e)

substitute : VarName -> Term -> Term -> Term
substitute x r t =
    case t of
        Var y ->
            if x == y then
                r
            else
                Var y
        App e1 e2 ->
            App (substitute x r e1) (substitute x r e2)
        Lambda y e ->
            if x == y then
                Lambda y e
            else
                if Set.member y (getFreeVars r) then
                    let
                        disallowed = Set.union (getFreeVars e) (getFreeVars r)
                        newName = findUnusedVar disallowed
                        renamedBody = renameFreeVar y newName e
                    in
                        substitute x r (Lambda newName renamedBody)
                else
                    Lambda y (substitute x r e)
