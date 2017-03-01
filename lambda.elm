import Html exposing (Html, text)
import Set exposing (Set)


--------------------------- MAIN ------------------------------------------

main =
  text <| toString <| allTestsOk
  --text <| showTerm <| evaluate <| App succ <| App succ (churchNum 0)
  --text <| showTerm <| evaluate (App omega omega)
  
  
  
  
  
  

---------------------- some useful terms ---------------------------------

-- infinite loop
omega : Term
omega = App (Lambda 1 (App (Var 1) (Var 1))) (Lambda 1 (App (Var 1) (Var 1)))

-- get the church numeral representation for a given non-negative integer
churchNum : Int -> Term
churchNum n =
  let
    fList = List.repeat n (App (Var 2))
    app = List.foldr (<<) id fList
  in
    Lambda 2 (Lambda 1 (app (Var 1)))

plus : Term
plus = Lambda 4 (Lambda 3 (Lambda 2 (Lambda 1 (App (App (Var 4) (Var 2)) (App (App (Var 3) (Var 2)) (Var 1))))))


-- successor church numeral
succ : Term
succ = evaluate <| App plus (churchNum 1)


-- bool stuff

true : Term
true = Lambda 1 (Lambda 2 (Var 1))

false : Term
false = Lambda 1 (Lambda 2 (Var 2))

isZero : Term
isZero = Lambda 1 (App (App (Var 1) (App true false)) true)

not : Term
not = Lambda 1 (App (App (Var 1) false) true)



-- fixed point combinator
y : Term
y = Lambda 2 (App (Lambda 1 (App (Var 2) (App (Var 1) (Var 1)))) (Lambda 1 (App (Var 2) (App (Var 1) (Var 1)))))



--------------------------- TESTS --------------------------------------

testsSubstitute = 
  [ ( substitute 1 (Var 2) (Var 1)
    , Var 2
    )
    
  , ( substitute 1 (Var 2) (Lambda 1 (Var 1))
    , Lambda 1 (Var 1)
    )
    
  , ( substitute 2 (Var 3) (Lambda 1 (Var 2))
    , Lambda 1 (Var 3)
    )
    
  , ( substitute 2 (Var 1) (Lambda 1 (Var 2))
    , Lambda 3 (Var 1)
    )
    
  , ( substitute 1 (Lambda 1 (App (Var 1) (Var 1))) (App (Var 1) (Var 1))
    , App (Lambda 1 (App (Var 1) (Var 1))) (Lambda 1 (App (Var 1) (Var 1)))
    )
  ]


testsEvaluation =
  [ ( evaluationStep (App (Lambda 1 (App (Var 1) (Var 1))) (Lambda 1 (App (Var 1) (Var 1))))
    , App (Lambda 1 (App (Var 1) (Var 1))) (Lambda 1 (App (Var 1) (Var 1)))
    )
    
  , ( evaluationStep (App (Lambda 1 (Lambda 2 (Var 1))) (Lambda 1 (Var 1)))
    , Lambda 2 (Lambda 1 (Var 1))
    )
  ]
  
testsNums =
  [ ( evaluate <| App succ (churchNum 0)
    , churchNum 1
    )
  
  , ( evaluate <| App succ (churchNum 1)
    , churchNum 2
    )
    
  , ( evaluate <| App (App plus (churchNum 2)) (churchNum 3)
    , churchNum 5
    )
    
  , ( evaluate <| App (App plus (churchNum 2)) (churchNum 0)
    , churchNum 2
    )
    
  , ( evaluate <| App (App plus (churchNum 4)) (churchNum 3)
    , evaluate <| App (App plus (churchNum 3)) (churchNum 4)
    )
    
  , ( evaluate <| App (App plus (churchNum 18)) (churchNum 9)
    , evaluate <| App (App plus (churchNum 7)) (churchNum 20)
    )
  ]
  
testBools =
  [ ( evaluate <| App isZero (churchNum 0)
    , true
    )
    
  , ( evaluate <| App isZero (churchNum 1)
    , false
    )
    
  , ( evaluate <| App not true
    , false
    )
    
  , ( evaluate <| App not false
    , true
    )
  ]


testsOk : List (a, a) -> Bool
testsOk = List.all (uncurry (==))


allTestsOk : Bool
allTestsOk =
  let
    testsList = [testsSubstitute, testsEvaluation, testsNums, testBools]
  in
    List.all testsOk testsList







------------------------------- LAMBDA CALCULUS ENGINE ----------------------

type alias VarName = Int

type Term = Var VarName | Lambda VarName Term | App Term Term

showTerm : Term -> String
showTerm t =
  case t of
    Var a -> 
      toString a
    Lambda a t ->
      "\\" ++ toString a ++ "." ++ showTerm t
    App t1 t2 ->
      "(" ++ showTerm t1 ++ ")(" ++ showTerm t2 ++ ")"


-- first variable name that is not contained in the given set
findUnusedVar : Set VarName -> VarName
findUnusedVar s =
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
            renamed = renameFreeVar y newName e
          in
            Lambda newName (substitute x r renamed)
        else
          Lambda y (substitute x r e)

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









------- why is this stuff not in core? --------------
  
zip : List a -> List b -> List (a,b)
zip = List.map2 (\a b -> (a,b))

uncurry : (a -> b -> c) -> (a,b) -> c
uncurry f pair = f (Tuple.first pair) (Tuple.second pair)

id : a -> a
id a = a





printLn : String -> Html a
printLn txt =
  Html.p [] [text txt]