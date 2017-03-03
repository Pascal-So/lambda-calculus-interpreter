import Html exposing (Html, text)
import Html.Attributes
import Html.Events
import Set exposing (Set)


--------------------------- MAIN ------------------------------------------

main = 
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }
  -- text <| toString <| allTestsOk
  -- text <| toString <| testsOk testParsing


-- (\123.2(123))(\12.2)

type alias Model =
  { program : String
  , result : Maybe (List Term)
  }

type Msg = ProgramChanged String

noCmd : Model -> (Model, Cmd Msg)
noCmd m =
  (m, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ProgramChanged newProg ->
      Model newProg (runProgram newProg) |> noCmd

init : (Model, Cmd Msg)
init =
  Model "" Nothing |> noCmd


view : Model -> Html Msg
view model =
  Html.div [Html.Attributes.style [("text-align", "center")]]
    [ Html.textarea 
        [ Html.Attributes.cols 40
        , Html.Attributes.rows 3
        , Html.Attributes.value model.program
        , Html.Events.onInput ProgramChanged]
        []
    , case model.result of
        Nothing ->
          Html.h3[] [text "Could not parse program"]
        Just (terms) ->
          List.map (\s -> Html.h3 [] [text <| showTerm s]) terms 
            |> Html.div []
    ]



runProgram : String -> Maybe (List Term)
runProgram p =
  (parseTerm <<< pEOF) p
    |> List.head
    |> Maybe.map Tuple.first
    |> Maybe.map evaluateStepwise
  

------------- wrapper language transformer ---------------


isAlpha : Char -> Bool
isAlpha c =
  (Char.isUpper c) || (Char.isLower c)


isAlphanum : Char -> Bool
isAlphanum c =
  (isAlpha c) || (Char.isDigit c)

parseComment : Parser ()
parseComment =
  pChar '#' >>>
  pRestOfLine >>>
  return ()
  
parseVariableName : Parser String
parseVariableName =
  pSat isAlpha >>= \fst ->
  parserMany (pSat isAlphanum) >>= \tail ->
  return (String.fromList (fst::tail))
  
  
parseAssignment : Parser (String, Term)
parseAssignment =
  parseVariableName >>= \varname ->
  pWhitespace >>>
  pChar '=' >>>
  pWhitespace >>>
  pRestOfLine >>= \content ->
  return (varname, content)


------------- Lambda Calculus Parser -----------------------



parseApp : Parser Term
parseApp =
  let
    appTerm = (parseVar || parens parseTerm) <<< pWhitespace
  in
    appTerm >>= \func ->
    parserMany1 appTerm >>= \args ->
    return (multiApp func args)

parseLambda : Parser Term
parseLambda str =
  (pChar '\\' >>>
  pWhitespace >>>
  parserMany1 (parseVarName <<< pWhitespace) >>= \bound ->
  pChar '.' >>>
  pWhitespace >>>
  parseTerm >>= \t ->
  return (multiVarLambda bound t)) str

parseSingleCharVar : Parser VarName
parseSingleCharVar =
  pSat isAlpha >>= \char ->
  return (String.fromChar char)

parseLongVarName : Parser VarName
parseLongVarName =
  pChar '`' >>>
  pSat isAlpha >>= \fst ->
  parserMany (pSat isAlphanum) >>= \rest ->
  pChar '`' >>>
  return (String.fromList (fst::rest))

parseVarName : Parser VarName
parseVarName = 
  parseSingleCharVar ||| parseLongVarName

parseVar : Parser Term
parseVar = parserMap Var parseVarName


-- need to pass str argument explicitly because of eager evaluation
parseTerm : Parser Term
parseTerm str = 
  let
    pureTerms = parseApp || parseLambda || parseVar
  in
    (pureTerms || parens pureTerms) str


extractParsedWithDefault : Term -> Parsed Term -> Term
extractParsedWithDefault t p =
  List.head p
    |> Maybe.map Tuple.first
    |> Maybe.withDefault t

extractParsed : Parsed Term -> Term
extractParsed = extractParsedWithDefault (Var "parserError")

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
  [ ( substitute "x" (Var "y") (Var "x")
    , Var "y"
    )
    
  , ( substitute "x" (Var "y") (Lambda "x" (Var "x"))
    , Lambda "x" (Var "x")
    )
    
  , ( substitute "y" (Var "z") (Lambda "x" (Var "y"))
    , Lambda "x" (Var "z")
    )
    
  , ( substitute "y" (Var "x") (Lambda "x" (Var "y"))
    , Lambda "1" (Var "x")
    )
    
  , ( substitute "x" (Lambda "x" (App (Var "x") (Var "x"))) (App (Var "x") (Var "x"))
    , App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x")))
    )
  ]


testsEvaluation =
  [ ( evaluationStep (App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x"))))
    , App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x")))
    )
    
  , ( evaluationStep (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x")))
    , Lambda "y" (Lambda "x" (Var "x"))
    )
    
  , ( evaluationStep (App (Lambda "x" (Lambda "y" (Var "y"))) (Var "y"))
    , Lambda "1" (Var "1")
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


testParsing =
  [ ( parseVar "x" |> extractParsed
    , Var "x"
    )
    
  , ( parseApp "xy" |> extractParsed
    , App (Var "x") (Var "y")
    )
    
  , ( parseLambda "\\x.x" |> extractParsed
    , Lambda "x" (Var "x")
    )
    
  , ( parseLambda "\\xy.x" |> extractParsed
    , Lambda "x" (Lambda "y" (Var "x"))
    )
    
  , ( parseLambda "\\xy.xy" |> extractParsed
    , Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))
    )
    
  , ( parseLambda "\\xyz.x" |> extractParsed
    , Lambda "x" (Lambda "y" (Lambda "z" (Var "x")))
    )
    
  , ( parseTerm "(\\x.x)y" |> extractParsed
    , App (Lambda "x" (Var "x")) (Var "y")
    )
    
  , ( parseTerm "(\\x.x)yz" |> extractParsed
    , App (App (Lambda "x" (Var "x")) (Var "y")) (Var "z")
    )
  ]


testsOk : List (a, a) -> Bool
testsOk = List.all (uncurry (==))

testsList = [testsSubstitute, testsEvaluation, testsNums, testBools, testParsing]

allTestsOk : Bool
allTestsOk =
  List.all testsOk testsList


showFailedTests : String
showFailedTests = 
  testsList
    |> List.map (List.filter (uncurry (/=)))
    |> toString






------------------------------- LAMBDA CALCULUS ENGINE ----------------------

type alias VarName = String

type Term = Var VarName | Lambda VarName Term | App Term Term

showTerm : Term -> String
showTerm t =
  case t of
    Var a -> 
      if String.length a == 1 then
        a
      else
        "`" ++ a ++ "`"
    Lambda a t ->
      "\\" ++ a ++ "." ++ showTerm t
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
            App _ _ ->
              "(" ++ showTerm t2 ++ ")"
            _ ->
              showTerm t2
      in
        showLeft ++ " " ++ showRight


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

-- convert a list of bound vars and a contained term to multiple nested lambdas
multiVarLambda : List VarName -> Term -> Term
multiVarLambda =
  flip (List.foldr Lambda)
  
  
-- convert application of multi arg function to multiple applications
multiApp : Term -> List Term -> Term
multiApp = List.foldl (flip App)









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