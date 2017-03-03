import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events
import Set exposing (Set)
import Char
import Dict exposing (Dict)

import Debug exposing (log)

import Parser exposing ((<*), (*>), (+++), (>>=), Parser)


--------------------------- MAIN ------------------------------------------

main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
        -- text <| toString <| allTestsOk
        -- text <| showFailedTests
        -- text <| toString <| testsOk testParsing


-- (\123.2(123))(\12.2)

type alias Model =
    { program : String
    , result : Result String (List (List Term))
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
    Model "" (Err "Empty program") |> noCmd


lightGrey : Html.Attribute a
lightGrey = style [("color", "#777")]

darkGrey : Html.Attribute a
darkGrey = style [("color", "#222")]

ma : Int -> Html.Attribute a
ma x = style [("margin", toString x ++ "px")]

mb : Int -> Html.Attribute a
mb x = style [("margin-bottom", toString x ++ "px")]

viewStepwise : List Term -> List (Html Msg)
viewStepwise terms =
    case terms of
        []      -> []
        [x]     -> [Html.h3 [darkGrey, ma 5] [text <| showTerm x]]
        (x::xs) -> Html.h3 [lightGrey, ma 5] [text <| showTerm x] :: viewStepwise xs


view : Model -> Html Msg
view model =
    Html.div [Html.Attributes.style [("text-align", "center")]]
        [ Html.textarea
            [ Html.Attributes.cols 60
            , Html.Attributes.rows 7
            , Html.Attributes.value model.program
            , Html.Events.onInput ProgramChanged]
            []
        , case model.result of
            Err error ->
                Html.h3[] [text error]
            Ok termChains ->
                List.map viewStepwise termChains
                |> List.map (\x -> Html.div [mb 30] x)
                |> Html.div []
        ]



runProgram : String -> Result String (List (List Term))
runProgram str =
    (parseProgram <* Parser.eof) str
    |> log "parse"
    |> List.head
    |> Maybe.map Tuple.first
    |> Result.fromMaybe "Could not parse program"
    |> Result.andThen (\program ->
         if List.isEmpty program then
             Err "Empty program"
         else
             Ok program
       )
    |> Result.andThen (\program ->
         if hasDuplicateVars program then
             Err "Some variables have been assigned twice"
         else
             Ok program
       )
    |> Result.map (List.map evaluateStepwise << programToExpressions)

------------- Wrapper Language Transformer ---------------

getAssignmentsDict : List ProgramLine -> Dict VarName Term
getAssignmentsDict lines =
    lines
    |> List.filterMap (\line ->
         case line of
            Assignment a b -> Just (a, b)
            Expression _   -> Nothing
       )
    |> Dict.fromList


getExpressionTerms : List ProgramLine -> List Term
getExpressionTerms lines =
    lines
    |> List.filterMap (\line ->
         case line of
             Expression term -> Just term
             Assignment _ _  -> Nothing
       )


wrapInLambdas : Dict VarName Term -> Term -> Term
wrapInLambdas dict term =
    let
        freeVars = getFreeVars term
        termsToAssign = dict
        |> Dict.filter (\var _ ->
            Set.member var freeVars
           )
        |> Dict.toList
    in
        termsToAssign
        |> List.foldr (\(name, val) accTerm ->
            App (Lambda name accTerm) val
         ) term


-- only run on valid program (no duplicate variable assignments),
-- otherwise duplicate assignments will be overridden
programToExpressions : List ProgramLine -> List Term
programToExpressions lines =
    let
        assignments = getAssignmentsDict lines
        terms = getExpressionTerms lines
    in
        List.map (wrapInLambdas assignments) terms


hasDuplicateVars : List ProgramLine -> Bool
hasDuplicateVars prog =
    let
        vars =
            prog
            |> List.filterMap (\line ->
                case line of
                    Assignment var _ -> Just var
                    Expression _     -> Nothing
               )
        len = List.length vars
        uniqueLen = Set.size (Set.fromList vars)
    in
        len /= uniqueLen



------------ Wrapper Language Parser ----------------------


type ProgramLine = Assignment VarName Term | Expression Term


isAlpha : Char -> Bool
isAlpha c =
    (Char.isUpper c) || (Char.isLower c)


isAlphanum : Char -> Bool
isAlphanum c =
    (isAlpha c) || (Char.isDigit c)

parseComment : Parser ()
parseComment =
    Parser.char '#' *>
    pRestOfLine *>
    Parser.return ()


parseAssignment : Parser ProgramLine
parseAssignment =
    parseVarName >>= \varname ->
    Parser.whitespace *>
    Parser.char '=' *>
    Parser.whitespace *>
    Parser.inRestOfLine parseTerm >>= \term ->
    Parser.return (Assignment varname term)

parseExpression : Parser ProgramLine
parseExpression =
    parseTerm >>= \term ->
    Parser.return (Expression term)


parseProgram : Parser (List ProgramLine)
parseProgram =
    Parser.many
        ( Parser.whitespace *>
          (parserInRestOfLine (parseExpression <* Parser.whitespace) +++ parseAssignment)
          <* Parser.whitespace) >>= \lines ->
    Parser.return lines



------------- Lambda Calculus Parser -----------------------



parseApp : Parser Term
parseApp =
    let
        appTerm = (parseVar || Parser.parens parseTerm) <* Parser.whitespace
    in
        appTerm >>= \func ->
        Parser.many1 appTerm >>= \args ->
        Parser.return (multiApp func args)

parseLambda : Parser Term
parseLambda str =
    (Parser.char '\\' *>
    Parser.whitespace *>
    Parser.many1 (parseVarName <* Parser.whitespace) >>= \bound ->
    Parser.char '.' *>
    Parser.whitespace *>
    parseTerm >>= \t ->
    Parser.return (multiVarLambda bound t)) str

parseSingleCharVar : Parser VarName
parseSingleCharVar =
    pSat isAlphanum >>= \char ->
    Parser.return (String.fromChar char)

parseLongVarName : Parser VarName
parseLongVarName =
    Parser.char '`' *>
    Parser.many (pSat isAlphanum) >>= \name ->
    Parser.char '`' *>
    Parser.return (String.fromList name)

parseVarName : Parser VarName
parseVarName =
    parseSingleCharVar +++ parseLongVarName

parseVar : Parser Term
parseVar = Parser.map Var parseVarName


-- need to pass str argument explicitly because of eager evaluation
parseTerm : Parser Term
parseTerm str =
    let
        pureTerms = parseApp || parseLambda || parseVar
    in
        (pureTerms || Parser.parens pureTerms) str


extractParsedWithDefault : Term -> Parser.Parsed Term -> Term
extractParsedWithDefault t p =
    List.head p
    |> Maybe.map Tuple.first
    |> Maybe.withDefault t

extractParsed : Parser.Parsed Term -> Term
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
    [   ( substitute "x" (Var "y") (Var "x")
        , Var "y"
        )

    ,   ( substitute "x" (Var "y") (Lambda "x" (Var "x"))
        , Lambda "x" (Var "x")
        )

    ,   ( substitute "y" (Var "z") (Lambda "x" (Var "y"))
        , Lambda "x" (Var "z")
        )

    ,   ( substitute "y" (Var "x") (Lambda "x" (Var "y"))
        , Lambda "1" (Var "x")
        )

    ,   ( substitute "x" (Lambda "x" (App (Var "x") (Var "x"))) (App (Var "x") (Var "x"))
        , App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x")))
        )
    ]


testsEvaluation =
    [   ( evaluationStep (App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x"))))
        , App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x")))
        )

    ,   ( evaluationStep (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x")))
        , Lambda "y" (Lambda "x" (Var "x"))
        )

    ,   ( evaluationStep (App (Lambda "x" (Lambda "y" (Var "y"))) (Var "y"))
        , Lambda "1" (Var "1")
        )
    ]

testsNums =
    [   ( evaluate <| App succ (churchNum 0)
        , churchNum 1
        )

    ,   ( evaluate <| App succ (churchNum 1)
        , churchNum 2
        )

    ,   ( evaluate <| App (App plus (churchNum 2)) (churchNum 3)
        , churchNum 5
        )

    ,   ( evaluate <| App (App plus (churchNum 2)) (churchNum 0)
        , churchNum 2
        )

    ,   ( evaluate <| App (App plus (churchNum 4)) (churchNum 3)
        , evaluate <| App (App plus (churchNum 3)) (churchNum 4)
        )

    ,   ( evaluate <| App (App plus (churchNum 18)) (churchNum 9)
        , evaluate <| App (App plus (churchNum 7)) (churchNum 20)
        )
    ]

testBools =
    [   ( evaluate <| App isZero (churchNum 0)
        , true
        )

    ,   ( evaluate <| App isZero (churchNum 1)
        , false
        )

    ,   ( evaluate <| App not true
        , false
        )

    ,   ( evaluate <| App not false
        , true
        )
    ]


testParsing =
    [   ( parseVar "x" |> extractParsed
        , Var "x"
        )

    ,   ( parseApp "xy" |> extractParsed
        , App (Var "x") (Var "y")
        )

    ,   ( parseLambda "\\x.x" |> extractParsed
        , Lambda "x" (Var "x")
        )

    ,   ( parseLambda "\\xy.x" |> extractParsed
        , Lambda "x" (Lambda "y" (Var "x"))
        )

    ,   ( parseLambda "\\xy.xy" |> extractParsed
        , Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))
        )

    ,   ( parseLambda "\\xyz.x" |> extractParsed
        , Lambda "x" (Lambda "y" (Lambda "z" (Var "x")))
        )

    ,   ( parseTerm "(\\x.x)y" |> extractParsed
        , App (Lambda "x" (Var "x")) (Var "y")
        )

    ,   ( parseTerm "(\\x.x)yz" |> extractParsed
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
